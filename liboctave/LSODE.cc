// ODE.cc                                                -*- C++ -*-
/*

Copyright (C) 1992, 1993 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, write to the Free
Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

*/

#ifdef __GNUG__
#pragma implementation
#endif

#include <iostream.h>
#include "ODE.h"
#include "f77-uscore.h"

extern "C"
{
  int F77_FCN (lsode) (int (*)(), int *, double *, double *, double *,
		       int *, double *, double *, int *, int *, int *,
		       double *, int *, int *, int *, int (*)(), int *);
}

static ColumnVector (*user_fun) (ColumnVector&, double);
static Matrix       (*user_jac) (ColumnVector&, double);
static ColumnVector *tmp_x;

ODE::ODE (void)
{
  n = 0;
  t = 0.0;

  absolute_tolerance = 1.0e-6;
  relative_tolerance = 1.0e-6;

  stop_time_set = 0;
  stop_time = 0.0;

  restart = 1;

  istate = 1;
  itol = 1;
  itask = 1;
  iopt = 0;

  liw = 20 + n;
  lrw = 22 + n * (9 + n);

  iwork = new int [liw];
  rwork = new double [lrw];
  for (int i = 4; i < 9; i++)
    {
      iwork[i] = 0;
      rwork[i] = 0.0;
    }

  fun = NULL;
  jac = NULL;
}

ODE::ODE (int size)
{
  n = size;
  t = 0.0;

  absolute_tolerance = 1.0e-6;
  relative_tolerance = 1.0e-6;

  stop_time_set = 0;
  stop_time = 0.0;

  restart = 1;

  istate = 1;
  itol = 1;
  itask = 1;
  iopt = 0;

  liw = 20 + n;
  lrw = 22 + n * (9 + n);

  iwork = new int [liw];
  rwork = new double [lrw];
  for (int i = 4; i < 9; i++)
    {
      iwork[i] = 0;
      rwork[i] = 0.0;
    }

  fun = NULL;
  jac = NULL;
}

ODE::ODE (const ColumnVector& state, double time, const ODEFunc& f)
{
  n = state.capacity ();
  t = time;
  x = state;

  absolute_tolerance = 1.0e-6;
  relative_tolerance = 1.0e-6;

  stop_time_set = 0;
  stop_time = 0.0;

  restart = 1;

  istate = 1;
  itol = 1;
  itask = 1;
  iopt = 0;

  liw = 20 + n;
  lrw = 22 + n * (9 + n);

  iwork = new int [liw];
  rwork = new double [lrw];
  for (int i = 4; i < 9; i++)
    {
      iwork[i] = 0;
      rwork[i] = 0.0;
    }

  fun = f.function ();
  jac = f.jacobian_function ();
}

ODE::~ODE (void)
{
  delete [] rwork;
  delete [] iwork;
}

int
lsode_f (int *neq, double *time, double *state, double *deriv)
{
  int nn = *neq;
  ColumnVector tmp_deriv (nn);

  /*
   * NOTE: this won't work if LSODE passes copies of the state vector.
   *       In that case we have to create a temporary vector object
   *       and copy.
   */
  tmp_deriv = (*user_fun) (*tmp_x, *time);

  for (int i = 0; i < nn; i++)
    deriv [i] = tmp_deriv.elem (i);

  return 0;
}

int
lsode_j (int *neq, double *time, double *state, int *ml, int *mu,
         double *pd, int *nrowpd)
{
  int nn = *neq;
  Matrix tmp_jac (nn, nn);

  /*
   * NOTE: this won't work if LSODE passes copies of the state vector.
   *       In that case we have to create a temporary vector object
   *       and copy.
   */
  tmp_jac = (*user_jac) (*tmp_x, *time);

  for (int j = 0; j < nn; j++)
    for (int i = 0; i < nn; i++)
      pd [*nrowpd * j + i] = tmp_jac (i, j);

  return 0;
}

ColumnVector
ODE::integrate (double tout)
{
  if (jac == NULL)
    method_flag = 22;
  else
    method_flag = 21;

  double *xp = x.fortran_vec ();

// NOTE: this won't work if LSODE passes copies of the state vector.
//       In that case we have to create a temporary vector object
//       and copy.

  tmp_x = &x;
  user_fun = fun;
  user_jac = jac;

// Try 5000 steps before giving up.

  iwork[5] = 5000;
  int working_too_hard = 0;

  if (stop_time_set)
    {
      iopt = 1;
      itask = 4;
      rwork [0] = stop_time;
    }
  else
    {
      iopt = 0;
      itask = 1;
    }

  if (restart)
    {
      restart = 0;
      istate = 1;
    }

 again:

  (void) F77_FCN (lsode) (lsode_f, &n, xp, &t, &tout, &itol,
			  &relative_tolerance, &absolute_tolerance,
			  &itask, &istate, &iopt, rwork, &lrw, iwork,
			  &liw, lsode_j, &method_flag);

  switch (istate)
    {
    case -6: // error weight became zero during problem. (solution
	     // component i vanished, and atol or atol(i) = 0.)
      break;
    case -5: // repeated convergence failures (perhaps bad jacobian
	     // supplied or wrong choice of mf or tolerances).
      break;
    case -4: // repeated error test failures (check all inputs).
      break;
    case -3: // illegal input detected (see printed message).
      break;
    case -2: // excess accuracy requested (tolerances too small).
      break;
    case -1: // excess work done on this call (perhaps wrong mf).
      working_too_hard++;
      if (working_too_hard > 20)
	{
	  cerr << "Shut 'er down Slim!  She's a suckin' mud!\n";
	  exit (1);
	}
      else
	{
	  istate = 2;
	  goto again;
	}
      break;
    case 2: // lsode was successful
      break;
    default:
      // Error?
      break;
    }

  t = tout;

  return x;
}

void
ODE::integrate (int nsteps, double tstep, ostream& s)
{
  int time_to_quit = 0;
  double tout = t;

  s << t << " " << x << "\n";

  for (int i = 0; i < nsteps; i++)
    {
      tout += tstep;
      if (stop_time_set && tout > stop_time)
	{
	  tout = stop_time;
	  time_to_quit = 1;
	}

      x = integrate (tout);

      s << t << " " << x << "\n";

      if (time_to_quit)
	return;
    }
}

Matrix
ODE::integrate (const ColumnVector& tout)
{
  Matrix retval;
  int n_out = tout.capacity ();

  if (n_out > 0 && n > 0)
    {
      retval.resize (n_out, n);

      for (int i = 0; i < n; i++)
	retval.elem (0, i) = x.elem (i);

      for (int j = 1; j < n_out; j++)
	{
	  ColumnVector x_next = integrate (tout.elem (j));
	  for (i = 0; i < n; i++)
	    retval.elem (j, i) = x_next.elem (i);
	}
    }

  return retval;
}

Matrix
ODE::integrate (const ColumnVector& tout, const ColumnVector& tcrit)
{
  Matrix retval;
  int n_out = tout.capacity ();

  if (n_out > 0 && n > 0)
    {
      retval.resize (n_out, n);

      for (int i = 0; i < n; i++)
	retval.elem (0, i) = x.elem (i);

      int n_crit = tcrit.capacity ();

      if (n_crit > 0)
	{
	  int i_crit = 0;
	  int i_out = 1;
	  double next_crit = tcrit.elem (0);
	  double next_out;
	  while (i_out < n_out)
	    {
	      int do_restart = 0;

	      next_out = tout.elem (i_out);
	      if (i_crit < n_crit)
		next_crit = tcrit.elem (i_crit);

	      int save_output;
	      double t_out;

	      if (next_crit == next_out)
		{
		  set_stop_time (next_crit);
		  t_out = next_out;
		  save_output = 1;
		  i_out++;
		  i_crit++;
		  do_restart = 1;
		}
	      else if (next_crit < next_out)
		{
		  if (i_crit < n_crit)
		    {
		      set_stop_time (next_crit);
		      t_out = next_crit;
		      save_output = 0;
		      i_crit++;
		      do_restart = 1;
		    }
		  else
		    {
		      clear_stop_time ();
		      t_out = next_out;
		      save_output = 1;
		      i_out++;
		    }
		}
	      else
		{
		  set_stop_time (next_crit);
		  t_out = next_out;
		  save_output = 1;
		  i_out++;
		}

	      ColumnVector x_next = integrate (t_out);

	      if (save_output)
		{
		  for (i = 0; i < n; i++)
		    retval.elem (i_out-1, i) = x_next.elem (i);
		}

	      if (do_restart)
		force_restart ();
	    }
	}
      else
	retval = integrate (tout);
    }

  return retval;
}

int
ODE::size (void) const
{
  return n;
}

ColumnVector
ODE::state (void) const
{
  return x;
}

double ODE::time (void) const
{
  return t;
}

void
ODE::force_restart (void)
{
  restart = 1;
}

void
ODE::initialize (const ColumnVector& state, double time)
{
  restart = 1;
  x = state;
  t = time;
}

void
ODE::set_stop_time (double time)
{
  stop_time_set = 1;
  stop_time = time;
}

void
ODE::clear_stop_time (void)
{
  stop_time_set = 0;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
