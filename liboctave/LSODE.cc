// LSODE.cc                                                -*- C++ -*-
/*

Copyright (C) 1992, 1993, 1994, 1995 John W. Eaton

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
Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*/

#if defined (__GNUG__)
#pragma implementation
#endif

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cfloat>
#include <cmath>

#include <iostream.h>

#include "LSODE.h"
#include "f77-uscore.h"
#include "lo-error.h"

extern "C"
{
  int F77_FCN (lsode, LSODE) (int (*)(const int&, const double&,
				      double*, double*, int&),
			      int&, double*, double&, double&, int&,
			      double&, double&, int&, int&, int&,
			      double*, int&, int*, int&,
			      int (*)(const int&, const double&,
				      double*, const int&, const int&,
				      double*, const int&),
			      int&);
}

static ODEFunc::ODERHSFunc user_fun;
static ODEFunc::ODEJacFunc user_jac;
static ColumnVector *tmp_x;

LSODE::LSODE (void) : ODE (), LSODE_options ()
{
  n = size ();

  stop_time_set = 0;
  stop_time = 0.0;

  integration_error = 0;
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
}

LSODE::LSODE (const ColumnVector& state, double time, const ODEFunc& f)
  : ODE (state, time, f), LSODE_options ()
{
  n = size ();

  stop_time_set = 0;
  stop_time = 0.0;

  integration_error = 0;
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
}

LSODE::~LSODE (void)
{
  delete [] rwork;
  delete [] iwork;
}

void
LSODE::force_restart (void)
{
  restart = 1;
}

void
LSODE::set_stop_time (double time)
{
  stop_time_set = 1;
  stop_time = time;
}

void
LSODE::clear_stop_time (void)
{
  stop_time_set = 0;
}

int
lsode_f (const int& neq, const double& time, double *,
	 double *deriv, int& ierr) 
{
  ColumnVector tmp_deriv (neq);

  // NOTE: this won't work if LSODE passes copies of the state vector.
  //       In that case we have to create a temporary vector object
  //       and copy.

  tmp_deriv = (*user_fun) (*tmp_x, time);

  if (tmp_deriv.length () == 0)
    ierr = -1;
  else
    {
      for (int i = 0; i < neq; i++)
	deriv [i] = tmp_deriv.elem (i);
    }

  return 0;
}

int
lsode_j (const int& neq, const double& time, double *,
	 const int&, const int&, double *pd, const int& nrowpd)
{
  Matrix tmp_jac (neq, neq);

  // NOTE: this won't work if LSODE passes copies of the state vector.
  //       In that case we have to create a temporary vector object
  //       and copy.

  tmp_jac = (*user_jac) (*tmp_x, time);

  for (int j = 0; j < neq; j++)
    for (int i = 0; i < neq; i++)
      pd [nrowpd * j + i] = tmp_jac (i, j);

  return 0;
}

ColumnVector
LSODE::do_integrate (double tout)
{
  if (jac)
    method_flag = 21;
  else
    method_flag = 22;

  integration_error = 0;

  double *xp = x.fortran_vec ();

  // NOTE: this won't work if LSODE passes copies of the state vector.
  //       In that case we have to create a temporary vector object
  //       and copy.

  tmp_x = &x;
  user_fun = function ();
  user_jac = jacobian_function ();

  // Try 5000 steps before giving up.

  iwork[5] = 5000;
  int working_too_hard = 0;

  if (stop_time_set)
    {
      itask = 4;
      rwork [0] = stop_time;
    }
  else
    {
      itask = 1;
    }

  double abs_tol = absolute_tolerance ();
  double rel_tol = relative_tolerance ();

  rwork[4] = (initial_step_size () >= 0.0) ? initial_step_size () : 0.0;
  rwork[5] = (maximum_step_size () >= 0.0) ? maximum_step_size () : 0.0;
  rwork[6] = (minimum_step_size () >= 0.0) ? minimum_step_size () : 0.0;

  if (restart)
    {
      restart = 0;
      istate = 1;
    }

 again:

  F77_FCN (lsode, LSODE) (lsode_f, n, xp, t, tout, itol, rel_tol,
			  abs_tol, itask, istate, iopt, rwork, lrw,
			  iwork, liw, lsode_j, method_flag);

  switch (istate)
    {
    case -13: // Return requested in user-supplied function.
    case -6:  // error weight became zero during problem. (solution
	      // component i vanished, and atol or atol(i) = 0.)
    case -5:  // repeated convergence failures (perhaps bad jacobian
	      // supplied or wrong choice of mf or tolerances).
    case -4:  // repeated error test failures (check all inputs).
    case -3:  // illegal input detected (see printed message).
    case -2:  // excess accuracy requested (tolerances too small).
      integration_error = 1;
      return ColumnVector ();
      break;

    case -1:  // excess work done on this call (perhaps wrong mf).
      working_too_hard++;
      if (working_too_hard > 20)
	{
	  (*current_liboctave_error_handler)
	    ("giving up after more than %d steps attempted in lsode",
	     iwork[5] * 20);
	  integration_error = 1;
	  return ColumnVector ();
	}
      else
	{
	  istate = 2;
	  goto again;
	}
      break;

    case 2:  // lsode was successful
      break;

    default:
      // Error?
      break;
    }

  t = tout;

  return x;
}

#if 0
void
LSODE::integrate (int nsteps, double tstep, ostream& s)
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
#endif

Matrix
LSODE::do_integrate (const ColumnVector& tout)
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
	  ColumnVector x_next = do_integrate (tout.elem (j));

	  if (integration_error)
	    return retval;

	  for (int i = 0; i < n; i++)
	    retval.elem (j, i) = x_next.elem (i);
	}
    }

  return retval;
}

Matrix
LSODE::integrate (const ColumnVector& tout, const ColumnVector& tcrit)
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

	      ColumnVector x_next = do_integrate (t_out);

	      if (integration_error)
		return retval;

	      if (save_output)
		{
		  for (int i = 0; i < n; i++)
		    retval.elem (i_out-1, i) = x_next.elem (i);
		}

	      if (do_restart)
		force_restart ();
	    }
	}
      else
	{
	  retval = do_integrate (tout);

	  if (integration_error)
	    return retval;
	}
    }

  return retval;
}

LSODE_options::LSODE_options (void)
{
  init ();
}

LSODE_options::LSODE_options (const LSODE_options& opt)
{
  copy (opt);
}

LSODE_options&
LSODE_options::operator = (const LSODE_options& opt)
{
  if (this != &opt)
    copy (opt);

  return *this;
}

LSODE_options::~LSODE_options (void)
{
}

void
LSODE_options::init (void)
{
  double sqrt_eps = sqrt (DBL_EPSILON);
  x_absolute_tolerance = sqrt_eps;
  x_initial_step_size = -1.0;
  x_maximum_step_size = -1.0;
  x_minimum_step_size = 0.0;
  x_relative_tolerance = sqrt_eps;
}

void
LSODE_options::copy (const LSODE_options& opt)
{
  x_absolute_tolerance = opt.x_absolute_tolerance;
  x_initial_step_size = opt.x_initial_step_size;
  x_maximum_step_size = opt.x_maximum_step_size;
  x_minimum_step_size = opt.x_minimum_step_size;
  x_relative_tolerance = opt.x_relative_tolerance;
}

void
LSODE_options::set_default_options (void)
{
  init ();
}

void
LSODE_options::set_absolute_tolerance (double val)
{
  x_absolute_tolerance = (val > 0.0) ? val : sqrt (DBL_EPSILON);
}

void
LSODE_options::set_initial_step_size (double val)
{
  x_initial_step_size = (val >= 0.0) ? val : -1.0;
}

void
LSODE_options::set_maximum_step_size (double val)
{
  x_maximum_step_size = (val >= 0.0) ? val : -1.0;
}

void
LSODE_options::set_minimum_step_size (double val)
{
  x_minimum_step_size = (val >= 0.0) ? val : 0.0;
}

void
LSODE_options::set_relative_tolerance (double val)
{
  x_relative_tolerance = (val > 0.0) ? val : sqrt (DBL_EPSILON);
}

double
LSODE_options::absolute_tolerance (void)
{
  return x_absolute_tolerance;
}

double
LSODE_options::initial_step_size (void)
{
  return x_initial_step_size;
}

double
LSODE_options::maximum_step_size (void)
{
  return x_maximum_step_size;
}

double
LSODE_options::minimum_step_size (void)
{
  return x_minimum_step_size;
}

double
LSODE_options::relative_tolerance (void)
{
  return x_relative_tolerance;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
