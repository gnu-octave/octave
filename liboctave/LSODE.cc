/*

Copyright (C) 1996, 1997 John W. Eaton

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

#include <iostream>

#include "LSODE.h"
#include "f77-fcn.h"
#include "lo-error.h"

typedef int (*lsode_fcn_ptr) (const int&, const double&, double*,
			      double*, int&);

typedef int (*lsode_jac_ptr) (const int&, const double&, double*,
			      const int&, const int&, double*, const
			      int&);

extern "C"
int F77_FUNC (lsode, LSODE) (lsode_fcn_ptr, int&, double*, double&,
			     double&, int&, double&, const double*, int&,
			     int&, int&, double*, int&, int*, int&,
			     lsode_jac_ptr, int&);

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
  itask = 1;
  iopt = 0;

  liw = 20 + n;
  lrw = 22 + n * (9 + n);

  sanity_checked = 0;
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
  itask = 1;
  iopt = 0;

  liw = 20 + n;
  lrw = 22 + n * (9 + n);

  sanity_checked = 0;
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
  ColumnVector tmp_deriv;

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
  ColumnVector retval;

  if (restart)
    {
      restart = 0;
      istate = 1;
    }

  if (iwork.length () != liw)
    {
      iwork.resize (liw);

      for (int i = 4; i < 9; i++)
	iwork.elem (i) = 0;
    }

  if (rwork.length () != lrw)
    {
      rwork.resize (lrw);

      for (int i = 4; i < 9; i++)
	rwork.elem (i) = 0;
    }

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

  if (! sanity_checked)
    {
      ColumnVector xdot = (*user_fun) (x, t);

      if (x.length () != xdot.length ())
	{
	  (*current_liboctave_error_handler)
	    ("lsode: inconsistent sizes for state and derivative vectors");

	  integration_error = 1;
	  return retval;
	}

      sanity_checked = 1;
    }

  if (stop_time_set)
    {
      itask = 4;
      rwork.elem (0) = stop_time;
      iopt = 1;
    }
  else
    {
      itask = 1;
    }

  double rel_tol = relative_tolerance ();

  const Array<double> abs_tol = absolute_tolerance ();

  int abs_tol_len = abs_tol.length ();

  int itol;

  if (abs_tol_len == 1)
    itol = 1;
  else if (abs_tol_len == n)
    itol = 2;
  else
    {
      (*current_liboctave_error_handler)
	("lsode: inconsistent sizes for state and absolute tolerance vectors");

      integration_error = 1;
      return retval;
    }

  if (initial_step_size () >= 0.0)
    {
      rwork.elem (4) = initial_step_size ();
      iopt = 1;
    }

  if (maximum_step_size () >= 0.0)
    {
      rwork.elem (5) = maximum_step_size ();
      iopt = 1;
    }

  if (minimum_step_size () >= 0.0)
    {
      rwork.elem (6) = minimum_step_size ();
      iopt = 1;
    }

  if (step_limit () > 0)
    {
      iwork.elem (5) = step_limit ();
      iopt = 1;
    }

  const double *pabs_tol = abs_tol.fortran_vec ();
  int *piwork = iwork.fortran_vec ();
  double *prwork = rwork.fortran_vec ();

  F77_XFCN (lsode, LSODE, (lsode_f, n, xp, t, tout, itol, rel_tol,
			   pabs_tol, itask, istate, iopt, prwork, lrw,
			   piwork, liw, lsode_j, method_flag));

  if (f77_exception_encountered)
    {
      integration_error = 1;
      (*current_liboctave_error_handler) ("unrecoverable error in lsode");
    }
  else
    {
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
	  break;

	case -1:  // excess work done on this call (perhaps wrong mf).
	  (*current_liboctave_error_handler)
	    ("giving up after more than %d steps attempted in lsode",
	     step_limit ());
	  integration_error = 1;
	  break;

	case 2:  // lsode was successful
	  retval = x;
	  t = tout;
	  break;
	  
	default: // Error?
	  (*current_liboctave_error_handler)
	    ("unrecognized value of istate returned from lsode");
	  break;
	}
    }

  return retval;
}

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
LSODE::do_integrate (const ColumnVector& tout, const ColumnVector& tcrit)
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

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
