// DAE.cc                                               -*- C++ -*-
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
Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "DAE.h"
#include "f77-uscore.h"
#include "lo-error.h"

extern "C"
{
  int F77_FCN (ddassl) (int (*)(double*, double*, double*, double*,
				int*, double*, int*),
			const int*, double*, double*, double*,
			double*, const int*, const double*,
			const double*, int*, double*, const int*, 
			int*, const int*, const double*, const int*,
			int (*)(double*, double*, double*, double*,
				double*, double*, int*));
}

static DAEFunc::DAERHSFunc user_fun;
static DAEFunc::DAEJacFunc user_jac;
static int nn;

DAE::DAE (void)
{
  n = 0;
  t = 0.0;

  stop_time_set = 0;
  stop_time = 0.0;

  integration_error = 0;
  restart = 1;

  DAEFunc::set_function (0);
  DAEFunc::set_jacobian_function (0);

  liw = 0;
  lrw = 0;

  info  = new int [15];
  iwork = (int *) 0;
  rwork = (double *) 0;

  for (int i = 0; i < 15; i++)
    info [i] = 0;
}

DAE::DAE (int size)
{
  n = size;
  t = 0.0;

  stop_time_set = 0;
  stop_time = 0.0;

  integration_error = 0;
  restart = 1;

  DAEFunc::set_function (0);
  DAEFunc::set_jacobian_function (0);

  liw = 20 + n;
  lrw = 40 + 9*n + n*n;

  info  = new int [15];
  iwork = new int [liw];
  rwork = new double [lrw];

  for (int i = 0; i < 15; i++)
    info [i] = 0;
}

DAE::DAE (const Vector& state, double time, DAEFunc& f)
{
  n = state.capacity ();
  t = time;
  x = state;
  xdot.resize (n, 0.0);

  stop_time_set = 0;
  stop_time = 0.0;

  integration_error = 0;
  restart = 1;

  DAEFunc::set_function (f.function ());
  DAEFunc::set_jacobian_function (f.jacobian_function ());

  liw = 20 + n;
  lrw = 40 + 9*n + n*n;

  info  = new int [15];
  iwork = new int [liw];
  rwork = new double [lrw];

  for (int i = 0; i < 15; i++)
    info [i] = 0;
}

DAE::DAE (const Vector& state, const Vector& deriv, double time, DAEFunc& f)
{
  if (deriv.capacity () != state.capacity ())
    {
      (*current_liboctave_error_handler)
	("x, xdot size mismatch in DAE constructor");
      n = 0;
      t = 0.0;
      return;
    }

  n = state.capacity ();
  t = time;
  xdot = deriv;
  x = state;

  stop_time_set = 0;
  stop_time = 0.0;

  DAEFunc::set_function (f.function ());
  DAEFunc::set_jacobian_function (f.jacobian_function ());

  liw = 20 + n;
  lrw = 40 + 9*n + n*n;

  info  = new int [15];
  iwork = new int [liw];
  rwork = new double [lrw];

  for (int i = 0; i < 15; i++)
    info [i] = 0;
}

DAE::~DAE (void)
{
  delete info;
  delete rwork;
  delete iwork;
}

Vector
DAE::deriv (void)
{
  return xdot;
}

void
DAE::initialize (const Vector& state, double time)
{
  integration_error = 0;
  restart = 1;
  x = state;
  int nx = x.capacity ();
  xdot.resize (nx, 0.0);
  t = time;
}

void
DAE::initialize (const Vector& state, const Vector& deriv, double time)
{
  integration_error = 0;
  restart = 1;
  xdot = deriv;
  x = state;
  t = time;
}

int
ddassl_f (double *time, double *state, double *deriv, double *delta,
	  int *ires, double *rpar, int *ipar)
{
  Vector tmp_deriv (nn);
  Vector tmp_state (nn);
  Vector tmp_delta (nn);

  for (int i = 0; i < nn; i++)
    {
      tmp_deriv.elem (i) = deriv [i];
      tmp_state.elem (i) = state [i];
    }

  tmp_delta = user_fun (tmp_state, tmp_deriv, *time);

  if (tmp_delta.length () == 0)
    *ires = -2;
  else
    {
      for (i = 0; i < nn; i++)
	delta [i] = tmp_delta.elem (i);
    }

  return 0;
}

int
ddassl_j (double *time, double *state, double *deriv, double *pd,
	  double *cj, double *rpar, int *ipar)
{
  Vector tmp_state (nn);
  Vector tmp_deriv (nn);

// XXX FIXME XXX

  Matrix tmp_dfdxdot (nn, nn);
  Matrix tmp_dfdx (nn, nn);

  DAEFunc::DAEJac tmp_jac;
  tmp_jac.dfdxdot = &tmp_dfdxdot;
  tmp_jac.dfdx    = &tmp_dfdx;

  tmp_jac = user_jac (tmp_state, tmp_deriv, *time);

  // Fix up the matrix of partial derivatives for dassl.

  tmp_dfdx = tmp_dfdx + (tmp_dfdxdot * (*cj));

  for (int j = 0; j < nn; j++)
    for (int i = 0; i < nn; i++)
      pd [nn * j + i] = tmp_dfdx.elem (i, j);

  return 0;
}

Vector
DAE::integrate (double tout)
{
  integration_error = 0;

  if (DAEFunc::jacobian_function ())
    iwork [4] = 1;
  else
    iwork [4] = 0;

  double *px    = x.fortran_vec ();
  double *pxdot = xdot.fortran_vec ();

  nn = n;
  user_fun = DAEFunc::fun;
  user_jac = DAEFunc::jac;

  if (stop_time_set)
    {
      info [3] = 1;
      rwork [0] = stop_time;
    }
  else
    info [3] = 0;

  double abs_tol = absolute_tolerance ();
  double rel_tol = relative_tolerance ();

  if (initial_step_size () >= 0.0)
    {
      rwork[2] = initial_step_size ();
      info[7] = 1;
    }
  else
    info[7] = 0;

  if (maximum_step_size () >= 0.0)
    {
      rwork[2] = maximum_step_size ();
      info[6] = 1;
    }
  else
    info[6] = 0;

  double dummy;
  int idummy;

  if (restart)
    {
      restart = 0;
      info[0] = 0;
    }

// again:

  F77_FCN (ddassl) (ddassl_f, &n, &t, px, pxdot, &tout, info,
		    &rel_tol, &abs_tol, &idid, rwork, &lrw, iwork,
		    &liw, &dummy, &idummy, ddassl_j);

  switch (idid)
    {
    case 1: // A step was successfully taken in the
	    // intermediate-output mode. The code has not yet reached
	    // TOUT.
      break;
    case 2: // The integration to TSTOP was successfully completed
	    // (T=TSTOP) by stepping exactly to TSTOP.
      break;
    case 3: // The integration to TOUT was successfully completed
	    // (T=TOUT) by stepping past TOUT.  Y(*) is obtained by
	    // interpolation.  YPRIME(*) is obtained by interpolation.
      break;
    case -1: // A large amount of work has been expended.  (About 500 steps).
    case -2: // The error tolerances are too stringent.
    case -3: // The local error test cannot be satisfied because you
	     // specified a zero component in ATOL and the
	     // corresponding computed solution component is zero.
	     // Thus, a pure relative error test is impossible for
	     // this component.
    case -6: // DDASSL had repeated error test failures on the last
	     // attempted step.
    case -7: // The corrector could not converge.
    case -8: // The matrix of partial derivatives is singular.
    case -9: // The corrector could not converge.  There were repeated
	     // error test failures in this step.
    case -10: // The corrector could not converge because IRES was
	      // equal to minus one.
    case -11: // IRES equal to -2 was encountered and control is being
	      // returned to the calling program.
    case -12: // DDASSL failed to compute the initial YPRIME.
    case -33: // The code has encountered trouble from which it cannot
	      // recover. A message is printed explaining the trouble
	      // and control is returned to the calling program. For
	      // example, this occurs when invalid input is detected.
    default:
      integration_error = 1;
      break;
    }

  t = tout;

  return x;
}

Matrix
DAE::integrate (const Vector& tout, Matrix& xdot_out)
{
  Matrix retval;
  int n_out = tout.capacity ();

  if (n_out > 0 && n > 0)
    {
      retval.resize (n_out, n);
      xdot_out.resize (n_out, n);

      for (int i = 0; i < n; i++)
	{
	  retval.elem (0, i) = x.elem (i);
	  xdot_out.elem (0, i) = xdot.elem (i);
	}

      for (int j = 1; j < n_out; j++)
	{
	  ColumnVector x_next = integrate (tout.elem (j));

	  if (integration_error)
	    return retval;

	  for (i = 0; i < n; i++)
	    {
	      retval.elem (j, i) = x_next.elem (i);
	      xdot_out.elem (j, i) = xdot.elem (i);
	    }
	}
    }

  return retval;
}

Matrix
DAE::integrate (const Vector& tout, Matrix& xdot_out, const Vector& tcrit) 
{
  Matrix retval;
  int n_out = tout.capacity ();

  if (n_out > 0 && n > 0)
    {
      retval.resize (n_out, n);
      xdot_out.resize (n_out, n);

      for (int i = 0; i < n; i++)
	{
	  retval.elem (0, i) = x.elem (i);
	  xdot_out.elem (0, i) = xdot.elem (i);
	}

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

	      if (integration_error)
		return retval;

	      if (save_output)
		{
		  for (i = 0; i < n; i++)
		    {
		      retval.elem (i_out-1, i) = x_next.elem (i);
		      xdot_out.elem (i_out-1, i) = xdot.elem (i);
		    }
		}

	      if (do_restart)
		force_restart ();
	    }
	}
      else
	{
	  retval = integrate (tout, xdot_out);

	  if (integration_error)
	    return retval;
	}
    }

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
