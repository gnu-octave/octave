/*

Copyright (C) 2002 John W. Eaton

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

#include <iostream.h>
#include <fstream.h>

#include <cstdlib>
#include <cfloat>
#include <cmath>
#include "defun-dld.h"
#include "error.h"
#include "gripes.h"
#include "oct-obj.h"
#include "ov-fcn.h"
#include "pager.h"
#include "parse.h"
#include "unwind-prot.h"
#include "utils.h"
#include "variables.h"

// For instantiating the Array<Matrix> object.
#include "Array.h"
#include "Array.cc"

#include "DASRT.h"
#include "f77-fcn.h"
#include "lo-error.h"

#ifndef F77_FUNC
#define F77_FUNC(x, X) F77_FCN (x, X)
#endif

extern "C"
{
  int F77_FUNC (ddasrt, DASRT) (int (*)(const double&, double*, double*,
					double*, int&, double*, int*),
				const int&, const double&, double*, double*,
				const double&, int*, double*, double*,
				int&, double*, const int&, int*, 
				const int&, double*, int*,
				int (*)(const double&, double*,
					double*, double*,
					const double&, double*, int*),
				int (*)(const int&, const double&, double*,
					const int&, double*, double*, int*),
				const int&, int*);
}

template class Array<Matrix>;

static DAEFunc::DAERHSFunc user_fsub;
static DAEFunc::DAEJacFunc user_jsub;
static DAERTFunc::DAERTConstrFunc user_csub;
static int nn;

static int
ddasrt_f (const double& t, double *state, double *deriv, double *delta,
          int& ires, double *rpar, int *ipar)
{
  ColumnVector tmp_state (nn);
  for (int i = 0; i < nn; i++)
    tmp_state(i) = state[i];

  ColumnVector tmp_deriv (nn);
  for (int i = 0; i < nn; i++)
    tmp_deriv(i) = deriv[i];

  ColumnVector tmp_fval = user_fsub (tmp_state, tmp_deriv, t, ires);

  if (tmp_fval.length () == 0)
    ires = -2;
  else
    {
      for (int i = 0; i < nn; i++)
	delta[i] = tmp_fval(i);
    }

  return 0;
}

//typedef int (*efptr) (const double& t, const int& n, double *state,
//		      double *ework, double *rpar, int *ipar,
//		      const int& ieform, int& ires);

//static efptr e_fun;

static int
ddasrt_j (const double& t, double *state, double *deriv,
	  double *pdwork, const double& cj, double *rpar, int *ipar) 
{
  ColumnVector tmp_state (nn);
  for (int i = 0; i < nn; i++)
    tmp_state(i) = state[i];

  ColumnVector tmp_deriv (nn);
  for (int i = 0; i < nn; i++)
    tmp_deriv(i) = deriv[i];

  // XXX FIXME XXX

  Matrix tmp_dfdxdot (nn, nn);
  Matrix tmp_dfdx (nn, nn);

  DAEFunc::DAEJac tmp_jac;
  tmp_jac.dfdxdot = &tmp_dfdxdot;
  tmp_jac.dfdx    = &tmp_dfdx;

  tmp_jac = user_jsub (tmp_state, tmp_deriv, t);

  // Fix up the matrix of partial derivatives for dasrt.

  tmp_dfdx = tmp_dfdx + cj * tmp_dfdxdot;

  for (int j = 0; j < nn; j++)
    for (int i = 0; i < nn; i++)
      pdwork[j*nn+i] = tmp_dfdx.elem (i, j);

  return 0;
}

static int
ddasrt_g (const int& neq, const double& t, double *state, const int& ng,
	  double *gout, double *rpar, int *ipar) 
{
  int n = neq;

  ColumnVector tmp_state (n);
  for (int i = 0; i < n; i++)
    tmp_state(i) = state[i];

  ColumnVector tmp_fval = user_csub (tmp_state, t);

  for (int i = 0; i < ng; i++)
    gout[i] = tmp_fval(i);

  return 0;
}


DASRT::DASRT (void)
  : DAERT ()
{
  initialized = false;
  restart = false;

  stop_time_set = false;
  stop_time = 0.0;

  sanity_checked = false;

  info.resize (30, 0);

  npar = 0;

  liw = 0;
  lrw = 0;
}

DASRT::DASRT (const int& ng, const ColumnVector& state, 
	      const ColumnVector& deriv, double time, DAERTFunc& f)
  : DAERT (state, deriv, time, f)
{
  n = size ();

  initialized = false;
  restart = false;

  stop_time_set = false;
  stop_time = 0.0;

  DAERTFunc::operator = (f);

  sanity_checked = false;

  info.resize (30, 0);
  jroot.resize (ng, 1);

  npar = 0;

  rpar.resize (npar+1);
  ipar.resize (npar+1);

  info(11) = npar;

  // Also store it here, for communication with user-supplied
  // subroutines.
  ipar(0) = npar;

  y.resize (n, 1, 0.0);
  ydot.resize (n, 1, 0.0);
}

void
DASRT::init_work_size (int info_zero)
{
  double t;
  double *py = y.fortran_vec ();
  double *pydot = ydot.fortran_vec ();
  double rel_tol = relative_tolerance ();
  double abs_tol = absolute_tolerance ();
  int *pinfo = info.fortran_vec ();
  double *prpar = rpar.fortran_vec ();
  int *pipar = ipar.fortran_vec ();
  int *pjroot = jroot.fortran_vec ();
  int idid;

  // We do not have to lie.
  rwork.resize (5000+9*n+n*n, 0.0);
  iwork.resize (n+20, 0);

  liw = n+20;
  lrw = 5000+9*n+n*n;

  double *prwork = rwork.fortran_vec ();
  int *piwork = iwork.fortran_vec ();


  F77_FUNC (ddasrt, DASRT) (ddasrt_f, n, t, py, pydot, t, pinfo,
			    &rel_tol, &abs_tol, idid, prwork, lrw,
			    piwork, liw, prpar, pipar, ddasrt_j,
			    ddasrt_g, ng, pjroot);

  int iwadd = iwork(18);


  if (iwadd > 0)
    liw += iwadd;

  info(0) = 0;

  iwork.resize (liw, 0);

  piwork = iwork.fortran_vec ();

  F77_FUNC (ddasrt, DASRT) (ddasrt_f, n, t, py, pydot, t, pinfo,
			    &rel_tol, &abs_tol, idid, prwork, lrw,
			    piwork, liw, prpar, pipar, ddasrt_j,
			    ddasrt_g, ng, pjroot);

  int rwadd = iwork(19);


  if (rwadd > 0)
    lrw += rwadd;

  rwork.resize (lrw, 0.0);

  info(0) = info_zero;

}

void
DASRT::force_restart (void)
{
  restart = true;
  integration_error = false;
}

void
DASRT::set_stop_time (double t)
{
  stop_time_set = true;
  stop_time = t;
}

void
DASRT::set_ng (int the_ng)
{
  ng = the_ng;
}

int DASRT::get_ng (void)
{
  return ng;
}

void
DASRT::clear_stop_time (void)
{
  stop_time_set = false;
}

void
DASRT::integrate (double tout)
{
  DASRT_result retval;

  if (! initialized)
    {
      info(0) = 0;

      for (int i = 0; i < n; i++)
       {
	y(i,0) = x(i);
        ydot(i,0) = xdot(i);
       }

      integration_error = false;

      user_fsub = DAEFunc::function ();
      user_jsub = DAEFunc::jacobian_function ();
      user_csub = DAERTFunc::constraint_function ();

      if (user_jsub)
	info(4) = 1;
      else
	info(4) = 0;

      if (! sanity_checked)
	{
	  int ires = 0;

	  ColumnVector fval = user_fsub (x, xdot, t, ires);

	  if (fval.length () != x.length ())
	    {
	      (*current_liboctave_error_handler)
		("dassl: inconsistent sizes for state and residual vectors");

	      integration_error = true;
	      return;
	    }

	  sanity_checked = true;
	}

  
      init_work_size (info(0));




      if (iwork.length () != liw)
	iwork.resize (liw);

      if (rwork.length () != lrw)
	rwork.resize (lrw);

      abs_tol = absolute_tolerance ();
      rel_tol = relative_tolerance ();


      if (initial_step_size () >= 0.0)
	{
	  rwork(2) = initial_step_size ();
	  info(7) = 1;
	}
      else
	info(7) = 0;

      if (step_limit () >= 0)
	{
	  info(11) = 1;
	  iwork(18) = step_limit ();
	}
      else
	info(11) = 0;

      if (maximum_step_size () >= 0.0)
	{
	  rwork(1) = maximum_step_size ();
	  info(6) = 1;
	}
      else
	info(6) = 0;


      py = y.fortran_vec ();
      pydot = ydot.fortran_vec ();
      pinfo = info.fortran_vec ();
      piwork = iwork.fortran_vec ();
      prwork = rwork.fortran_vec ();
      prpar = rpar.fortran_vec ();
      pipar = ipar.fortran_vec ();
      pjroot = jroot.fortran_vec ();

      info(5) = 0;
      info(8) = 0;
      initialized = true;
    }

  if (restart)
    {
      info(0) = 0;

      if (stop_time_set)
	{
	  info(3) = 1;
	  rwork(0) = stop_time;
	}
      else
	info(3) = 0;
    }




  F77_XFCN (ddasrt, DASRT, (ddasrt_f, n, t, py, pydot, tout, pinfo,
			    &rel_tol, &abs_tol, idid, prwork, lrw,
			    piwork, liw, prpar, pipar, ddasrt_j,
			    ddasrt_g, ng, pjroot));

  if (f77_exception_encountered)
    {
      integration_error = true;
      (*current_liboctave_error_handler) ("unrecoverable error in dassl");
    }
  else
    {
      switch (idid)
	{
	case 0: // Initial conditions made consistent.
	case 1: // A step was successfully taken in intermediate-output
	        // mode. The code has not yet reached TOUT.
	case 2: // The integration to TOUT was successfully completed
	        // (T=TOUT) by stepping exactly to TOUT.
	case 3: // The integration to TOUT was successfully completed
	        // (T=TOUT) by stepping past TOUT.  Y(*) is obtained by
	        // interpolation.  YPRIME(*) is obtained by interpolation.
	case 5: // The integration to TSTOP was successfully completed
	        // (T=TSTOP) by stepping to TSTOP within the
	        // tolerance.  Must restart to continue.
	  for (int i = 0; i < n; i++)
	    x(i) = y(i,0);
	  t = tout;
	  break;

	case 4: //  We've hit the stopping condition.
          for (int i = 0; i < n; i++)
            x(i) = y(i,0);
          break;

	case -1: // A large amount of work has been expended.  (~500 steps).
	case -2: // The error tolerances are too stringent.
	case -3: // The local error test cannot be satisfied because you
	         // specified a zero component in ATOL and the
		 // corresponding computed solution component is zero.
		 // Thus, a pure relative error test is impossible for
		 // this component.
	case -6: // DDASRT had repeated error test failures on the last
		 // attempted step.
	case -7: // The corrector could not converge.
	case -8: // The matrix of partial derivatives is singular.
	case -9: // The corrector could not converge.  There were repeated
		 // error test failures in this step.
	case -10: // The corrector could not converge because IRES was
		  // equal to minus one.
	case -11: // IRES equal to -2 was encountered and control is being
		  // returned to the calling program.
	case -12: // DASSL failed to compute the initial YPRIME.
	case -33: // The code has encountered trouble from which it cannot
		  // recover. A message is printed explaining the trouble
		  // and control is returned to the calling program. For
		  // example, this occurs when invalid input is detected.
	default:
	  integration_error = true;
	  (*current_liboctave_error_handler)
	    ("ddasrt failed with IDID = %d", idid);
	  break;
	}
    }
}

DASRT_result
DASRT::integrate (const ColumnVector& tout)
{
  DASRT_result retval;

  Matrix x_out;
  Matrix xdot_out;
  ColumnVector t_out;

  int oldj = 0;

  int n_out = tout.capacity ();


  if (n_out > 0 && n > 0)
    {
      x_out.resize (n_out, n);
      xdot_out.resize (n_out, n);
      t_out.resize (n_out);

      for (int j = 0; j < n_out; j++)
	{
	  integrate (tout(j));
	  if (integration_error)
	    {
	      retval = DASRT_result (x_out, xdot_out, t_out);
	      return retval;
	    }
          if (idid == 4)
            t_out(j) = t;
          else
            t_out(j) = tout(j);


	  for (int i = 0; i < n; i++)
	    {
	      x_out(j,i) = y(i,0);
	      xdot_out(j,i) = ydot(i,0);
	    }
          if (idid ==4)
           {
            oldj = j;
            j = n_out;
            x_out.resize (oldj+1, n);
            xdot_out.resize (oldj+1, n);
            t_out.resize (oldj+1);
           }
	}
    }

  retval = DASRT_result (x_out, xdot_out, t_out);

  return retval;
}

DASRT_result
DASRT::integrate (const ColumnVector& tout, const ColumnVector& tcrit) 
{
  DASRT_result retval;

  Matrix x_out;
  Matrix xdot_out;
  ColumnVector t_outs;

  int n_out = tout.capacity ();

  if (n_out > 0 && n > 0)
    {
      x_out.resize (n_out, n);
      xdot_out.resize (n_out, n);
      t_outs.resize (n_out);

      int n_crit = tcrit.capacity ();

      if (n_crit > 0)
	{
	  int i_crit = 0;
	  int i_out = 0;
	  double next_crit = tcrit(0);
	  double next_out;
	  while (i_out < n_out)
	    {
	      bool do_restart = false;

	      next_out = tout(i_out);
	      if (i_crit < n_crit)
		next_crit = tcrit(i_crit);

	      int save_output;
	      double t_out;

	      if (next_crit == next_out)
		{
		  set_stop_time (next_crit);
		  t_out = next_out;
		  save_output = 1;
		  i_out++;
		  i_crit++;
		  do_restart = true;
		}
	      else if (next_crit < next_out)
		{
		  if (i_crit < n_crit)
		    {
		      set_stop_time (next_crit);
		      t_out = next_crit;
		      save_output = 0;
		      i_crit++;
		      do_restart = true;
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

	      integrate (t_out);

	      if (integration_error)
		{
		  retval = DASRT_result (x_out, xdot_out, t_outs);
		  return retval;
		}
              if (idid == 4)
                t_out = t;

	      if (save_output)
		{
		  for (int i = 0; i < n; i++)
		    {
		      x_out(i_out-1,i) = y(i,0);
		      xdot_out(i_out-1,i) = ydot(i,0);
		    }
                  t_outs(i_out-1) = t_out;
                  if (idid ==4)
                    {
                      x_out.resize (i_out, n);
                      xdot_out.resize (i_out, n);
                      t_outs.resize (i_out);
                      i_out = n_out;
                    }

		}

	      if (do_restart)
		force_restart ();
	    }

	  retval = DASRT_result (x_out, xdot_out, t_outs);
	}
      else
	{
	  retval = integrate (tout);

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
