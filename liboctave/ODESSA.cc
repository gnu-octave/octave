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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cfloat>
#include <cmath>

// For instantiating the Array<Matrix> object.
#include "Array.h"
#include "Array.cc"

#include "ODESSA.h"
#include "f77-fcn.h"
#include "lo-error.h"
#include "lo-sstream.h"
#include "quit.h"

typedef int (*odessa_fcn_ptr) (int*, const double&, double*,
			       double*, double*);

typedef int (*odessa_jac_ptr) (int*, const double&, double*,
			       double*, const int&, const int&,
			       double*, const int&);

typedef int (*odessa_dfdp_ptr) (int*, const double&, double*,
				double*, double*, const int&);


extern "C"
{
  F77_RET_T
  F77_FUNC (dodessa, DODESSA) (odessa_fcn_ptr, odessa_dfdp_ptr, int*,
			       double*, double*, double&, double&,
			       int&, double&, const double*, int&, 
			       int&, int*, double*, int&, int*, int&,
			       odessa_jac_ptr, int&);
}

INSTANTIATE_ARRAY (Matrix);

static ODESFunc::ODES_fsub user_fsub;
static ODESFunc::ODES_bsub user_bsub;
static ODESFunc::ODES_jsub user_jsub;


static int
odessa_f (int* neq, const double& t, double *state,
	  double *par, double *fval)
{
  BEGIN_INTERRUPT_WITH_EXCEPTIONS;

  int n = neq[0];
  int n_par = neq[1];

  // Load the state and parameter arrays as Octave objects

  ColumnVector tmp_state (n);
  for (int i = 0; i < n; i++)
    tmp_state(i) = state[i];

  ColumnVector tmp_param (n_par);
  for (int i = 0; i < n_par; i++)
    tmp_param(i) = par[i];

  ColumnVector tmp_fval = user_fsub (tmp_state, t, tmp_param);

  if (tmp_fval.length () == 0)
    octave_jump_to_enclosing_context ();
  else
    {
      for (int i = 0; i < n; i++)
	fval[i] = tmp_fval(i);
    }

  END_INTERRUPT_WITH_EXCEPTIONS;

  return 0;
}

static int
odessa_j (int* neq, const double& t, double *state,
	  double *par, const int& /* ml */, const int& /* mu */,
	  double *pd, const int& nrowpd)
{
  BEGIN_INTERRUPT_WITH_EXCEPTIONS;

  int n = neq[0];
  int n_par = neq[1];

  // Load the state and parameter arrays as Octave objects
  ColumnVector tmp_state (n);
  for (int i = 0; i < n; i++)
    tmp_state(i) = state[i];

  ColumnVector tmp_param (n_par);
  for (int i = 0; i < n_par; i++)
    tmp_param(i) = par[i];

  Matrix tmp_fval = user_jsub (tmp_state, t, tmp_param);

  if (tmp_fval.length () == 0)
    octave_jump_to_enclosing_context ();
  else
    {
      for (int j = 0; j < n; j++)
	for (int i = 0; i < nrowpd; i++)
	  pd[nrowpd*j+i] = tmp_fval(i,j);
    }

  END_INTERRUPT_WITH_EXCEPTIONS;

  return 0;
}

static int
odessa_b (int* neq, const double& t, double *state,
	  double *par, double *dfdp, const int& jpar)

{
  BEGIN_INTERRUPT_WITH_EXCEPTIONS;

  int n = neq[0];
  int n_par = neq[1];

  // Load the state and parameter arrays as Octave objects
  ColumnVector tmp_state (n);
  for (int i = 0; i < n; i++)
    tmp_state(i) = state[i];

  ColumnVector tmp_param (n_par);
  for (int i = 0; i < n_par; i++)
    tmp_param(i) = par[i];

  ColumnVector tmp_fval = user_bsub (tmp_state, t, tmp_param, jpar);

  if (tmp_fval.length () == 0)
    octave_jump_to_enclosing_context ();
  else
    {
      for (int i = 0; i < n; i++)
	dfdp[i] = tmp_fval(i);
    }

  END_INTERRUPT_WITH_EXCEPTIONS;

  return 0;
}

ODESSA::ODESSA (void) : ODES (), ODESSA_options ()
{
  initialized = false;

  neq.resize(2);
  n = size ();

  iopt.resize(4);

  itask = 1;
  iopt(0) = 0;
  isopt = 0;
  iopt(1) = isopt;
  npar = 0;
  neq(0) = n;
  neq(1) = npar;

  sanity_checked = false;
}

ODESSA::ODESSA (const ColumnVector& s, double tm, ODESFunc& f)
  : ODES (s, tm, f), ODESSA_options ()
{
  initialized = false;

  neq.resize(2);
  n = size ();

  iopt.resize(4);
  itask = 1;
  iopt(0) = 0;
  isopt = 0;
  iopt(1) = isopt;

  sanity_checked = false;

  npar = 0;
  neq(0) = n;
  neq(1) = npar;

  y.resize (n, 1, 0.0);
}

ODESSA::ODESSA (const ColumnVector& s, const ColumnVector& xtheta,
		const Matrix& sensitivity_guess, double tm, ODESFunc& f)
  : ODES (s, xtheta, tm, f)
{
  initialized = false;

  neq.resize(2);
  n = s.length();
  npar = xtheta.length();

  neq(0) = n;
  neq(1) = npar;

  sx0 = sensitivity_guess;
  par.resize (npar);

  for (int i = 0; i < npar; i++)
    {
      par(i) = xtheta(i);
    }

  sanity_checked = false;

  npar = xtheta.length ();

  iopt.resize(4);
  itask = 1;
  iopt(0) = 0;
  isopt = 1;
  iopt(1) = isopt;

  y.resize (n, npar+1, 0.0);
}

void
ODESSA::integrate (double tout)
{
  ODESSA_result retval;

  if (! initialized)
    {
      
      for (int i = 0; i < n; i++)
	y(i,0) = x(i);
      
      if (npar > 0)
	{
	  for (int j = 0; j < npar; j++)
	    for (int i = 0; i < n; i++)
	      y(i,j+1) = sx0(i,j);
	}
      
      integration_error = false;
      
      user_fsub = ODESFunc::fsub_function ();
      user_bsub = ODESFunc::bsub_function ();
      user_jsub = ODESFunc::jsub_function ();
      
      int idf;

      if (user_bsub)
	idf = 1;
      else
	idf = 0;
      
      iopt(2) = idf;

      if (restart)
	{
	  restart = false;
	  istate = 1;
	}

      int max_maxord = 0;

      if (integration_method () == "stiff")
	{
	  if (user_jsub)
	    method_flag = 21;
	  else
	    method_flag = 22;

	  max_maxord = 5;

	  if (isopt)
	    {
	      liw = 21 + n + npar;
	      lrw = 22 + 8*(npar+1)*n + n*n + n;
	    }
	  else
	    {
	      liw = 20 + n;
	      lrw = 22 + 9*n + n*n;
	    }
	}
      else
	{
	  max_maxord = 12;

	  if (isopt)
	    {
	      if (user_jsub)
		method_flag = 11;
	      else
		method_flag = 12;
	      liw = 21 + n + npar;
	      lrw = 22 + 15*(npar+1)*n + n*n + n;
	    }
	  else
	    {
	      method_flag = 10;
	      liw = 20 + n;
	      lrw = 22 + 16 * n;
	    }
	}
      
      if (iwork.length () != liw)
	{
	  iwork.resize (liw);
	  
	  for (int i = 4; i < 10; i++)
	    iwork.elem (i) = 0;
	}
      
      if (rwork.length () != lrw)
	{
	  rwork.resize (lrw);
	  
	  for (int i = 4; i < 10; i++)
	    rwork.elem (i) = 0.0;
	}
      
      maxord = maximum_order ();

      if (maxord >= 0)
	{
	  if (maxord > 0 && maxord <= max_maxord)
	    {
	      iwork(4) = maxord;
	      iopt(0) = 1;
	    }
	  else
	    {
	      (*current_liboctave_error_handler)
		("odessa: invalid value for maximum order");
	      integration_error = true;
	      return;
	    }
	}

      initialized = true;
    }

  integration_error = false;

  // NOTE: this won't work if LSODE passes copies of the state vector.
  //       In that case we have to create a temporary vector object
  //       and copy.


  if (! sanity_checked)
    {
      ColumnVector fval = user_fsub (x, t, theta);
      
      if (fval.length () != x.length ())
	{
	  (*current_liboctave_error_handler)
	    ("odessa: inconsistent sizes for state and residual vectors");
	  
	  integration_error = true;
	  return;
	}

      sanity_checked = true;
    }

  if (stop_time_set)
    {
      itask = 4;
      rwork.elem (0) = stop_time;
      iopt(0) = 1;
    }
  else
    {
      itask = 1;
    }

  double rel_tol = relative_tolerance ();

  Array<double> abs_tol = absolute_tolerance ();  //note; this should
  //  be a matrix, not a vector

  int abs_tol_len = abs_tol.length ();

  int itol;

  if (abs_tol_len == 1)
    itol = 1;
  else if (abs_tol_len == n)
    itol = 2;
  else
    {
      (*current_liboctave_error_handler)
        ("odessa: inconsistent sizes for state and absolute tolerance vectors");

      integration_error = 1;
      return;
    }

  if (initial_step_size () >= 0.0)
    {
      rwork.elem (4) = initial_step_size ();
      iopt(0) = 1;
    }

  if (maximum_step_size () >= 0.0)
    {
      rwork.elem (5) = maximum_step_size ();
      iopt(0) = 1;
    }

  if (minimum_step_size () >= 0.0)
    {
      rwork.elem (6) = minimum_step_size ();
      iopt(0) = 1;
    }

  if (step_limit () > 0)
    {
      iwork.elem (5) = step_limit ();
      iopt(0) = 1;
    }

      py = y.fortran_vec ();
      piwork = iwork.fortran_vec ();
      prwork = rwork.fortran_vec ();
      ppar = par.fortran_vec ();
      piopt = iopt.fortran_vec ();
      pneq = neq.fortran_vec ();

  const double *pabs_tol = abs_tol.fortran_vec ();

   F77_XFCN (dodessa, DODESSA, (odessa_f, odessa_b, pneq, py, ppar, t,
				tout, itol, rel_tol, pabs_tol, itask,
				istate, piopt, prwork, lrw, piwork, liw,
				odessa_j, method_flag));

  if (f77_exception_encountered)
    {
      integration_error = true;
      (*current_liboctave_error_handler) ("unrecoverable error in odessa");
    }
  else
    {
      switch (istate)
        {
	case 1:  // prior to initial integration step.
        case 2:  // odessa was successful.
          t = tout;
          break;
          
        case -1:  // excess work done on this call (perhaps wrong mf).
        case -2:  // excess accuracy requested (tolerances too small).
        case -3:  // illegal input detected (see printed message).
        case -4:  // repeated error test failures (check all inputs).
        case -5:  // repeated convergence failures (perhaps bad jacobian
                  // supplied or wrong choice of mf or tolerances).
        case -6:  // error weight became zero during problem. (solution
                  // component i vanished, and atol or atol(i) = 0.)
        case -13: // Return requested in user-supplied function.
          integration_error = 1;
          break;
	  
        default:
          integration_error = 1;
          (*current_liboctave_error_handler)
            ("unrecognized value of istate (= %d) returned from odessa",
	     istate);
          break;
        }
    }
}

std::string
ODESSA::error_message (void) const
{
  std::string retval;

  OSSTREAM buf;
  buf << t << OSSTREAM_ENDS;
  std::string t_curr = OSSTREAM_STR (buf);
  OSSTREAM_FREEZE (buf);

  switch (istate)
    {
    case 1:
      retval = "prior to initial integration step";
      break;

    case 2:
      retval = "successful exit";
      break;
          
    case 3:
      retval = "prior to continuation call with modified parameters";
      break;
          
    case -1:
      retval = std::string ("excess work on this call (t = ")
	+ t_curr + "; perhaps wrong integration method)";
      break;

    case -2:
      retval = "excess accuracy requested (tolerances too small)";
      break;

    case -3:
      retval = "invalid input detected (see printed message)";
      break;

    case -4:
      retval = std::string ("repeated error test failures (t = ")
	+ t_curr + "check all inputs)";
      break;

    case -5:
      retval = std::string ("repeated convergence failures (t = ")
	+ t_curr
	+ "perhaps bad jacobian supplied or wrong choice of integration method or tolerances)";
      break;

    case -6:
      retval = std::string ("error weight became zero during problem. (t = ")
	+ t_curr
	+ "; solution component i vanished, and atol or atol(i) == 0)";
      break;

    case -13:
      retval = "return requested in user-supplied function (t = "
	+ t_curr + ")";
      break;

    default:
      retval = "unknown error state";
      break;
    }

  return retval;
}


ODESSA_result
ODESSA::integrate (const ColumnVector& tout)
{
  ODESSA_result retval;

  Matrix x_out;

  Array<Matrix> x_s_out;

  int n_out = tout.capacity ();

  if (n_out > 0 && n > 0)
    {
      x_out.resize (n_out, n);

      x_s_out.resize (npar, Matrix (n_out, n, 0.0));

      for (int j = 0; j < n_out; j++)
	{
	  integrate (tout(j));

	  if (integration_error)
	    {
	      retval = ODESSA_result (x_out, x_s_out);
	      return retval;
	    }

	  for (int i = 0; i < n; i++)
	    {
	      x_out(j,i) = y(i,0);

	      for (int k = 0; k < npar; k++)
		{
		  x_s_out(k)(j,i) = y(i,k+1);
		}
	    }
	}
    }

  retval = ODESSA_result (x_out, x_s_out);

  return retval;
}

ODESSA_result
ODESSA::integrate (const ColumnVector& tout, const ColumnVector& tcrit) 
{
  ODESSA_result retval;

  Matrix x_out;

  Array<Matrix> x_s_out;

  int n_out = tout.capacity ();

  if (n_out > 0 && n > 0)
    {
      x_out.resize (n_out, n);

      x_s_out.resize (npar, Matrix (n_out, n, 0.0));

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
		  save_output = true;
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
		      save_output = false;
		      i_crit++;
		      do_restart = true;
		    }
		  else
		    {
		      clear_stop_time ();
		      t_out = next_out;
		      save_output = true;
		      i_out++;
		    }
		}
	      else
		{
		  set_stop_time (next_crit);
		  t_out = next_out;
		  save_output = true;
		  i_out++;
		}
	      integrate (t_out);
	      if (integration_error)
		{
		  retval = ODESSA_result (x_out, x_s_out);
		  return retval;
		}
	      if (save_output)
		{
		  for (int i = 0; i < n; i++)
		    {
		      x_out(i_out-1,i) = y(i,0);

		      for (int k = 0; k < npar; k++)
			{
			  x_s_out(k)(i_out-1,i) = y(i,k+1);
			}
		    }
		}

	      if (do_restart)
		force_restart ();
	    }

	  retval = ODESSA_result (x_out, x_s_out);
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
