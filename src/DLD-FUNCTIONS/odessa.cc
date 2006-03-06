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
Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <string>

#include <iomanip>
#include <iostream>

#include "ODESSA.h"
#include "lo-mappers.h"

#include "defun-dld.h"
#include "error.h"
#include "gripes.h"
#include "oct-obj.h"
#include "ov-fcn.h"
#include "pager.h"
#include "pr-output.h"
#include "unwind-prot.h"
#include "utils.h"
#include "variables.h"
#include "parse.h"

#include "ODESSA-opts.cc"

// Global pointer for user defined function required by odessa.
static octave_function *odessa_f;
static octave_function *odessa_j;
static octave_function *odessa_b;

// Have we warned about imaginary values returned from user function?
static bool warned_fcn_imaginary = false;
static bool warned_jac_imaginary = false;
static bool warned_b_imaginary = false;

// Is this a recursive call?
static int call_depth = 0;

static ColumnVector
odessa_user_f (const ColumnVector& x, double t, const ColumnVector& theta)
{
  ColumnVector retval;

  octave_value_list args;

  int n = x.length ();
  int npar = theta.length ();

  if (npar > 1)
    args(2) = theta;
  else if (npar == 1)
    args(2) = theta(0);
  else
    args(2) = Matrix ();

  args(1) = t;

  if (n > 1)
    args(0) = x;
  else if (n == 1)
    args(0) = x(0);
  else
    args(0) = Matrix ();

  if (odessa_f)
    {
      octave_value_list tmp = odessa_f->do_multi_index_op (1, args);

      if (error_state)
	{
	  gripe_user_supplied_eval ("odessa");
	  return retval;
	}

      if (tmp.length () > 0 && tmp(0).is_defined ())
	{
	  if (! warned_fcn_imaginary && tmp(0).is_complex_type ())
	    {
	      warning ("odessa: ignoring imaginary part returned from user-supplied function");
	      warned_fcn_imaginary = true;
	    }

	  retval = ColumnVector (tmp(0).vector_value ());

	  if (error_state || retval.length () == 0)
	    gripe_user_supplied_eval ("odessa");
	}
      else
	gripe_user_supplied_eval ("odessa");
    }

  return retval;
}

static Matrix
odessa_user_j (const ColumnVector& x, double t, const ColumnVector& theta)
{
  Matrix retval;

  if (odessa_j)
    {
      octave_value_list args;

      int n = x.length ();
      int npar = theta.length ();

      if (npar > 1)
	args(2) = theta;
      else if (npar == 1)
	args(2) = theta(0);
      else
	args(2) = Matrix ();

      args(1) = t;

      if (n > 1)
	args(0) = x;
      else if (n == 1)
	args(0) = x(0);
      else
	args(0) = Matrix ();

      octave_value_list tmp = odessa_j->do_multi_index_op (1, args);

      if (error_state)
	{
	  gripe_user_supplied_eval ("odessa");
	  return retval;
	}

      if (tmp.length () > 0 && tmp(0).is_defined ())
	{
	  if (! warned_jac_imaginary && tmp(0).is_complex_type ())
	    {
	      warning ("odessa: ignoring imaginary part returned from user-supplied jacobian function");
	      warned_jac_imaginary = true;
	    }

	  retval = tmp(0).matrix_value ();

	  if (error_state || retval.length () == 0)
	    gripe_user_supplied_eval ("odessa");
	}
      else
	gripe_user_supplied_eval ("odessa");
    }

  return retval;
}

static ColumnVector
odessa_user_b (const ColumnVector& x, double t, 
	       const ColumnVector& theta, int column)
{
  ColumnVector retval;

  if (odessa_b)
    {
      octave_value_list args;

      int n = x.length ();
      int npar = theta.length ();

      args(3) = static_cast<double> (column);

      if (npar > 1)
	args(2) = theta;
      else if (npar == 1)
	args(2) = theta(0);
      else
	args(2) = Matrix ();

      args(1) = t;

      if (n > 1)
	args(0) = x;
      else if (n == 1)
	args(0) = x(0);
      else
	args(0) = Matrix ();

      octave_value_list tmp = odessa_b->do_multi_index_op (1, args);

      if (error_state)
	{
	  gripe_user_supplied_eval ("odessa");
	  return retval;
	}

      if (tmp.length () > 0 && tmp(0).is_defined ())
	{
	  if (! warned_b_imaginary && tmp(0).is_complex_type ())
	    {
	      warning ("odessa: ignoring imaginary part returned from user-supplied inhomogeneity function");
	      warned_b_imaginary = true;
	    }

	  retval = ColumnVector (tmp(0).vector_value ());

	  if (error_state || retval.length () == 0)
	    gripe_user_supplied_eval ("odessa");
	}
      else
	gripe_user_supplied_eval ("odessa");
    }

  return retval;
}

static octave_value
make_list (const Array<Matrix>& m_array)
{
  octave_value_list retval;

  int len = m_array.length ();

  retval.resize (len);

  for (int i = 0; i < len; i++)
    retval(i) = m_array(i);

  return octave_value (retval);
}

#define ODESSA_ABORT() \
  do \
    { \
      unwind_protect::run_frame ("Fodessa"); \
      return retval; \
    } \
  while (0)
 
#define ODESSA_ABORT1(msg) \
  do \
    { \
      ::error ("odessa: " msg); \
      ODESSA_ABORT (); \
    } \
  while (0)

#define ODESSA_ABORT2(fmt, arg) \
  do \
    { \
      ::error ("odessa: " fmt, arg); \
      ODESSA_ABORT (); \
    } \
  while (0)

DEFUN_DLD (odessa, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {[@var{x}, @var{sx}, @var{istate}, @var{msg}]} odessa (@var{fcn}, @var{x_0}, @var{p}, @var{sx_0}, @var{t}, @var{t_crit})\n\
Solve the set of differential equations\n\
@tex\n\
$$ {dx \\over dt} = f (x, t; p) $$\n\
with\n\
$$ x(t_0) = x_0 $$\n\
@end tex\n\
@ifinfo\n\
\n\
@example\n\
dx\n\
-- = f(x, t; p)\n\
dt\n\
@end example\n\
\n\
with\n\
\n\
@example\n\
x(t_0) = x_0\n\
@end example\n\
\n\
@end ifinfo\n\
and simultaneously compute the first-order sensitivity coefficients\n\
given by\n\
\n\
@example\n\
s'(t) = j(t)*s(t) + df/dp\n\
@end example\n\
\n\
in which\n\
\n\
@example\n\
s(t)  = dx(t)/dp        (sensitivity functions)\n\
s'(t) = d(dx(t)/dp)/dt\n\
j(t)  = df(x,t;p)/dx(t) (Jacobian matrix)\n\
df/dp = df(x,t;p)/dp    (inhomogeneity matrix)\n\
@end example\n\
\n\
The solution is returned in the matrix @var{x}, with each row\n\
corresponding to an element of the vector @var{t}.  The first element\n\
of @var{t} should be @math{t_0} and should correspond to the initial\n\
state of the system @var{x_0}, so that the first row of the output\n\
is @var{x_0}.\n\
\n\
The sensitivities are returned in a list of matrices, @var{sx},\n\
with each element of the list corresponding to an element of the\n\
vector @var{t}.\n\
\n\
The first argument, @var{fcn}, is a string that names the function to\n\
call to compute the vector of right hand sides for the set of equations.\n\
The function must have the form\n\
\n\
@example\n\
@var{xdot} = f (@var{x}, @var{t}, @var{p})\n\
@end example\n\
\n\
@noindent\n\
in which @var{xdot} and @var{x} are vectors and @var{t} is a scalar.\n\
\n\
The variable @var{p} is a vector of parameters.\n\
\n\
The @var{fcn} argument may also be an array of strings\n\
\n\
@example\n\
[\"f\"; \"j\"; \"b\"]\n\
@end example\n\
\n\
in which the first element names the function @math{f} described\n\
above, the second element names a function to compute the Jacobian\n\
of @math{f}, and the third element names a function to compute the\n\
inhomogeneity matrix.\n\
\n\
The Jacobian function must have the form\n\
\n\
@example\n\
@var{jac} = j (@var{x}, @var{t}, @var{p})\n\
@end example\n\
\n\
in which @var{x}, @var{t}, and @var{p} have the same meanings as\n\
above for the function @var{f}, and  @var{jac} is the matrix of\n\
partial derivatives\n\
@tex\n\
$$ J = {\\partial f_i \\over \\partial x_j} $$\n\
@end tex\n\
@ifinfo\n\
\n\
@example\n\
      df_i\n\
jac = ----\n\
      dx_j\n\
@end example\n\
\n\
@end ifinfo\n\
\n\
The function @var{b} must have the form\n\
\n\
@example\n\
@var{dfdp} = b (@var{x}, @var{t}, @var{p}, @var{c})\n\
@end example\n\
\n\
in which @var{x}, @var{t}, and @var{p} have the same meanings as\n\
above for the function @var{f}, @var{c} indicates which partial\n\
derivatives to return in @var{dfdp}.  For example, if @var{c} is 2,\n\
you should return the vector\n\
\n\
@example\n\
       df_i\n\
dfdp = ----,    i = 1:length(x)\n\
       dp_2\n\
@end example\n\
\n\
The second argument, @var{x_0}, specifies the intial state of the system.\n\
\n\
The third argument, @var{p}, specifies the set of parameters.\n\
\n\
The fourth argument, @var{sx_0} specifies the initial values of the\n\
sensitivities.\n\
\n\
The sixth argument is optional, and may be used to specify a set of\n\
times that the ODE solver should not integrate past.  It is useful for\n\
avoiding difficulties with singularities and points where there is a\n\
discontinuity in the derivative.\n\
\n\
After a successful computation, the value of @var{istate} will be 2\n\
(consistent with the Fortran version of @sc{Odessa}).\n\
\n\
If the computation is not successful, @var{istate} will be something\n\
other than 2 and @var{msg} will contain additional information.\n\
\n\
You can use the function @code{odessa_options} to set optional\n\
parameters for @code{odessa}.\n\
@seealso{daspk, dassl, dasrt, lsode}\n\
@end deftypefn")
{
  octave_value_list retval;

  warned_fcn_imaginary = false;
  warned_jac_imaginary = false;
  warned_b_imaginary = false;

  unwind_protect::begin_frame ("Fodessa");

  unwind_protect_int (call_depth);
  call_depth++;

  if (call_depth > 1)
    ODESSA_ABORT1 ("invalid recursive call");

  int nargin = args.length ();

  if (nargin < 5 || nargin > 6)
    {
      print_usage ("odessa");
      unwind_protect::run_frame ("Fodessa");
      return retval;
    }

  odessa_f = 0;
  odessa_j = 0;
  odessa_b = 0;

  octave_value f_arg = args(0);

  int nr = f_arg.rows ();

  if (nr == 1)
    {
      odessa_f = extract_function
	(f_arg, "odessa", "__odessa_fcn__",
	 "function xdot = __odessa_fcn__ (x, t, p) xdot = ",
	 "; endfunction");
    }
  else if (nr == 2 || nr == 3)
    {
      string_vector tmp = f_arg.all_strings ();

      if (! error_state)
	{
	  odessa_f = extract_function
	    (tmp(0), "odessa", "__odessa_fcn__",
	     "function xdot = __odessa_fcn__ (x, t, p) xdot = ",
	     "; endfunction");

	  if (odessa_f)
	    {
	      odessa_j = extract_function
		(tmp(1), "odessa", "__odessa_jac__",
		 "function xdot = __odessa_jac__ (x, t, p) jac = ",
		 "; endfunction");

	      if (odessa_j && nr == 3)
		{
		  odessa_b = extract_function
		    (tmp(2), "odessa", "__odessa_b__",
		     "function dfdp = __odessa_b__ (x, t, p, c) dfdp = ",
		     "; endfunction");

		  if (! odessa_b)
		    odessa_j = 0;
		}

	      if (! odessa_j)
		odessa_f = 0;
	    }
	}
    }

  if (error_state || ! odessa_f)
    ODESSA_ABORT1
      ("expecting function name as argument 1");
      
  ColumnVector state (args(1).vector_value ());

  if (error_state)
    ODESSA_ABORT1 ("expecting state vector as argument 2");

  bool have_parameters = false;
  
  ColumnVector theta;
  Matrix sensitivity_guess;

  if (nargin == 5 || nargin == 6)
    {
      octave_value theta_arg = args(2);

      if (! theta_arg.is_empty ())
	{
	  theta = ColumnVector (theta_arg.vector_value ());
	  
	  if (error_state)
	    ODESSA_ABORT1
	      ("expecting parameter vector as argument 3");
	}

      have_parameters = (theta.length () > 0);
      
      if (have_parameters)
	{
	  sensitivity_guess = args(3).matrix_value ();
	  
	  if (error_state)
	    ODESSA_ABORT1
	      ("expecting sensitivity guess as argument 4");
	  
	  if (sensitivity_guess.rows () != state.length ()
	      || sensitivity_guess.columns () != theta.length ())
	    ODESSA_ABORT1
	      ("incorrect dimension for sensitivity guess");
	}
    }

      ColumnVector out_times (args(4).vector_value ());

      if (error_state)
	ODESSA_ABORT1
	  ("expecting output time vector as %s argument 5");

      ColumnVector crit_times;

      bool crit_times_set = false;

      if (nargin == 6)
	{
	  crit_times = ColumnVector (args(5).vector_value ());

	  if (error_state)
	    ODESSA_ABORT1
	      ("expecting critical time vector as argument 6");

	  crit_times_set = true;
	}

      ODESFunc func (odessa_user_f);

      if (odessa_j)
	func.set_jsub_function (odessa_user_j);

      if (odessa_b)
	func.set_bsub_function (odessa_user_b);

      double tzero = out_times (0);

      ODESSA_result output;

      ODESSA ode = have_parameters
	? ODESSA (state, theta, sensitivity_guess, tzero, func)
	: ODESSA (state, tzero, func);
	  
      ode.set_options (odessa_opts);

      if (crit_times_set)
	output = ode.integrate (out_times, crit_times);
      else
	output = ode.integrate (out_times);

      if (! error_state)
	{
	  int k = have_parameters ? 3 : 2;

	  std::string msg = ode.error_message ();

	  retval(k--) = msg;
	  retval(k--) = static_cast<double> (ode.integration_state ());

	  if (ode.integration_ok ())
	    {
	      if (have_parameters)
		retval(1) = make_list (output.state_sensitivity ());

	      retval(0) = output.state ();
	    }
	  else
	    {
	      if (have_parameters)
		retval(1) = Matrix ();

	      retval(0) = Matrix ();

	      if ((have_parameters && nargout < 3) || nargout < 2)
		error ("odessa: %s", msg.c_str ());
	    }
	}

  unwind_protect::run_frame ("Fodessa");

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
