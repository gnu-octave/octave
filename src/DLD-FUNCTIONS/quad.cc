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
Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <string>

#include <iomanip>
#include <iostream>

#include "Quad.h"
#include "lo-mappers.h"

#include "defun-dld.h"
#include "error.h"
#include "gripes.h"
#include "pager.h"
#include "oct-obj.h"
#include "ov-fcn.h"
#include "unwind-prot.h"
#include "utils.h"
#include "variables.h"

#include "Quad-opts.cc"

#if defined (quad)
#undef quad
#endif

// Global pointer for user defined function required by quadrature functions.
static octave_function *quad_fcn;

// Have we warned about imaginary values returned from user function?
static bool warned_imaginary = false;

// Is this a recursive call?
static int call_depth = 0;

double
quad_user_function (double x)
{
  double retval = 0.0;

  octave_value_list args;
  args(0) = x;

  if (quad_fcn)
    {
      octave_value_list tmp = quad_fcn->do_multi_index_op (1, args);

      if (error_state)
	{
	  quad_integration_error = 1;  // FIXME
	  gripe_user_supplied_eval ("quad");
	  return retval;
	}

      if (tmp.length () && tmp(0).is_defined ())
	{
	  if (! warned_imaginary && tmp(0).is_complex_type ())
	    {
	      warning ("quad: ignoring imaginary part returned from user-supplied function");
	      warned_imaginary = true;
	    }

	  retval = tmp(0).double_value ();

	  if (error_state)
	    {
	      quad_integration_error = 1;  // FIXME
	      gripe_user_supplied_eval ("quad");
	    }
	}
      else
	{
	  quad_integration_error = 1;  // FIXME
	  gripe_user_supplied_eval ("quad");
	}
    }

  return retval;
}

#define QUAD_ABORT() \
  do \
    { \
      if (fcn_name.length()) \
	clear_function (fcn_name); \
      unwind_protect::run_frame ("Fquad"); \
      return retval; \
    } \
  while (0)

#define QUAD_ABORT1(msg) \
  do \
    { \
      ::error ("quad: " msg); \
      QUAD_ABORT (); \
    } \
  while (0)

#define QUAD_ABORT2(fmt, arg) \
  do \
    { \
      ::error ("quad: " fmt, arg); \
      QUAD_ABORT (); \
    } \
  while (0)

DEFUN_DLD (quad, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {[@var{v}, @var{ier}, @var{nfun}, @var{err}] =} quad (@var{f}, @var{a}, @var{b}, @var{tol}, @var{sing})\n\
Integrate a nonlinear function of one variable using Quadpack.\n\
The first argument is the name of the  function, the function handle or\n\
the inline function to call to compute the value of the integrand.  It\n\
must have the form\n\
\n\
@example\n\
y = f (x)\n\
@end example\n\
\n\
@noindent\n\
where @var{y} and @var{x} are scalars.\n\
\n\
The second and third arguments are limits of integration.  Either or\n\
both may be infinite.\n\
\n\
The optional argument @var{tol} is a vector that specifies the desired\n\
accuracy of the result.  The first element of the vector is the desired\n\
absolute tolerance, and the second element is the desired relative\n\
tolerance.  To choose a relative test only, set the absolute\n\
tolerance to zero.  To choose an absolute test only, set the relative\n\
tolerance to zero. \n\
\n\
The optional argument @var{sing} is a vector of values at which the\n\
integrand is known to be singular.\n\
\n\
The result of the integration is returned in @var{v} and @var{ier}\n\
contains an integer error code (0 indicates a successful integration).\n\
The value of @var{nfun} indicates how many function evaluations were\n\
required, and @var{err} contains an estimate of the error in the\n\
solution.\n\
\n\
You can use the function @code{quad_options} to set optional\n\
parameters for @code{quad}.\n\
@end deftypefn")
{
  octave_value_list retval;

  std::string fcn_name;

  warned_imaginary = false;

  unwind_protect::begin_frame ("Fquad");

  unwind_protect_int (call_depth);
  call_depth++;

  if (call_depth > 1)
    QUAD_ABORT1 ("invalid recursive call");

  int nargin = args.length ();

  if (nargin > 2 && nargin < 6 && nargout < 5)
    {
      if (args(0).is_function_handle () || args(0).is_inline_function ())
	quad_fcn = args(0).function_value ();
      else
	{
	  fcn_name = unique_symbol_name ("__quad_fcn_");
	  std::string fname = "function y = ";
	  fname.append (fcn_name);
	  fname.append ("(x) y = ");
	  quad_fcn = extract_function (args(0), "quad", fcn_name, fname,
				       "; endfunction");
	}

      if (! quad_fcn)
	QUAD_ABORT ();

      double a = args(1).double_value ();

      if (error_state)
	QUAD_ABORT1 ("expecting second argument to be a scalar");

      double b = args(2).double_value ();

      if (error_state)
	QUAD_ABORT1 ("expecting third argument to be a scalar");

      int indefinite = 0;
      IndefQuad::IntegralType indef_type = IndefQuad::doubly_infinite;
      double bound = 0.0;
      if (xisinf (a) && xisinf (b))
	{
	  indefinite = 1;
	  indef_type = IndefQuad::doubly_infinite;
	}
      else if (xisinf (a))
	{
	  indefinite = 1;
	  bound = b;
	  indef_type = IndefQuad::neg_inf_to_bound;
	}
      else if (xisinf (b))
	{
	  indefinite = 1;
	  bound = a;
	  indef_type = IndefQuad::bound_to_inf;
	}

      octave_idx_type ier = 0;
      octave_idx_type nfun = 0;
      double abserr = 0.0;
      double val = 0.0;
      bool have_sing = false;
      ColumnVector sing;
      ColumnVector tol;

      switch (nargin)
	{
	case 5:
	  if (indefinite)
	    QUAD_ABORT1 ("singularities not allowed on infinite intervals");

	  have_sing = true;

	  sing = ColumnVector (args(4).vector_value ());

	  if (error_state)
	    QUAD_ABORT1 ("expecting vector of singularities as fourth argument");

	case 4:
	  tol = ColumnVector (args(3).vector_value ());

	  if (error_state)
	    QUAD_ABORT1 ("expecting vector of tolerances as fifth argument");

	  switch (tol.capacity ())
	    {
	    case 2:
	      quad_opts.set_relative_tolerance (tol (1));

	    case 1:
	      quad_opts.set_absolute_tolerance (tol (0));
	      break;

	    default:
	      QUAD_ABORT1 ("expecting tol to contain no more than two values");
	    }

	case 3:
	  if (indefinite)
	    {
	      IndefQuad iq (quad_user_function, bound, indef_type);
	      iq.set_options (quad_opts);
	      val = iq.integrate (ier, nfun, abserr);
	    }
	  else
	    {
	      if (have_sing)
		{
		  DefQuad dq (quad_user_function, a, b, sing);
		  dq.set_options (quad_opts);
		  val = dq.integrate (ier, nfun, abserr);
		}
	      else
		{
		  DefQuad dq (quad_user_function, a, b);
		  dq.set_options (quad_opts);
		  val = dq.integrate (ier, nfun, abserr);
		}
	    }
	  break;

	default:
	  panic_impossible ();
	  break;
	}

      retval(3) = abserr;
      retval(2) = static_cast<double> (nfun);
      retval(1) = static_cast<double> (ier);
      retval(0) = val;

      if (fcn_name.length())
	clear_function (fcn_name);
    }
  else
    print_usage ("quad");

  unwind_protect::run_frame ("Fquad");

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
