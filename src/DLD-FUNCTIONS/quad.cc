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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <string>

#include <iostream.h>

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

#if defined (quad)
#undef quad
#endif

// Global pointer for user defined function required by quadrature functions.
static octave_function *quad_fcn;

static Quad_options quad_opts;

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
      octave_value_list tmp = quad_fcn->do_index_op (1, args);

      if (error_state)
	{
	  quad_integration_error = 1;  // XXX FIXME XXX
	  gripe_user_supplied_eval ("quad");
	  return retval;
	}

      if (tmp.length () && tmp(0).is_defined ())
	{
	  retval = tmp(0).double_value ();

	  if (error_state)
	    {
	      quad_integration_error = 1;  // XXX FIXME XXX
	      gripe_user_supplied_eval ("quad");
	    }
	}
      else
	{
	  quad_integration_error = 1;  // XXX FIXME XXX
	  gripe_user_supplied_eval ("quad");
	}
    }

  return retval;
}

#define QUAD_ABORT() \
  do \
    { \
      unwind_protect::run_frame ("Fquad"); \
      return retval; \
    } \
  while (0)

#define QUAD_ABORT1(msg) \
  do \
    { \
      ::error ("quad: " ## msg); \
      QUAD_ABORT (); \
    } \
  while (0)

#define QUAD_ABORT2(fmt, arg) \
  do \
    { \
      ::error ("quad: " ## fmt, arg); \
      QUAD_ABORT (); \
    } \
  while (0)

DEFUN_DLD (quad, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {[@var{v}, @var{ier}, @var{nfun}, @var{err}] =} quad (@var{f}, @var{a}, @var{b}, @var{tol}, @var{sing})\n\
Integrate a nonlinear function of one variable using Quadpack.\n\
The first argument is the name of the  function to call to compute the\n\
value of the integrand.  It must have the form\n\
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
@end deftypefn")
{
  octave_value_list retval;

  unwind_protect::begin_frame ("Fquad");

  unwind_protect_int (call_depth);
  call_depth++;

  if (call_depth > 1)
    QUAD_ABORT1 ("invalid recursive call");

  int nargin = args.length ();

  if (nargin > 2 && nargin < 6 && nargout < 5)
    {
      quad_fcn = extract_function (args(0), "quad", "__quad_fcn__",
				   "function y = __quad_fcn__ (x) y = ",
				   "; endfunction");
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

      int ier = 0;
      int nfun = 0;
      double abserr = 0.0;
      double val = 0.0;
      double abstol = 1e-6;
      double reltol = 1e-6;
      ColumnVector tol (2);
      ColumnVector sing;
      int have_sing = 0;
      switch (nargin)
	{
	case 5:
	  if (indefinite)
	    QUAD_ABORT1 ("singularities not allowed on infinite intervals");

	  have_sing = 1;

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
	      reltol = tol (1);

	    case 1:
	      abstol = tol (0);
	      break;

	    default:
	      QUAD_ABORT1 ("expecting tol to contain no more than two values");
	    }

	case 3:
	  if (indefinite)
	    {
	      IndefQuad iq (quad_user_function, bound, indef_type, abstol, reltol);
	      iq.set_options (quad_opts);
	      val = iq.integrate (ier, nfun, abserr);
	    }
	  else
	    {
	      if (have_sing)
		{
		  DefQuad dq (quad_user_function, a, b, sing, abstol, reltol);
		  dq.set_options (quad_opts);
		  val = dq.integrate (ier, nfun, abserr);
		}
	      else
		{
		  DefQuad dq (quad_user_function, a, b, abstol, reltol);
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
    }
  else
    print_usage ("quad");

  unwind_protect::run_frame ("Fquad");

  return retval;
}

typedef void (Quad_options::*d_set_opt_mf) (double);
typedef double (Quad_options::*d_get_opt_mf) (void);

#define MAX_TOKENS 2

struct QUAD_OPTIONS
{
  const char *keyword;
  const char *kw_tok[MAX_TOKENS + 1];
  int min_len[MAX_TOKENS + 1];
  int min_toks_to_match;
  d_set_opt_mf d_set_fcn;
  d_get_opt_mf d_get_fcn;
};

static QUAD_OPTIONS quad_option_table [] =
{
  { "absolute tolerance",
    { "absolute", "tolerance", 0, },
    { 1, 0, 0, }, 1,
    &Quad_options::set_absolute_tolerance,
    &Quad_options::absolute_tolerance, },

  { "relative tolerance",
    { "relative", "tolerance", 0, },
    { 1, 0, 0, }, 1,
    &Quad_options::set_relative_tolerance,
    &Quad_options::relative_tolerance, },

  { 0,
    { 0, 0, 0, },
    { 0, 0, 0, }, 0,
    0, 0, },
};

static void
print_quad_option_list (ostream& os)
{
  print_usage ("quad_options", 1);

  os << "\n"
     << "Options for quad include:\n\n"
     << "  keyword                                  value\n"
     << "  -------                                  -----\n\n";

  QUAD_OPTIONS *list = quad_option_table;

  const char *keyword;
  while ((keyword = list->keyword) != 0)
    {
      os.form ("  %-40s ", keyword);

      double val = (quad_opts.*list->d_get_fcn) ();
      if (val < 0.0)
	os << "computed automatically";
      else
	os << val;

      os << "\n";
      list++;
    }

  os << "\n";
}

static void
set_quad_option (const string& keyword, double val)
{
  QUAD_OPTIONS *list = quad_option_table;

  while (list->keyword != 0)
    {
      if (keyword_almost_match (list->kw_tok, list->min_len, keyword,
				list->min_toks_to_match, MAX_TOKENS))
	{
	  (quad_opts.*list->d_set_fcn) (val);

	  return;
	}
      list++;
    }

  warning ("quad_options: no match for `%s'", keyword.c_str ());
}

static octave_value_list
show_quad_option (const string& keyword)
{
  octave_value retval;

  QUAD_OPTIONS *list = quad_option_table;

  while (list->keyword != 0)
    {
      if (keyword_almost_match (list->kw_tok, list->min_len, keyword,
				list->min_toks_to_match, MAX_TOKENS))
	{
	  return (quad_opts.*list->d_get_fcn) ();
	}
      list++;
    }

  warning ("quad_options: no match for `%s'", keyword.c_str ());

  return retval;
}

DEFUN_DLD (quad_options, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {} quad_options (@var{opt}, @var{val})\n\
When called with two arguments, this function allows you set options\n\
parameters for the function @code{quad}.  Given one argument,\n\
@code{quad_options} returns the value of the corresponding option.  If\n\
no arguments are supplied, the names of all the available options and\n\
their current values are displayed.\n\
@end deftypefn")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin == 0)
    {
      print_quad_option_list (octave_stdout);
      return retval;
    }
  else if (nargin == 1 || nargin == 2)
    {
      string keyword = args(0).string_value ();

      if (! error_state)
	{
	  if (nargin == 1)
	    return show_quad_option (keyword);
	  else
	    {
	      double val = args(1).double_value ();

	      if (! error_state)
		{
		  set_quad_option (keyword, val);
		  return retval;
		}
	    }
	}
    }

  print_usage ("quad_options");

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
