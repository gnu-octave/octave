// f-quad.cc                                           -*- C++ -*-
/*

Copyright (C) 1993, 1994, 1995 John W. Eaton

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

#include <strstream.h>

#include "Quad.h"

#include "tree-const.h"
#include "variables.h"
#include "mappers.h"
#include "gripes.h"
#include "error.h"
#include "utils.h"
#include "pager.h"
#include "help.h"
#include "defun-dld.h"

// Global pointer for user defined function required by quadrature functions.
static tree_fvc *quad_fcn;

static Quad_options quad_opts;

double
quad_user_function (double x)
{
  double retval = 0.0;

  Octave_object args;
  args(0) = x;

  if (quad_fcn)
    {
      Octave_object tmp = quad_fcn->eval (0, 1, args);

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

DEFUN_DLD_BUILTIN ("quad", Fquad, Squad, 5, 3,
  "[V, IER, NFUN] = quad (F, A, B [, TOL] [, SING])\n\
\n\
Where the first argument is the name of the  function to call to\n\
compute the value of the integrand.  It must have the form\n\
\n\
  y = f (x)
\n\
where y and x are scalars.\n\
\n\
The second and third arguments are limits of integration.  Either or\n\
both may be infinite.\n\
\n\
The optional argument tol is a vector that specifies the desired\n\
accuracy of the result.  The first element of the vector is the desired\n\
absolute tolerance, and the second element is the desired relative\n\
tolerance.\n\
\n\
The optional argument @var{sing} is a vector of values at which the\n\
integrand is singular.")
{
  Octave_object retval;

  int nargin = args.length ();

  if (nargin < 3 || nargin > 5 || nargout > 4)
    {
      print_usage ("quad");
      return retval;
    }

  quad_fcn = is_valid_function (args(0), "quad", 1);
  if (! quad_fcn || takes_correct_nargs (quad_fcn, 1, "quad", 1) != 1)
    return retval;

  double a = args(1).double_value ();

  if (error_state)
    {
      error ("quad: expecting second argument to be a scalar");
      return retval;
    }

  double b = args(2).double_value ();

  if (error_state)
    {
      error ("quad: expecting third argument to be a scalar");
      return retval;
    }

  int indefinite = 0;
  IndefQuad::IntegralType indef_type = IndefQuad::doubly_infinite;
  double bound = 0.0;
  if ((int) xisinf (a) && (int) xisinf (b))
    {
      indefinite = 1;
      indef_type = IndefQuad::doubly_infinite;
    }
  else if ((int) xisinf (a))
    {
      indefinite = 1;
      bound = b;
      indef_type = IndefQuad::neg_inf_to_bound;
    }
  else if ((int) xisinf (b))
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
  Vector tol (2);
  Vector sing;
  int have_sing = 0;
  switch (nargin)
    {
    case 5:
      if (indefinite)
	{
	  error("quad: singularities not allowed on infinite intervals");
	  return retval;
	}

      have_sing = 1;

      sing = args(4).vector_value ();

      if (error_state)
	{
	  error ("quad: expecting vector of singularities as fourth argument");
	  return retval;
	}

    case 4:
      tol = args(3).vector_value ();

      if (error_state)
	{
	  error ("quad: expecting vector of tolerances as fifth argument");
	  return retval;
	}

      switch (tol.capacity ())
	{
	case 2:
	  reltol = tol.elem (1);

	case 1:
	  abstol = tol.elem (0);
	  break;

	default:
	  error ("quad: expecting tol to contain no more than two values");
	  return retval;
	}

    case 3:
      if (indefinite)
	{
	  IndefQuad iq (quad_user_function, bound, indef_type, abstol, reltol);
	  iq.copy (quad_opts);
	  val = iq.integrate (ier, nfun, abserr);
	}
      else
	{
	  if (have_sing)
	    {
	      DefQuad dq (quad_user_function, a, b, sing, abstol, reltol);
	      dq.copy (quad_opts);
	      val = dq.integrate (ier, nfun, abserr);
	    }
	  else
	    {
	      DefQuad dq (quad_user_function, a, b, abstol, reltol);
	      dq.copy (quad_opts);
	      val = dq.integrate (ier, nfun, abserr);
	    }
	}
      break;

    default:
      panic_impossible ();
      break;
    }

  retval(3) = abserr;
  retval(2) = nfun;
  retval(1) = ier;
  retval(0) = val;

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
    Quad_options::set_absolute_tolerance,
    Quad_options::absolute_tolerance, },

  { "relative tolerance",
    { "relative", "tolerance", 0, },
    { 1, 0, 0, }, 1,
    Quad_options::set_relative_tolerance,
    Quad_options::relative_tolerance, },

  { 0,
    { 0, 0, 0, },
    { 0, 0, 0, }, 0,
    0, 0, },
};

static void
print_quad_option_list (void)
{
  ostrstream output_buf;

  print_usage ("quad_options", 1);

  output_buf << "\n"
	     << "Options for quad include:\n\n"
	     << "  keyword                                  value\n"
	     << "  -------                                  -----\n\n";

  QUAD_OPTIONS *list = quad_option_table;

  const char *keyword;
  while ((keyword = list->keyword) != 0)
    {
      output_buf.form ("  %-40s ", keyword);

      double val = (quad_opts.*list->d_get_fcn) ();
      if (val < 0.0)
	output_buf << "computed automatically";
      else
	output_buf << val;

      output_buf << "\n";
      list++;
    }

  output_buf << "\n" << ends;
  maybe_page_output (output_buf);
}

static void
set_quad_option (char *keyword, double val)
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

  warning ("quad_options: no match for `%s'", keyword);
}

static Octave_object
show_quad_option (char *keyword)
{
  Octave_object retval;

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

  warning ("quad_options: no match for `%s'", keyword);

  return retval;
}

DEFUN_DLD_BUILTIN ("quad_options", Fquad_options, Squad_options, -1, 1,
  "quad_options (KEYWORD, VALUE)\n\
\n\
Set or show options for quad.  Keywords may be abbreviated\n\
to the shortest match.")
{
  Octave_object retval;

  int nargin = args.length ();

  if (nargin == 0)
    {
      print_quad_option_list ();
      return retval;
    }
  else if (nargin == 1 || nargin == 2)
    {
      char *keyword = args(0).string_value ();

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
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
