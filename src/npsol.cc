// f-npsol.cc                                           -*- C++ -*-
/*

Copyright (C) 1993, 1994 John W. Eaton

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
#include "config.h"
#endif

#ifndef NPSOL_MISSING

#include <strstream.h>

#include "NPSOL.h"

#include "tree-const.h"
#include "variables.h"
#include "builtins.h"
#include "gripes.h"
#include "error.h"
#include "pager.h"
#include "utils.h"
#include "f-npsol.h"

// Global pointers for user defined functions required by npsol.
static tree_fvc *npsol_objective;
static tree_fvc *npsol_constraints;

#ifdef WITH_DLD
Octave_object
builtin_npsol_2 (const Octave_object& args, int nargout)
{
  return npsol (args, nargout);
}

Octave_object
builtin_npsol_options_2 (const Octave_object& args, int nargout)
{
  return npsol_options (args, nargout);
}
#endif

static NPSOL_options npsol_opts;

double
npsol_objective_function (const ColumnVector& x)
{
  int n = x.capacity ();

  tree_constant decision_vars;
  if (n > 1)
    {
      Matrix m (n, 1);
      for (int i = 0; i < n; i++)
	m (i, 0) = x.elem (i);
      decision_vars = m;
    }
  else
    {
      double d = x.elem (0);
      decision_vars = d;
    }

//  tree_constant name = npsol_objective->name ();
  Octave_object args (2);
//  args(0) = name;
  args(1) = decision_vars;

  static double retval;
  retval = 0.0;

  tree_constant objective_value;
  if (npsol_objective != (tree_fvc *) NULL)
    {
      Octave_object tmp = npsol_objective->eval (0, 1, args);

      if (error_state)
	{
	  error ("npsol: error evaluating objective function");
	  npsol_objective_error = 1; // XXX FIXME XXX
	  return retval;
	}

      if (tmp.length () > 0 && tmp(0).is_defined ())
	objective_value = tmp(0);
      else
	{
	  error ("npsol: error evaluating objective function");
	  npsol_objective_error = 1; // XXX FIXME XXX
	  return retval;
	}
    }

  switch (objective_value.const_type ())
    {
    case tree_constant_rep::matrix_constant:
      {
	Matrix m = objective_value.matrix_value ();
	if (m.rows () == 1 && m.columns () == 1)
	  retval = m.elem (0, 0);
	else
	  {
	    gripe_user_returned_invalid ("npsol_objective");
	    npsol_objective_error = 1; // XXX FIXME XXX
	  }
      }
      break;
    case tree_constant_rep::scalar_constant:
      retval = objective_value.double_value ();
      break;
    default:
      gripe_user_returned_invalid ("npsol_objective");
      npsol_objective_error = 1; // XXX FIXME XXX
      break;
    }

  return retval;
}

ColumnVector
npsol_constraint_function (const ColumnVector& x)
{
  ColumnVector retval;

  int n = x.capacity ();

  tree_constant decision_vars;
  if (n > 1)
    {
      Matrix m (n, 1);
      for (int i = 0; i < n; i++)
	m (i, 0) = x.elem (i);
      decision_vars = m;
    }
  else
    {
      double d = x.elem (0);
      decision_vars = d;
    }

//  tree_constant name = npsol_constraints->name ();
  Octave_object args (2);
//  args(0) = name;
  args(1) = decision_vars;

  if (npsol_constraints != (tree_fvc *)NULL)
    {
      Octave_object tmp = npsol_constraints->eval (0, 1, args);

      if (error_state)
	{
	  error ("npsol: error evaluating constraints");
	  return retval;
	}

      if (tmp.length () > 0 && tmp(0).is_defined ())
	{
	  retval = tmp(0).to_vector ();

	  if (retval.length () <= 0)
	    error ("npsol: error evaluating constraints");
	}
      else
	error ("npsol: error evaluating constraints");
    }

  return retval;
}

int
linear_constraints_ok (const ColumnVector& x, const ColumnVector& llb,
		       const Matrix& c, const ColumnVector& lub,
		       char *warn_for, int warn)
{
  int x_len = x.capacity ();
  int llb_len = llb.capacity ();
  int lub_len = lub.capacity ();
  int c_rows = c.rows ();
  int c_cols = c.columns ();

  int ok = 1;
  if (warn)
    {
      if (c_rows == 0 || c_cols == 0 || llb_len == 0 || lub_len == 0)
	{
	  ok = 0;
	  error ("%s: linear constraints must have nonzero dimensions",
		 warn_for);
	}
      else if (x_len != c_cols || llb_len != lub_len || llb_len != c_rows)
	{
	  ok = 0;
	  error ("%s: linear constraints have inconsistent dimensions",
		 warn_for);
	}
    }

  return ok;
}

int
nonlinear_constraints_ok (const ColumnVector& x, const ColumnVector& nllb,
			  nonlinear_fcn g, const ColumnVector& nlub,
			  char *warn_for, int warn)
{
  int nllb_len = nllb.capacity ();
  int nlub_len = nlub.capacity ();
  ColumnVector c = (*g) (x);
  int c_len = c.capacity ();

  int ok = 1;
  if (warn)
    {
      if (nllb_len == 0 || nlub_len == 0 || c_len == 0)
	{
	  ok = 0;
	  error ("%s: nonlinear constraints have nonzero dimensions",
		 warn_for);
	}
      else if (nllb_len != nlub_len || nllb_len != c_len)
	{
	  ok = 0;
	  error ("%s: nonlinear constraints have inconsistent dimensions",
		 warn_for);
	}
    }
  return ok;
}

Octave_object
npsol (const Octave_object& args, int nargout)
{
/*

Handle all of the following:

  1. npsol (x, phi)
  2. npsol (x, phi, lb, ub)
  3. npsol (x, phi, lb, ub, llb, c, lub)
  4. npsol (x, phi, lb, ub, llb, c, lub, nllb, g, nlub)
  5. npsol (x, phi, lb, ub,              nllb, g, nlub)
  6. npsol (x, phi,         llb, c, lub, nllb, g, nlub)
  7. npsol (x, phi,         llb, c, lub)
  8. npsol (x, phi,                      nllb, g, nlub)

*/

// Assumes that we have been given the correct number of arguments.

  Octave_object retval;

  int nargin = args.length ();

  ColumnVector x = args(1).to_vector ();

  if (x.capacity () == 0)
    {
      error ("npsol: expecting vector as first argument");
      return retval;
    }

  npsol_objective = is_valid_function (args(2), "npsol", 1);
  if (npsol_objective == (tree_fvc *) NULL
      || takes_correct_nargs (npsol_objective, 2, "npsol", 1) != 1)
    return retval;

  Objective func (npsol_objective_function);

  ColumnVector soln;

  Bounds bounds;
  if (nargin == 5 || nargin == 8 || nargin == 11)
    {
      ColumnVector lb = args(3).to_vector ();
      ColumnVector ub = args(4).to_vector ();

      int lb_len = lb.capacity ();
      int ub_len = ub.capacity ();
      if (lb_len != ub_len || lb_len != x.capacity ())
	{
	  error ("npsol: lower and upper bounds and decision variable vector");
	  error ("must all have the same number of elements");
	  return retval;
	}

      bounds.resize (lb_len);
      bounds.set_lower_bounds (lb);
      bounds.set_upper_bounds (ub);
    }

  double objf;
  ColumnVector lambda;
  int inform;

  if (nargin == 3)
    {
      // 1. npsol (x, phi)

      NPSOL nlp (x, func);
      nlp.copy (npsol_opts);
      soln = nlp.minimize (objf, inform, lambda);

      goto solved;
    }

  if (nargin == 5)
    {
      // 2. npsol (x, phi, lb, ub)

      NPSOL nlp (x, func, bounds);
      nlp.copy (npsol_opts);
      soln = nlp.minimize (objf, inform, lambda);

      goto solved;
    }

  npsol_constraints = (tree_fvc *) NULL;
  if (nargin == 6 || nargin == 8 || nargin == 9 || nargin == 11)
    npsol_constraints = is_valid_function (args(nargin-2), "npsol", 0);

  if (nargin == 8 || nargin == 6)
    {
      if (npsol_constraints == (tree_fvc *) NULL)
	{
	  ColumnVector lub = args(nargin-1).to_vector ();
	  Matrix c = args(nargin-2).to_matrix ();
	  ColumnVector llb = args(nargin-3).to_vector ();

	  if (llb.capacity () == 0 || lub.capacity () == 0)
	    {
	      error ("npsol: bounds for linear constraints must be vectors");
	      return retval;
	    }

	  if (! linear_constraints_ok (x, llb, c, lub, "npsol", 1))
	    return retval;

	  LinConst linear_constraints (llb, c, lub);

	  if (nargin == 6)
	    {
	      // 7. npsol (x, phi, llb, c, lub)

	      NPSOL nlp (x, func, linear_constraints);
	      nlp.copy (npsol_opts);
	      soln = nlp.minimize (objf, inform, lambda);
	    }
	  else
	    {
	      // 3. npsol (x, phi, lb, ub, llb, c, lub)

	      NPSOL nlp (x, func, bounds, linear_constraints);
	      nlp.copy (npsol_opts);
	      soln = nlp.minimize (objf, inform, lambda);
	    }
	  goto solved;
	}
      else
	{
	  if (takes_correct_nargs (npsol_constraints, 2, "npsol", 1))
	    {
	      ColumnVector nlub = args(nargin-1).to_vector ();
	      ColumnVector nllb = args(nargin-3).to_vector ();

	      NLFunc const_func (npsol_constraint_function);

	      if (! nonlinear_constraints_ok
		  (x, nllb, npsol_constraint_function, nlub, "npsol", 1))
		return retval;

	      NLConst nonlinear_constraints (nllb, const_func, nlub);

	      if (nargin == 6)
		{
		  // 8. npsol (x, phi, nllb, g, nlub)

		  NPSOL nlp (x, func, nonlinear_constraints);
		  nlp.copy (npsol_opts);
		  soln = nlp.minimize (objf, inform, lambda);
		}
	      else
		{
		  // 5. npsol (x, phi, lb, ub, nllb, g, nlub)

		  NPSOL nlp (x, func, bounds, nonlinear_constraints);
		  nlp.copy (npsol_opts);
		  soln = nlp.minimize (objf, inform, lambda);
		}
	      goto solved;
	    }
	}
    }

  if (nargin == 9 || nargin == 11)
    {
      if (npsol_constraints == (tree_fvc *) NULL)
	{
	  // Produce error message.
	  is_valid_function (args(nargin-2), "npsol", 1);
	}
      else
	{
	  if (takes_correct_nargs (npsol_constraints, 2, "npsol", 1))
	    {
	      ColumnVector nlub = args(nargin-1).to_vector ();
	      ColumnVector nllb = args(nargin-3).to_vector ();

	      NLFunc const_func (npsol_constraint_function);

	      if (! nonlinear_constraints_ok
		  (x, nllb, npsol_constraint_function, nlub, "npsol", 1))
		return retval;

	      NLConst nonlinear_constraints (nllb, const_func, nlub);

	      ColumnVector lub = args(nargin-4).to_vector ();
	      Matrix c = args(nargin-5).to_matrix ();
	      ColumnVector llb = args(nargin-6).to_vector ();

	      if (llb.capacity () == 0 || lub.capacity () == 0)
		{
		  error ("npsol: bounds for linear constraints must be vectors");
		  return retval;
		}

	      if (! linear_constraints_ok (x, llb, c, lub, "npsol", 1))
		return retval;

	      LinConst linear_constraints (llb, c, lub);

	      if (nargin == 9)
		{
		  // 6. npsol (x, phi, llb, c, lub, nllb, g, nlub)

		  NPSOL nlp (x, func, linear_constraints,
			     nonlinear_constraints);
		  nlp.copy (npsol_opts);
		  soln = nlp.minimize (objf, inform, lambda);
		}
	      else
		{
		  // 4. npsol (x, phi, lb, ub, llb, c, lub, nllb, g, nlub)

		  NPSOL nlp (x, func, bounds, linear_constraints,
			     nonlinear_constraints);
		  nlp.copy (npsol_opts);
		  soln = nlp.minimize (objf, inform, lambda);
		}
	      goto solved;
	    }
	}
    }

  return retval;

 solved:

  retval.resize (nargout ? nargout : 1);
  retval(0) = soln, 1;
  if (nargout > 1)
    retval(1) = objf;
  if (nargout > 2)
    retval(2) = (double) inform;
  if (nargout > 3)
    retval(3) = lambda;

  return retval;
}

typedef void (NPSOL_options::*d_set_opt_mf) (double);
typedef void (NPSOL_options::*i_set_opt_mf) (int);
typedef double (NPSOL_options::*d_get_opt_mf) (void);
typedef int (NPSOL_options::*i_get_opt_mf) (void);

#define MAX_TOKENS 5

struct NPSOL_OPTIONS
{
  char *keyword;
  char *kw_tok[MAX_TOKENS + 1];
  int min_len[MAX_TOKENS + 1];
  int min_toks_to_match;
  d_set_opt_mf d_set_fcn;
  i_set_opt_mf i_set_fcn;
  d_get_opt_mf d_get_fcn;
  i_get_opt_mf i_get_fcn;
};

static NPSOL_OPTIONS npsol_option_table [] =
{
  { "central difference interval",
    { "central", "difference", "interval", NULL, NULL, NULL, },
    { 2, 0, 0, 0, 0, 0, }, 1,
    NPSOL_options::set_central_difference_interval, NULL,
    NPSOL_options::central_difference_interval, NULL, },

  { "crash tolerance",
    { "crash", "tolerance", NULL, NULL, NULL, NULL, },
    { 2, 0, 0, 0, 0, 0, }, 1,
    NPSOL_options::set_crash_tolerance, NULL,
    NPSOL_options::crash_tolerance, NULL, },

  { "derivative level",
    { "derivative", "level", NULL, NULL, NULL, NULL, },
    { 1, 0, 0, 0, 0, 0, }, 1,
    NULL, NPSOL_options::set_derivative_level,
    NULL, NPSOL_options::derivative_level, },

  { "difference interval",
    { "difference", "interval", NULL, NULL, NULL, NULL, },
    { 3, 0, 0, 0, 0, 0, }, 1,
    NPSOL_options::set_difference_interval, NULL,
    NPSOL_options::difference_interval, NULL, },

  { "function precision",
    { "function", "precision", NULL, NULL, NULL, NULL, },
    { 2, 0, 0, 0, 0, 0, }, 1,
    NPSOL_options::set_function_precision, NULL,
    NPSOL_options::function_precision, NULL, },

  { "infinite bound size",
    { "infinite", "bound", "size", NULL, NULL, NULL, },
    { 1, 1, 0, 0, 0, 0, }, 2,
    NPSOL_options::set_infinite_bound, NULL,
    NPSOL_options::infinite_bound, NULL, },

  { "infinite step size",
    { "infinite", "step", "size", NULL, NULL, NULL, },
    { 1, 1, 0, 0, 0, 0, }, 2,
    NPSOL_options::set_infinite_step, NULL,
    NPSOL_options::infinite_step, NULL, },

  { "linear feasibility tolerance",
    { "linear", "feasibility", "tolerance", NULL, NULL, NULL, },
    { 5, 0, 0, 0, 0, 0, }, 1,
    NPSOL_options::set_linear_feasibility_tolerance, NULL,
    NPSOL_options::linear_feasibility_tolerance, NULL, },

  { "linesearch tolerance",
    { "linesearch", "tolerance", NULL, NULL, NULL, NULL, },
    { 5, 0, 0, 0, 0, 0, }, 1,
    NPSOL_options::set_linesearch_tolerance, NULL,
    NPSOL_options::linesearch_tolerance, NULL, },

  { "major iteration limit",
    { "major", "iteration", "limit", NULL, NULL, NULL, },
    { 2, 1, 0, 0, 0, 0, }, 2,
    NULL, NPSOL_options::set_major_iteration_limit,
    NULL, NPSOL_options::major_iteration_limit, },

  { "minor iteration limit",
    { "minor", "iteration", "limit", NULL, NULL, NULL, },
    { 2, 1, 0, 0, 0, 0, }, 2,
    NULL, NPSOL_options::set_minor_iteration_limit,
    NULL, NPSOL_options::minor_iteration_limit, },

  { "major print level",
    { "major", "print", "level", NULL, NULL, NULL, },
    { 2, 1, 0, 0, 0, 0, }, 2,
    NULL, NPSOL_options::set_major_print_level,
    NULL, NPSOL_options::major_print_level, },

  { "minor print level",
    { "minor", "print", "level", NULL, NULL, NULL, },
    { 2, 1, 0, 0, 0, 0, }, 2,
    NULL, NPSOL_options::set_minor_print_level,
    NULL, NPSOL_options::minor_print_level, },

  { "nonlinear feasibility tolerance",
    { "nonlinear", "feasibility", "tolerance", NULL, NULL, },
    { 1, 0, 0, 0, 0, 0, }, 1,
    NPSOL_options::set_nonlinear_feasibility_tolerance, NULL,
    NPSOL_options::nonlinear_feasibility_tolerance, NULL, },

  { "optimality tolerance",
    { "optimality", "tolerance", NULL, NULL, NULL, NULL, },
    { 1, 0, 0, 0, 0, 0, }, 1,
    NPSOL_options::set_optimality_tolerance, NULL,
    NPSOL_options::optimality_tolerance, NULL, },

  { "start objective check at variable",
    { "start", "objective", "check", "at", "variable", NULL, },
    { 3, 1, 0, 0, 0, 0, }, 2,
    NULL, NPSOL_options::set_start_objective_check,
    NULL, NPSOL_options::start_objective_check, },

  { "start constraint check at variable",
    { "start", "constraint", "check", "at", "variable", NULL, },
    { 3, 1, 0, 0, 0, 0, }, 2,
    NULL, NPSOL_options::set_start_constraint_check,
    NULL, NPSOL_options::start_constraint_check, },

  { "stop objective check at variable",
    { "stop", "objective", "check", "at", "variable", NULL, },
    { 3, 1, 0, 0, 0, 0, }, 2,
    NULL, NPSOL_options::set_stop_objective_check,
    NULL, NPSOL_options::stop_objective_check, },

  { "stop constraint check at variable",
    { "stop", "constraint", "check", "at", "variable", NULL, },
    { 3, 1, 0, 0, 0, 0, }, 2,
    NULL, NPSOL_options::set_stop_constraint_check,
    NULL, NPSOL_options::stop_constraint_check, },

  { "verify level",
    { "verify", "level", NULL, NULL, NULL, NULL, },
    { 1, 0, 0, 0, 0, 0, }, 1,
    NULL, NPSOL_options::set_verify_level,
    NULL, NPSOL_options::verify_level, },

  { NULL,
    { NULL, NULL, NULL, NULL, NULL, NULL, },
    { 0, 0, 0, 0, 0, 0, }, 0,
    NULL, NULL, NULL, NULL, },
};

static void
print_npsol_option_list (void)
{
  ostrstream output_buf;

  print_usage ("npsol_options", 1);

  output_buf << "\n"
	     << "Options for npsol include:\n\n"
	     << "  keyword                                  value\n"
	     << "  -------                                  -----\n\n";

  NPSOL_OPTIONS *list = npsol_option_table;

  char *keyword;
  while ((keyword = list->keyword) != (char *) NULL)
    {
      output_buf.form ("  %-40s ", keyword);
      if (list->d_get_fcn)
	{
	  double val = (npsol_opts.*list->d_get_fcn) ();
	  if (val < 0.0)
	    output_buf << "computed automatically";
	  else
	    output_buf << val;
	}
      else
	{
	  int val = (npsol_opts.*list->i_get_fcn) ();
	  if (val < 0)
	    output_buf << "depends on problem size";
	  else
	    output_buf << val;
	}
      output_buf << "\n";
      list++;
    }

  output_buf << "\n" << ends;
  maybe_page_output (output_buf);
}

static void
do_npsol_option (char *keyword, double val)
{
  NPSOL_OPTIONS *list = npsol_option_table;

  while (list->keyword != (char *) NULL)
    {
      if (keyword_almost_match (list->kw_tok, list->min_len, keyword,
				list->min_toks_to_match, MAX_TOKENS))
	{
	  if (list->d_set_fcn)
	    (npsol_opts.*list->d_set_fcn) (val);
	  else
	    (npsol_opts.*list->i_set_fcn) (NINT (val));

	  return;
	}
      list++;
    }

  warning ("npsol_options: no match for `%s'", keyword);
}

Octave_object
npsol_options (const Octave_object& args, int nargout)
{
  Octave_object retval;

  int nargin = args.length ();

  if (nargin == 1)
    {
      print_npsol_option_list ();
    }
  else if (nargin == 3)
    {
      if (args(1).is_string_type ())
	{
	  char *keyword = args(1).string_value ();
	  double val = args(2).double_value ();
	  do_npsol_option (keyword, val);
	}
      else
	print_usage ("npsol_options");
    }
  else
    {
      print_usage ("npsol_options");
    }

  return retval;
}

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
