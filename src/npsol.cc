/*

Copyright (C) 1996 John W. Eaton

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

#include <strstream.h>

#include "NPSOL.h"

#include "defun-dld.h"
#include "error.h"
#include "gripes.h"
#include "help.h"
#include "mappers.h"
#include "oct-obj.h"
#include "pager.h"
#include "utils.h"
#include "variables.h"

#ifndef NPSOL_MISSING

// Global pointers for user defined functions required by npsol.
static tree_fvc *npsol_objective;
static tree_fvc *npsol_constraints;

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

  Octave_object args;
  args(0) = decision_vars;

  static double retval;
  retval = 0.0;

  tree_constant objective_value;
  if (npsol_objective)
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

  if (objective_value.is_real_matrix ())
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
  else if (objective_value.is_real_scalar ())
    {
      retval = objective_value.double_value ();
    }
  else
    {
      gripe_user_returned_invalid ("npsol_objective");
      npsol_objective_error = 1; // XXX FIXME XXX
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

  Octave_object args;
  args(0) = decision_vars;

  if (npsol_constraints)
    {
      Octave_object tmp = npsol_constraints->eval (0, 1, args);

      if (error_state)
	{
	  error ("npsol: error evaluating constraints");
	  return retval;
	}

      if (tmp.length () > 0 && tmp(0).is_defined ())
	{
	  retval = tmp(0).vector_value ();

	  if (error_state || retval.length () <= 0)
	    error ("npsol: error evaluating constraints");
	}
      else
	error ("npsol: error evaluating constraints");
    }

  return retval;
}

static int
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

static int
nonlinear_constraints_ok (const ColumnVector& x, const ColumnVector& nllb,
			  NLFunc::nonlinear_fcn g, const ColumnVector& nlub,
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

#endif

#if defined (NPSOL_MISSING)
DEFUN_DLD_BUILTIN (npsol, , ,
  "This function requires NPSOL, which is not freely\n\
redistributable.  For more information, read the file\n\
libcruft/npsol/README.MISSING in the source distribution.")
#else
DEFUN_DLD_BUILTIN (npsol, args, nargout,
  "[X, OBJ, INFO, LAMBDA] = npsol (X, PHI [, LB, UB] [, A_LB, A, A_UB]\n\
                                [, G_LB, G, G_UB])\n\
\n\
Groups of arguments surrounded in `[]' are optional, but\n\
must appear in the same relative order shown above.\n\
\n\
The second argument is a string containing the name of the objective\n\
function to call.  The objective function must be of the form\n\
\n\
  y = phi (x)\n\
\n\
where x is a vector and y is a scalar.\n\
\n\
The argument G is a string containing the name of the function that
defines the nonlinear constraints.  It must be of the form\n\
\n\
  y = g (x)\n\
\n\
where x is a vector and y is a vector.")
#endif
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

  Octave_object retval;

#if defined (NPSOL_MISSING)

  // Force a bad value of inform, and empty matrices for x, phi, and
  // lambda.

  retval.resize (4, Matrix ());

  retval(2) = -1.0;

  print_usage ("npsol");

#else

  int nargin = args.length ();

  if (nargin < 2 || nargin == 3 || nargin == 6 || nargin == 9
      || nargin > 10 || nargout > 4)
    {
      print_usage ("npsol");
      return retval;
    }

  ColumnVector x = args(0).vector_value ();

  if (error_state || x.capacity () == 0)
    {
      error ("npsol: expecting vector as first argument");
      return retval;
    }

  npsol_objective = is_valid_function (args(1), "npsol", 1);
  if (! npsol_objective)
    return retval;

  Objective func (npsol_objective_function);

  ColumnVector soln;

  Bounds bounds;
  if (nargin == 4 || nargin == 7 || nargin == 10)
    {
      ColumnVector lb = args(2).vector_value ();
      ColumnVector ub = args(3).vector_value ();

      int lb_len = lb.capacity ();
      int ub_len = ub.capacity ();

      if (error_state || lb_len != ub_len || lb_len != x.capacity ())
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

  if (nargin == 2)
    {
      // 1. npsol (x, phi)

      NPSOL nlp (x, func);
      nlp.set_options (npsol_opts);
      soln = nlp.minimize (objf, inform, lambda);

      goto solved;
    }

  if (nargin == 4)
    {
      // 2. npsol (x, phi, lb, ub)

      NPSOL nlp (x, func, bounds);
      nlp.set_options (npsol_opts);
      soln = nlp.minimize (objf, inform, lambda);

      goto solved;
    }

  npsol_constraints = 0;
  if (nargin == 5 || nargin == 7 || nargin == 8 || nargin == 10)
    npsol_constraints = is_valid_function (args(nargin-2), "npsol", 0);

  if (nargin == 7 || nargin == 5)
    {
      if (! npsol_constraints)
	{
	  ColumnVector lub = args(nargin-1).vector_value ();
	  ColumnVector llb = args(nargin-3).vector_value ();

	  if (error_state || llb.capacity () == 0 || lub.capacity () == 0)
	    {
	      error ("npsol: bounds for linear constraints must be vectors");
	      return retval;
	    }

	  Matrix c = args(nargin-2).matrix_value ();

	  if (error_state)
	    {
	      error ("npsol: invalid linear constraint matrix");
	      return retval;
	    }

	  if (! linear_constraints_ok (x, llb, c, lub, "npsol", 1))
	    return retval;

	  LinConst linear_constraints (llb, c, lub);

	  if (nargin == 5)
	    {
	      // 7. npsol (x, phi, llb, c, lub)

	      NPSOL nlp (x, func, linear_constraints);
	      nlp.set_options (npsol_opts);
	      soln = nlp.minimize (objf, inform, lambda);
	    }
	  else
	    {
	      // 3. npsol (x, phi, lb, ub, llb, c, lub)

	      NPSOL nlp (x, func, bounds, linear_constraints);
	      nlp.set_options (npsol_opts);
	      soln = nlp.minimize (objf, inform, lambda);
	    }
	  goto solved;
	}
      else
	{
	  ColumnVector nlub = args(nargin-1).vector_value ();
	  ColumnVector nllb = args(nargin-3).vector_value ();

	  if (error_state
	      || (! nonlinear_constraints_ok
		  (x, nllb, npsol_constraint_function, nlub, "npsol", 1)))
	    return retval;

	  NLFunc const_func (npsol_constraint_function);
	  NLConst nonlinear_constraints (nllb, const_func, nlub);

	  if (nargin == 5)
	    {
	      // 8. npsol (x, phi, nllb, g, nlub)

	      NPSOL nlp (x, func, nonlinear_constraints);
	      nlp.set_options (npsol_opts);
	      soln = nlp.minimize (objf, inform, lambda);
	    }
	  else
	    {
	      // 5. npsol (x, phi, lb, ub, nllb, g, nlub)

	      NPSOL nlp (x, func, bounds, nonlinear_constraints);
	      nlp.set_options (npsol_opts);
	      soln = nlp.minimize (objf, inform, lambda);
	    }
	  goto solved;
	}
    }

  if (nargin == 8 || nargin == 10)
    {
      if (! npsol_constraints)
	{
	  // Produce error message.

	  is_valid_function (args(nargin-2), "npsol", 1);
	}
      else
	{
	  ColumnVector nlub = args(nargin-1).vector_value ();
	  ColumnVector nllb = args(nargin-3).vector_value ();

	  if (error_state
	      || (! nonlinear_constraints_ok
		  (x, nllb, npsol_constraint_function, nlub, "npsol", 1)))
	    return retval;

	  NLFunc const_func (npsol_constraint_function);
	  NLConst nonlinear_constraints (nllb, const_func, nlub);

	  ColumnVector lub = args(nargin-4).vector_value ();
	  ColumnVector llb = args(nargin-6).vector_value ();

	  if (error_state || llb.capacity () == 0 || lub.capacity () == 0)
	    {
	      error ("npsol: bounds for linear constraints must be vectors");
	      return retval;
	    }
	      
	  Matrix c = args(nargin-5).matrix_value ();

	  if (error_state)
	    {
	      error ("npsol: invalid linear constraint matrix");
	      return retval;
	    }

	  if (! linear_constraints_ok (x, llb, c, lub, "npsol", 1))
	    return retval;

	  LinConst linear_constraints (llb, c, lub);

	  if (nargin == 8)
	    {
	      // 6. npsol (x, phi, llb, c, lub, nllb, g, nlub)

	      NPSOL nlp (x, func, linear_constraints,
			 nonlinear_constraints);
	      nlp.set_options (npsol_opts);
	      soln = nlp.minimize (objf, inform, lambda);
	    }
	  else
	    {
	      // 4. npsol (x, phi, lb, ub, llb, c, lub, nllb, g, nlub)

	      NPSOL nlp (x, func, bounds, linear_constraints,
			 nonlinear_constraints);
	      nlp.set_options (npsol_opts);
	      soln = nlp.minimize (objf, inform, lambda);
	    }
	  goto solved;
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

#endif

  return retval;
}

#ifndef NPSOL_MISSING

typedef void (NPSOL_options::*d_set_opt_mf) (double);
typedef void (NPSOL_options::*i_set_opt_mf) (int);
typedef double (NPSOL_options::*d_get_opt_mf) (void);
typedef int (NPSOL_options::*i_get_opt_mf) (void);

#define MAX_TOKENS 5

struct NPSOL_OPTIONS
{
  const char *keyword;
  const char *kw_tok[MAX_TOKENS + 1];
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
    { "central", "difference", "interval", 0, 0, 0, },
    { 2, 0, 0, 0, 0, 0, }, 1,
    NPSOL_options::set_central_difference_interval, 0,
    NPSOL_options::central_difference_interval, 0, },

  { "crash tolerance",
    { "crash", "tolerance", 0, 0, 0, 0, },
    { 2, 0, 0, 0, 0, 0, }, 1,
    NPSOL_options::set_crash_tolerance, 0,
    NPSOL_options::crash_tolerance, 0, },

  { "derivative level",
    { "derivative", "level", 0, 0, 0, 0, },
    { 1, 0, 0, 0, 0, 0, }, 1,
    0, NPSOL_options::set_derivative_level,
    0, NPSOL_options::derivative_level, },

  { "difference interval",
    { "difference", "interval", 0, 0, 0, 0, },
    { 3, 0, 0, 0, 0, 0, }, 1,
    NPSOL_options::set_difference_interval, 0,
    NPSOL_options::difference_interval, 0, },

  { "function precision",
    { "function", "precision", 0, 0, 0, 0, },
    { 2, 0, 0, 0, 0, 0, }, 1,
    NPSOL_options::set_function_precision, 0,
    NPSOL_options::function_precision, 0, },

  { "infinite bound size",
    { "infinite", "bound", "size", 0, 0, 0, },
    { 1, 1, 0, 0, 0, 0, }, 2,
    NPSOL_options::set_infinite_bound, 0,
    NPSOL_options::infinite_bound, 0, },

  { "infinite step size",
    { "infinite", "step", "size", 0, 0, 0, },
    { 1, 1, 0, 0, 0, 0, }, 2,
    NPSOL_options::set_infinite_step, 0,
    NPSOL_options::infinite_step, 0, },

  { "linear feasibility tolerance",
    { "linear", "feasibility", "tolerance", 0, 0, 0, },
    { 5, 0, 0, 0, 0, 0, }, 1,
    NPSOL_options::set_linear_feasibility_tolerance, 0,
    NPSOL_options::linear_feasibility_tolerance, 0, },

  { "linesearch tolerance",
    { "linesearch", "tolerance", 0, 0, 0, 0, },
    { 5, 0, 0, 0, 0, 0, }, 1,
    NPSOL_options::set_linesearch_tolerance, 0,
    NPSOL_options::linesearch_tolerance, 0, },

  { "major iteration limit",
    { "major", "iteration", "limit", 0, 0, 0, },
    { 2, 1, 0, 0, 0, 0, }, 2,
    0, NPSOL_options::set_major_iteration_limit,
    0, NPSOL_options::major_iteration_limit, },

  { "minor iteration limit",
    { "minor", "iteration", "limit", 0, 0, 0, },
    { 2, 1, 0, 0, 0, 0, }, 2,
    0, NPSOL_options::set_minor_iteration_limit,
    0, NPSOL_options::minor_iteration_limit, },

  { "major print level",
    { "major", "print", "level", 0, 0, 0, },
    { 2, 1, 0, 0, 0, 0, }, 2,
    0, NPSOL_options::set_major_print_level,
    0, NPSOL_options::major_print_level, },

  { "minor print level",
    { "minor", "print", "level", 0, 0, 0, },
    { 2, 1, 0, 0, 0, 0, }, 2,
    0, NPSOL_options::set_minor_print_level,
    0, NPSOL_options::minor_print_level, },

  { "nonlinear feasibility tolerance",
    { "nonlinear", "feasibility", "tolerance", 0, 0, },
    { 1, 0, 0, 0, 0, 0, }, 1,
    NPSOL_options::set_nonlinear_feasibility_tolerance, 0,
    NPSOL_options::nonlinear_feasibility_tolerance, 0, },

  { "optimality tolerance",
    { "optimality", "tolerance", 0, 0, 0, 0, },
    { 1, 0, 0, 0, 0, 0, }, 1,
    NPSOL_options::set_optimality_tolerance, 0,
    NPSOL_options::optimality_tolerance, 0, },

  { "start objective check at variable",
    { "start", "objective", "check", "at", "variable", 0, },
    { 3, 1, 0, 0, 0, 0, }, 2,
    0, NPSOL_options::set_start_objective_check,
    0, NPSOL_options::start_objective_check, },

  { "start constraint check at variable",
    { "start", "constraint", "check", "at", "variable", 0, },
    { 3, 1, 0, 0, 0, 0, }, 2,
    0, NPSOL_options::set_start_constraint_check,
    0, NPSOL_options::start_constraint_check, },

  { "stop objective check at variable",
    { "stop", "objective", "check", "at", "variable", 0, },
    { 3, 1, 0, 0, 0, 0, }, 2,
    0, NPSOL_options::set_stop_objective_check,
    0, NPSOL_options::stop_objective_check, },

  { "stop constraint check at variable",
    { "stop", "constraint", "check", "at", "variable", 0, },
    { 3, 1, 0, 0, 0, 0, }, 2,
    0, NPSOL_options::set_stop_constraint_check,
    0, NPSOL_options::stop_constraint_check, },

  { "verify level",
    { "verify", "level", 0, 0, 0, 0, },
    { 1, 0, 0, 0, 0, 0, }, 1,
    0, NPSOL_options::set_verify_level,
    0, NPSOL_options::verify_level, },

  { 0,
    { 0, 0, 0, 0, 0, 0, },
    { 0, 0, 0, 0, 0, 0, }, 0,
    0, 0, 0, 0, },
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

  const char *keyword;
  while ((keyword = list->keyword) != 0)
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
set_npsol_option (const string& keyword, double val)
{
  NPSOL_OPTIONS *list = npsol_option_table;

  while (list->keyword != 0)
    {
      if (keyword_almost_match (list->kw_tok, list->min_len, keyword,
				list->min_toks_to_match, MAX_TOKENS))
	{
	  if (list->d_set_fcn)
	    (npsol_opts.*list->d_set_fcn) (val);
	  else
	    {
	      if (xisnan (val))
		{
		  error ("npsol_options: %s: expecting integer, found NaN",
			 keyword.c_str ());
		}
	      else
		(npsol_opts.*list->i_set_fcn) (NINT (val));
	    }
	  return;
	}
      list++;
    }

  warning ("npsol_options: no match for `%s'", keyword.c_str ());
}

static Octave_object
show_npsol_option (const string& keyword)
{
  Octave_object retval;

  NPSOL_OPTIONS *list = npsol_option_table;

  while (list->keyword != 0)
    {
      if (keyword_almost_match (list->kw_tok, list->min_len, keyword,
				list->min_toks_to_match, MAX_TOKENS))
	{
	  if (list->d_get_fcn)
	    return (npsol_opts.*list->d_get_fcn) ();
	  else
	    return (double) (npsol_opts.*list->i_get_fcn) ();
	}
      list++;
    }

  warning ("npsol_options: no match for `%s'", keyword.c_str ());

  return retval;
}

#endif

#if defined (NPSOL_MISSING)
DEFUN_DLD_BUILTIN (npsol_options, , ,
  "This function requires NPSOL, which is not freely\n\
redistributable.  For more information, read the file\n\
libcruft/npsol/README.MISSING in the source distribution.")
#else
DEFUN_DLD_BUILTIN (npsol_options, args, ,
  "npsol_options (KEYWORD, VALUE)\n\
\n\
Set or show options for npsol.  Keywords may be abbreviated\n\
to the shortest match.")
#endif
{
  Octave_object retval;

#if defined (NPSOL_MISSING)

  print_usage ("npsol_options");

#else

  int nargin = args.length ();

  if (nargin == 0)
    {
      print_npsol_option_list ();
      return retval;
    }
  else if (nargin == 1 || nargin == 2)
    {
      string keyword = args(0).string_value ();

      if (! error_state)
	{
	  if (nargin == 1)
	    return show_npsol_option (keyword);
	  else
	    {
	      double val = args(1).double_value ();

	      if (! error_state)
		{
		  set_npsol_option (keyword, val);
		  return retval;
		}
	    }
	}
    }

  print_usage ("npsol_options");

#endif

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
