// f-npsol.cc                                           -*- C++ -*-
/*

Copyright (C) 1993 John W. Eaton

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

#ifdef __GNUG__
#pragma implementation
#endif

#ifndef NPSOL_MISSING

#include "NPSOL.h"

#include "tree-const.h"
#include "variables.h"
#include "gripes.h"
#include "error.h"
#include "utils.h"
#include "f-npsol.h"

// Global pointers for user defined functions required by npsol.
static tree *npsol_objective;
static tree *npsol_constraints;

#ifdef WITH_DLD
tree_constant *
builtin_npsol_2 (const tree_constant *args, int nargin, int nargout)
{
  return npsol (args, nargin, nargout);
}
#endif

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
      decision_vars = tree_constant (m);
    }
  else
    {
      double d = x.elem (0);
      decision_vars = tree_constant (d);
    }

//  tree_constant name = tree_constant (npsol_objective->name ());
  tree_constant *args = new tree_constant [2];
//  args[0] = name;
  args[1] = decision_vars;

  tree_constant objective_value;
  if (npsol_objective != NULL_TREE)
    {
      tree_constant *tmp = npsol_objective->eval (args, 2, 1, 0);
      delete [] args;
      if (tmp != NULL_TREE_CONST && tmp[0].is_defined ())
	{
	  objective_value = tmp[0];
	  delete [] tmp;
	}
      else
	{
	  delete [] tmp;
	  error ("npsol: error evaluating objective function");
	  jump_to_top_level ();
	}
    }

  static double retval;
  retval = 0.0;

  switch (objective_value.const_type ())
    {
    case tree_constant_rep::matrix_constant:
      {
	Matrix m = objective_value.matrix_value ();
	if (m.rows () == 1 && m.columns () == 1)
	  retval = m.elem (0, 0);
	else
	  gripe_user_returned_invalid ("npsol_objective");
      }
      break;
    case tree_constant_rep::scalar_constant:
      retval = objective_value.double_value ();
      break;
    default:
      gripe_user_returned_invalid ("npsol_objective");
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
      decision_vars = tree_constant (m);
    }
  else
    {
      double d = x.elem (0);
      decision_vars = tree_constant (d);
    }

//  tree_constant name = tree_constant (npsol_constraints->name ());
  tree_constant *args = new tree_constant [2];
//  args[0] = name;
  args[1] = decision_vars;

  if (npsol_constraints != NULL_TREE)
    {
      tree_constant *tmp = npsol_constraints->eval (args, 2, 1, 0);
      delete [] args;
      if (tmp != NULL_TREE_CONST && tmp[0].is_defined ())
	{
	  retval = tmp[0].to_vector ();
	  delete [] tmp;
	}
      else
	{
	  delete [] tmp;
	  error ("npsol: error evaluating constraints");
	  jump_to_top_level ();
	}
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

tree_constant *
npsol (const tree_constant *args, int nargin, int nargout)
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

  tree_constant *retval = NULL_TREE_CONST;

  ColumnVector x = args[1].to_vector ();

  if (x.capacity () == 0)
    {
      error ("npsol: expecting vector as first argument");
      return retval;
    }

  npsol_objective = is_valid_function (args[2], "npsol", 1);
  if (npsol_objective == NULL_TREE
      || takes_correct_nargs (npsol_objective, 2, "npsol", 1) != 1)
    return retval;

  Objective func (npsol_objective_function);

  ColumnVector soln;

  Bounds bounds;
  if (nargin == 5 || nargin == 8 || nargin == 11)
    {
      ColumnVector lb = args[3].to_vector ();
      ColumnVector ub = args[4].to_vector ();

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
      soln = nlp.minimize (objf, inform, lambda);

      goto solved;
    }

  if (nargin == 5)
    {
      // 2. npsol (x, phi, lb, ub)

      NPSOL nlp (x, func, bounds);
      soln = nlp.minimize (objf, inform, lambda);

      goto solved;
    }

  npsol_constraints = NULL_TREE;
  if (nargin == 6 || nargin == 8 || nargin == 9 || nargin == 11)
    npsol_constraints = is_valid_function (args[nargin-2], "npsol", 0);

  if (nargin == 8 || nargin == 6)
    {
      if (npsol_constraints == NULL_TREE)
	{
	  ColumnVector lub = args[nargin-1].to_vector ();
	  Matrix c = args[nargin-2].to_matrix ();
	  ColumnVector llb = args[nargin-3].to_vector ();

	  LinConst linear_constraints (llb, c, lub);

	  if (! linear_constraints_ok (x, llb, c, lub, "npsol", 1))
	    return retval;

	  if (nargin == 6)
	    {
	      // 7. npsol (x, phi, llb, c, lub)

	      NPSOL nlp (x, func, linear_constraints);
	      soln = nlp.minimize (objf, inform, lambda);
	    }
	  else
	    {
	      // 3. npsol (x, phi, lb, ub, llb, c, lub)

	      NPSOL nlp (x, func, bounds, linear_constraints);
	      soln = nlp.minimize (objf, inform, lambda);
	    }
	  goto solved;
	}
      else
	{
	  if (takes_correct_nargs (npsol_constraints, 2, "npsol", 1))
	    {
	      ColumnVector nlub = args[nargin-1].to_vector ();
	      ColumnVector nllb = args[nargin-3].to_vector ();

	      NLFunc const_func (npsol_constraint_function);

	      if (! nonlinear_constraints_ok
		  (x, nllb, npsol_constraint_function, nlub, "npsol", 1))
		return retval;

	      NLConst nonlinear_constraints (nllb, const_func, nlub);

	      if (nargin == 6)
		{
		  // 8. npsol (x, phi, nllb, g, nlub)

		  NPSOL nlp (x, func, nonlinear_constraints);
		  soln = nlp.minimize (objf, inform, lambda);
		}
	      else
		{
		  // 5. npsol (x, phi, lb, ub, nllb, g, nlub)

		  NPSOL nlp (x, func, bounds, nonlinear_constraints);
		  soln = nlp.minimize (objf, inform, lambda);
		}
	      goto solved;
	    }
	}
    }

  if (nargin == 9 || nargin == 11)
    {
      if (npsol_constraints == NULL_TREE)
	{
	  // Produce error message.
	  is_valid_function (args[nargin-2], "npsol", 1);
	}
      else
	{
	  if (takes_correct_nargs (npsol_constraints, 2, "npsol", 1))
	    {
	      ColumnVector nlub = args[nargin-1].to_vector ();
	      ColumnVector nllb = args[nargin-3].to_vector ();

	      NLFunc const_func (npsol_constraint_function);

	      if (! nonlinear_constraints_ok
		  (x, nllb, npsol_constraint_function, nlub, "npsol", 1))
		return retval;

	      NLConst nonlinear_constraints (nllb, const_func, nlub);

	      ColumnVector lub = args[nargin-4].to_vector ();
	      Matrix c = args[nargin-5].to_matrix ();
	      ColumnVector llb = args[nargin-6].to_vector ();

	      if (! linear_constraints_ok (x, llb, c, lub, "npsol", 1))
		return retval;

	      LinConst linear_constraints (llb, c, lub);

	      if (nargin == 9)
		{
		  // 6. npsol (x, phi, llb, c, lub, nllb, g, nlub)

		  NPSOL nlp (x, func, linear_constraints,
			     nonlinear_constraints);

		  soln = nlp.minimize (objf, inform, lambda);
		}
	      else
		{
		  // 4. npsol (x, phi, lb, ub, llb, c, lub, nllb, g, nlub)

		  NPSOL nlp (x, func, bounds, linear_constraints,
			     nonlinear_constraints);

		  soln = nlp.minimize (objf, inform, lambda);
		}
	      goto solved;
	    }
	}
    }

  return retval;

 solved:

  retval = new tree_constant [nargout+1];
  retval[0] = tree_constant (soln, 1);
  if (nargout > 1)
    retval[1] = tree_constant (objf);
  if (nargout > 2)
    retval[2] = tree_constant ((double) inform);
  if (nargout > 3)
    retval[3] = tree_constant (lambda);

  return retval;
}

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
