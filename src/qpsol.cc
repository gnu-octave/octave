// f-qpsol.cc                                           -*- C++ -*-
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

#include "QPSOL.h"

#include "defun-dld.h"
#include "error.h"
#include "gripes.h"
#include "help.h"
#include "mappers.h"
#include "oct-obj.h"
#include "pager.h"
#include "utils.h"
#include "variables.h"

#ifndef QPSOL_MISSING

// XXX FIXME XXX -- this is duplicated in npsol.cc

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

static QPSOL_options qpsol_opts;

#endif

#if defined (QPSOL_MISSING)
DEFUN_DLD_BUILTIN ("qpsol", Fqpsol, Sqpsol, FSqpsol, 00,
  "This function requires QPSOL, which is not freely\n\
redistributable.  For more information, read the file\n\
libcruft/qpsol/README.MISSING in the source distribution.")
#else
DEFUN_DLD_BUILTIN ("qpsol", Fqpsol, Sqpsol, FSqpsol, 11,
  "[X, OBJ, INFO, LAMBDA] = qpsol (X, H, C [, LB, UB] [, A_LB, A, A_UB])\n\
\n\
Groups of arguments surrounded in `[]' are optional, but\n\
must appear in the same relative order shown above.")
#endif
{
/*

Handle all of the following:

  1. qpsol (x, H, c)
  2. qpsol (x, H, c, lb, ub)
  3. qpsol (x, H, c, lb, ub, llb, A, lub)
  4. qpsol (x, H, c,         llb, A, lub)

*/

  Octave_object retval;

#if defined (QPSOL_MISSING)

  // Force a bad value of inform, and empty matrices for x, phi, and
  // lambda.

  retval.resize (4, Matrix ());

  retval(2) = -1.0;

  print_usage ("qpsol");

#else

  int nargin = args.length ();

  if (nargin < 3 || nargin == 4 || nargin == 7 || nargin > 8
      || nargout > 4)
    {
      print_usage ("qpsol");
      return retval;
    }

  ColumnVector x = args(0).vector_value ();

  if (error_state || x.capacity () == 0)
    {
      error ("qpsol: expecting vector as first argument");
      return retval;
    }

  Matrix H = args(1).matrix_value ();

  if (error_state || H.rows () != H.columns () || H.rows () != x.capacity ())
    {
      error ("qpsol: H must be a square matrix consistent with the size of x");
      return retval;
    }

  ColumnVector c = args(2).vector_value ();

  if (error_state || c.capacity () != x.capacity ())
    {
      error ("qpsol: c must be a vector the same size as x");
      return retval;
    }

  Bounds bounds;
  if (nargin == 5 || nargin == 8)
    {
      ColumnVector lb = args(3).vector_value ();
      ColumnVector ub = args(4).vector_value ();

      int lb_len = lb.capacity ();
      int ub_len = ub.capacity ();

      if (error_state || lb_len != ub_len || lb_len != x.capacity ())
	{
	  error ("qpsol: lower and upper bounds and decision variable vector");
	  error ("must all have the same number of elements");
	  return retval;
	}

      bounds.resize (lb_len);
      bounds.set_lower_bounds (lb);
      bounds.set_upper_bounds (ub);
    }

  ColumnVector soln;
  double objf;
  ColumnVector lambda;
  int inform;

  if (nargin == 3)
    {
      // 1. qpsol (x, H, c)

      QPSOL qp (x, H, c);
      qp.set_options (qpsol_opts);
      soln = qp.minimize (objf, inform, lambda);

      goto solved;
    }

  if (nargin == 5)
    {
      //  2. qpsol (x, H, c, lb, ub)

      QPSOL qp (x, H, c, bounds);
      qp.set_options (qpsol_opts);
      soln = qp.minimize (objf, inform, lambda);

      goto solved;
    }

  if (nargin == 6 || nargin == 8)
    {
      ColumnVector lub = args(nargin-1).vector_value ();
      ColumnVector llb = args(nargin-3).vector_value ();

      if (error_state || llb.capacity () == 0 || lub.capacity () == 0)
	{
	  error ("qpsol: bounds for linear constraints must be vectors");
	  return retval;
	}

      Matrix A = args(nargin-2).matrix_value ();

      if (error_state)
	{
	  error ("qpsol: invalid linear constraint matrix");
	  return retval;
	}

      if (! linear_constraints_ok (x, llb, A, lub, "qpsol", 1))
	return retval;

      LinConst linear_constraints (llb, A, lub);

      if (nargin == 8)
	{
	  // 3. qpsol (x, H, c, lb, ub, llb, A, lub)

	  QPSOL qp (x, H, c, bounds, linear_constraints);
	  qp.set_options (qpsol_opts);
	  soln = qp.minimize (objf, inform, lambda);
	}
      else
	{
	  // 4. qpsol (x, H, c,         llb, A, lub)

	 QPSOL qp (x, H, c, linear_constraints);
	 qp.set_options (qpsol_opts);
	 soln = qp.minimize (objf, inform, lambda);
       }
      goto solved;
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

#ifndef QPSOL_MISSING

typedef void (QPSOL_options::*d_set_opt_mf) (double);
typedef void (QPSOL_options::*i_set_opt_mf) (int);
typedef double (QPSOL_options::*d_get_opt_mf) (void);
typedef int (QPSOL_options::*i_get_opt_mf) (void);

#define MAX_TOKENS 2

struct QPSOL_OPTIONS
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

static QPSOL_OPTIONS qpsol_option_table [] =
{
  { "feasibility tolerance",
    { "feasibility", "tolerance", 0, },
    { 1, 0, 0, }, 1,
    QPSOL_options::set_feasibility_tolerance, 0,
    QPSOL_options::feasibility_tolerance, 0, },

  { "infinite bound",
    { "infinite", "bound", 0, },
    { 2, 0, 0, }, 1,
    QPSOL_options::set_infinite_bound, 0,
    QPSOL_options::infinite_bound, 0, },

  { "iteration limit",
    { "iteration", "limit", 0, },
    { 2, 0, 0, }, 1,
    0, QPSOL_options::set_iteration_limit,
    0, QPSOL_options::iteration_limit, },

  { "print level",
    { "print", "level", 0, },
    { 1, 0, 0, }, 1,
    0, QPSOL_options::set_print_level,
    0, QPSOL_options::print_level, },

  { 0,
    { 0, 0, 0, },
    { 0, 0, 0, }, 0,
    0, 0, 0, 0, },
};

static void
print_qpsol_option_list (void)
{
  ostrstream output_buf;

  print_usage ("qpsol_options", 1);

  output_buf << "\n"
	     << "Options for qpsol include:\n\n"
	     << "  keyword                                  value\n"
	     << "  -------                                  -----\n\n";

  QPSOL_OPTIONS *list = qpsol_option_table;

  const char *keyword;
  while ((keyword = list->keyword) != 0)
    {
      output_buf.form ("  %-40s ", keyword);
      if (list->d_get_fcn)
	{
	  double val = (qpsol_opts.*list->d_get_fcn) ();
	  if (val < 0.0)
	    output_buf << "computed automatically";
	  else
	    output_buf << val;
	}
      else
	{
	  int val = (qpsol_opts.*list->i_get_fcn) ();
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
set_qpsol_option (const string& keyword, double val)
{
  QPSOL_OPTIONS *list = qpsol_option_table;

  while (list->keyword != 0)
    {
      if (keyword_almost_match (list->kw_tok, list->min_len, keyword,
				list->min_toks_to_match, MAX_TOKENS))
	{
	  if (list->d_set_fcn)
	    (qpsol_opts.*list->d_set_fcn) (val);
	  else
	    {
	      if (xisnan (val))
		{
		  error ("qpsol_options: %s: expecting integer, found NaN",
			 keyword.c_str ());
		}
	      else
		(qpsol_opts.*list->i_set_fcn) (NINT (val));
	    }
	  return;
	}
      list++;
    }

  warning ("qpsol_options: no match for `%s'", keyword.c_str ());
}

static Octave_object
show_qpsol_option (const string& keyword)
{
  Octave_object retval;

  QPSOL_OPTIONS *list = qpsol_option_table;

  while (list->keyword != 0)
    {
      if (keyword_almost_match (list->kw_tok, list->min_len, keyword,
				list->min_toks_to_match, MAX_TOKENS))
	{
	  if (list->d_get_fcn)
	    return (qpsol_opts.*list->d_get_fcn) ();
	  else
	    return (double) (qpsol_opts.*list->i_get_fcn) ();
	}
      list++;
    }

  warning ("qpsol_options: no match for `%s'", keyword.c_str ());

  return retval;
}

#endif

#if defined (QPSOL_MISSING)
DEFUN_DLD_BUILTIN ("qpsol_options", Fqpsol_options, Sqpsol_options,
		   FSqpsol_options, 00,
  "This function requires QPSOL, which is not freely\n\
redistributable.  For more information, read the file\n\
libcruft/qpsol/README.MISSING in the source distribution.")
#else
DEFUN_DLD_BUILTIN ("qpsol_options", Fqpsol_options, Sqpsol_options,
		   FSqpsol_options, 10,
  "qpsol_options (KEYWORD, VALUE)\n
\n\
Set or show options for qpsol.  Keywords may be abbreviated\n\
to the shortest match.")
#endif
{
  Octave_object retval;

#if defined (QPSOL_MISSING)

  print_usage ("qpsol");

#else

  int nargin = args.length ();

  if (nargin == 0)
    {
      print_qpsol_option_list ();
      return retval;
    }
  else if (nargin == 1 || nargin == 2)
    {
      string keyword = args(0).string_value ();

      if (! error_state)
	{
	  if (nargin == 1)
	    return show_qpsol_option (keyword);
	  else
	    {
	      double val = args(1).double_value ();

	      if (! error_state)
		{
		  set_qpsol_option (keyword, val);
		  return retval;
		}
	    }
	}
    }

  print_usage ("qpsol_options");

#endif

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
