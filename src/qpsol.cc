// f-qpsol.cc                                           -*- C++ -*-
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

#ifndef QPSOL_MISSING

#include <strstream.h>

#include "QPSOL.h"

#include "tree-const.h"
#include "variables.h"
#include "gripes.h"
#include "error.h"
#include "utils.h"
#include "pager.h"
#include "help.h"
#include "defun-dld.h"

// This should probably be defined in some shared file and declared in
// a header file...
extern int linear_constraints_ok (const ColumnVector& x,
				  const ColumnVector& llb, const Matrix& c,
				  const ColumnVector& lub, char *warn_for,
				  int warn);

static QPSOL_options qpsol_opts;

#if defined (QPSOL_MISSING)
DEFUN_DLD ("qpsol", Fqpsol, Sqpsol, 9, 3,
  "This function requires QPSOL, which is not freely\n\
redistributable.  For more information, read the file\n\
libcruft/qpsol/README.MISSING in the source distribution.")
#else
DEFUN_DLD ("qpsol", Fqpsol, Sqpsol, 9, 3,
  "[X, OBJ, INFO, LAMBDA] = qpsol (X, H, C [, LB, UB] [, LB, A, UB])\n\
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

// Force a bad value of inform, and empty matrices for x, phi, and lambda.

  retval.resize (4, Matrix ());

  retval(2) = -1.0;

  print_usage ("qpsol");

#else

  int nargin = args.length ();

  if (nargin < 4 || nargin == 5 || nargin == 8 || nargin > 9
      || nargout > 4)
    {
      print_usage ("qpsol");
      return retval;
    }

  ColumnVector x = args(1).to_vector ();
  if (x.capacity () == 0)
    {
      error ("qpsol: expecting vector as first argument");
      return retval;
    }

  Matrix H = args(2).to_matrix ();
  if (H.rows () != H.columns () || H.rows () != x.capacity ())
    {
      error ("qpsol: H must be a square matrix consistent with the size of x");
      return retval;
    }

  ColumnVector c = args(3).to_vector ();
  if (c.capacity () != x.capacity ())
    {
      error ("qpsol: c must be a vector the same size as x");
      return retval;
    }

  Bounds bounds;
  if (nargin == 6 || nargin == 9)
    {
      ColumnVector lb = args(4).to_vector ();
      ColumnVector ub = args(5).to_vector ();

      int lb_len = lb.capacity ();
      int ub_len = ub.capacity ();
      if (lb_len != ub_len || lb_len != x.capacity ())
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

  if (nargin == 4)
    {
      // 1. qpsol (x, H, c)

      QPSOL qp (x, H, c);
      qp.copy (qpsol_opts);
      soln = qp.minimize (objf, inform, lambda);

      goto solved;
    }

  if (nargin == 6)
    {
      //  2. qpsol (x, H, c, lb, ub)

      QPSOL qp (x, H, c, bounds);
      qp.copy (qpsol_opts);
      soln = qp.minimize (objf, inform, lambda);

      goto solved;
    }

  if (nargin == 7 || nargin == 9)
    {
      ColumnVector lub = args(nargin-1).to_vector ();
      Matrix A = args(nargin-2).to_matrix ();
      ColumnVector llb = args(nargin-3).to_vector ();

      if (llb.capacity () == 0 || lub.capacity () == 0)
	{
	  error ("qpsol: bounds for linear constraints must be vectors");
	  return retval;
	}

      if (! linear_constraints_ok (x, llb, A, lub, "qpsol", 1))
	return retval;

      LinConst linear_constraints (llb, A, lub);

      if (nargin == 9)
	{
	  // 3. qpsol (x, H, c, lb, ub, llb, A, lub)

	  QPSOL qp (x, H, c, bounds, linear_constraints);
	  qp.copy (qpsol_opts);
	  soln = qp.minimize (objf, inform, lambda);
	}
      else
	{
	  // 4. qpsol (x, H, c,         llb, A, lub)

	 QPSOL qp (x, H, c, linear_constraints);
	 qp.copy (qpsol_opts);
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
do_qpsol_option (char *keyword, double val)
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
	    (qpsol_opts.*list->i_set_fcn) (NINT (val));

	  return;
	}
      list++;
    }

  warning ("qpsol_options: no match for `%s'", keyword);
}

#if defined (QPSOL_MISSING)
DEFUN_DLD ("qpsol_options", Fqpsol_options, Sqpsol_options, -1, 1,
  "This function requires QPSOL, which is not freely\n\
redistributable.  For more information, read the file\n\
libcruft/qpsol/README.MISSING in the source distribution.")
#else
DEFUN_DLD ("qpsol_options", Fqpsol_options, Sqpsol_options, -1, 1,
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

  if (nargin == 1)
    {
      print_qpsol_option_list ();
    }
  else if (nargin == 3)
    {
      if (args(1).is_string_type ())
	{
	  char *keyword = args(1).string_value ();
	  double val = args(2).double_value ();
	  do_qpsol_option (keyword, val);
	}
      else
	print_usage ("qpsol_options");
    }
  else
    {
      print_usage ("qpsol_options");
    }

#endif

  return retval;
}

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
