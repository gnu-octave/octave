// f-dassl.cc                                           -*- C++ -*-
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

#include <strstream.h>

#include "DAE.h"

#include "tree-const.h"
#include "variables.h"
#include "gripes.h"
#include "error.h"
#include "utils.h"
#include "pager.h"
#include "f-dassl.h"

// Global pointer for user defined function required by dassl.
static tree *dassl_fcn;

#ifdef WITH_DLD
tree_constant *
builtin_dassl_2 (const tree_constant *args, int nargin, int nargout)
{
  return dassl (args, nargin, nargout);
}

tree_constant *
builtin_dassl_options_2 (const tree_constant *args, int nargin, int nargout)
{
  return dassl_options (args, nargin, nargout);
}
#endif

static ODE_options dassl_opts;

ColumnVector
dassl_user_function (const ColumnVector& x, const ColumnVector& xdot, double t)
{
  ColumnVector retval;

  int nstates = x.capacity ();

  assert (nstates == xdot.capacity ());

//  tree_constant name (dassl_fcn->name ());
  tree_constant *args = new tree_constant [4];
//  args[0] = name;
  args[3] = tree_constant (t);

  if (nstates > 1)
    {
      Matrix m1 (nstates, 1);
      Matrix m2 (nstates, 1);
      for (int i = 0; i < nstates; i++)
	{
	  m1 (i, 0) = x.elem (i);
	  m2 (i, 0) = xdot.elem (i);
	}
      tree_constant state (m1);
      tree_constant deriv (m2);
      args[1] = state;
      args[2] = deriv;
    }
  else
    {
      double d1 = x.elem (0);
      double d2 = xdot.elem (0);
      tree_constant state (d1);
      tree_constant deriv (d2);
      args[1] = state;
      args[2] = deriv;
    }

  if (dassl_fcn != NULL_TREE)
    {
      tree_constant *tmp = dassl_fcn->eval (args, 4, 1, 0);

      delete [] args;

      if (error_state)
	{
	  gripe_user_supplied_eval ("dassl");
	  return retval;
	}

      if (tmp != NULL_TREE_CONST && tmp[0].is_defined ())
	{
	  retval = tmp[0].to_vector ();

	  delete [] tmp;

	  if (retval.length () == 0)
	    gripe_user_supplied_eval ("dassl");
	}
      else
	{
	  delete [] tmp;
	  gripe_user_supplied_eval ("dassl");
	}
    }

  return retval;
}

tree_constant *
dassl (const tree_constant *args, int nargin, int nargout)
{
// Assumes that we have been given the correct number of arguments.

  tree_constant *retval = NULL_TREE_CONST;

  dassl_fcn = is_valid_function (args[1], "dassl", 1);
  if (dassl_fcn == NULL_TREE
      || takes_correct_nargs (dassl_fcn, 4, "dassl", 1) != 1)
    return retval;

  ColumnVector state = args[2].to_vector ();
  ColumnVector deriv = args[3].to_vector ();
  ColumnVector out_times = args[4].to_vector ();
  ColumnVector crit_times;
  int crit_times_set = 0;
  if (nargin > 5)
    {
      crit_times = args[5].to_vector ();
      crit_times_set = 1;
    }

  if (state.capacity () != deriv.capacity ())
    {
      error ("dassl: x and xdot must have the same size");
      return retval;
    }

  double tzero = out_times.elem (0);

  DAEFunc func (dassl_user_function);
  DAE dae (state, deriv, tzero, func);
  dae.copy (dassl_opts);

  Matrix output;
  Matrix deriv_output;

  if (crit_times_set)
    output = dae.integrate (out_times, deriv_output, crit_times);
  else
    output = dae.integrate (out_times, deriv_output);

  retval = new tree_constant [3];
  retval[0] = tree_constant (output);
  retval[1] = tree_constant (deriv_output);
  return retval;
}

typedef void (ODE_options::*d_set_opt_mf) (double);
typedef double (ODE_options::*d_get_opt_mf) (void);

#define MAX_TOKENS 3

struct ODE_OPTIONS
{
  char *keyword;
  char *kw_tok[MAX_TOKENS + 1];
  int min_len[MAX_TOKENS + 1];
  int min_toks_to_match;
  d_set_opt_mf d_set_fcn;
  d_get_opt_mf d_get_fcn;
};

static ODE_OPTIONS dassl_option_table[] =
{
  { "absolute tolerance",
    { "absolute", "tolerance", NULL, NULL, },
    { 1, 0, 0, 0, }, 1,
    ODE_options::set_absolute_tolerance,
    ODE_options::absolute_tolerance, },

  { "initial step size",
    { "initial", "step", "size", NULL, },
    { 1, 0, 0, 0, }, 1,
    ODE_options::set_initial_step_size,
    ODE_options::initial_step_size, },

  { "maximum step size",
    { "maximum", "step", "size", NULL, },
    { 2, 0, 0, 0, }, 1,
    ODE_options::set_maximum_step_size,
    ODE_options::maximum_step_size, },

  { "relative tolerance",
    { "relative", "tolerance", NULL, NULL, },
    { 1, 0, 0, 0, }, 1,
    ODE_options::set_relative_tolerance,
    ODE_options::relative_tolerance, },

  { NULL,
    { NULL, NULL, NULL, NULL, },
    { 0, 0, 0, 0, }, 0,
    NULL, NULL, },
};

static void
print_dassl_option_list (void)
{
  ostrstream output_buf;

  print_usage ("dassl_options", 1);

  output_buf << "\n"
	     << "Options for dassl include:\n\n"
	     << "  keyword                                  value\n"
	     << "  -------                                  -----\n\n";

  ODE_OPTIONS *list = dassl_option_table;

  char *keyword;
  while ((keyword = list->keyword) != (char *) NULL)
    {
      output_buf.form ("  %-40s ", keyword);

      double val = (dassl_opts.*list->d_get_fcn) ();
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
do_dassl_option (char *keyword, double val)
{
  ODE_OPTIONS *list = dassl_option_table;

  while (list->keyword != (char *) NULL)
    {
      if (keyword_almost_match (list->kw_tok, list->min_len, keyword,
				list->min_toks_to_match, MAX_TOKENS))
	{
	  (dassl_opts.*list->d_set_fcn) (val);

	  return;
	}
      list++;
    }

  warning ("dassl_options: no match for `%s'", keyword);
}

tree_constant *
dassl_options (const tree_constant *args, int nargin, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;

  if (nargin == 1)
    {
      print_dassl_option_list ();
    }
  else if (nargin == 3)
    {
      if (args[1].is_string_type ())
	{
	  char *keyword = args[1].string_value ();
	  double val = args[2].double_value ();
	  do_dassl_option (keyword, val);
	}
      else
	print_usage ("dassl_options");
    }
  else
    {
      print_usage ("dassl_options");
    }

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
