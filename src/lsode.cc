// f-lsode.cc                                           -*- C++ -*-
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

#include "ODE.h"

#include "tree-const.h"
#include "variables.h"
#include "gripes.h"
#include "error.h"
#include "utils.h"
#include "pager.h"
#include "defun-dld.h"

// Global pointer for user defined function required by lsode.
static tree_fvc *lsode_fcn;

static ODE_options lsode_opts;

ColumnVector
lsode_user_function (const ColumnVector& x, double t)
{
  ColumnVector retval;

  int nstates = x.capacity ();

//  tree_constant name (lsode_fcn->name ());
  Octave_object args (3);
//  args(0) = name;
  args(2) = t;

  if (nstates > 1)
    {
      Matrix m (nstates, 1);
      for (int i = 0; i < nstates; i++)
	m (i, 0) = x.elem (i);
      tree_constant state (m);
      args(1) = state;
    }
  else
    {
      double d = x.elem (0);
      tree_constant state (d);
      args(1) = state;
    }

  if (lsode_fcn)
    {
      Octave_object tmp = lsode_fcn->eval (0, 1, args);

      if (error_state)
	{
	  gripe_user_supplied_eval ("lsode");
	  return retval;
	}

      if (tmp.length () > 0 && tmp(0).is_defined ())
	{
	  retval = tmp(0).to_vector ();

	  if (retval.length () == 0)
	    gripe_user_supplied_eval ("lsode");
	}
      else
	gripe_user_supplied_eval ("lsode");
    }

  return retval;
}

DEFUN_DLD ("lsode", Flsode, Slsode, 6, 1,
  "lsode (F, X0, T_OUT, T_CRIT)\n\
\n\
The first argument is the name of the function to call to\n\
compute the vector of right hand sides.  It must have the form\n\
\n\
  xdot = f (x, t)\n\
\n\
where xdot and x are vectors and t is a scalar.\n")
{
  Octave_object retval;

  int nargin = args.length ();

  if (nargin < 4 || nargin > 5 || nargout > 1)
    {
      print_usage ("lsode");
      return retval;
    }

  lsode_fcn = is_valid_function (args(1), "lsode", 1);
  if (! lsode_fcn || takes_correct_nargs (lsode_fcn, 3, "lsode", 1) != 1)
    return retval;

  ColumnVector state = args(2).to_vector ();
  ColumnVector out_times = args(3).to_vector ();
  ColumnVector crit_times;
  int crit_times_set = 0;
  if (nargin > 4)
    {
      crit_times = args(4).to_vector ();
      crit_times_set = 1;
    }

  double tzero = out_times.elem (0);
  int nsteps = out_times.capacity ();

  ODEFunc func (lsode_user_function);
  ODE ode (state, tzero, func);
  ode.copy (lsode_opts);

  int nstates = state.capacity ();
  Matrix output (nsteps, nstates + 1);

  if (crit_times_set)
    output = ode.integrate (out_times, crit_times);
  else
    output = ode.integrate (out_times);

  retval.resize (1);
  retval(0) = output;
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

static ODE_OPTIONS lsode_option_table [] =
{
  { "absolute tolerance",
    { "absolute", "tolerance", 0, 0, },
    { 1, 0, 0, 0, }, 1,
    ODE_options::set_absolute_tolerance,
    ODE_options::absolute_tolerance, },

  { "initial step size",
    { "initial", "step", "size", 0, },
    { 1, 0, 0, 0, }, 1,
    ODE_options::set_initial_step_size,
    ODE_options::initial_step_size, },

  { "maximum step size",
    { "maximum", "step", "size", 0, },
    { 2, 0, 0, 0, }, 1,
    ODE_options::set_maximum_step_size,
    ODE_options::maximum_step_size, },

  { "minimum step size",
    { "minimum", "step", "size", 0, },
    { 2, 0, 0, 0, }, 1,
    ODE_options::set_minimum_step_size,
    ODE_options::minimum_step_size, },

  { "relative tolerance",
    { "relative", "tolerance", 0, 0, },
    { 1, 0, 0, 0, }, 1,
    ODE_options::set_relative_tolerance,
    ODE_options::relative_tolerance, },

  { 0,
    { 0, 0, 0, 0, },
    { 0, 0, 0, 0, }, 0,
    0, 0, },
};

static void
print_lsode_option_list (void)
{
  ostrstream output_buf;

  print_usage ("lsode_options", 1);

  output_buf << "\n"
	     << "Options for lsode include:\n\n"
	     << "  keyword                                  value\n"
	     << "  -------                                  -----\n\n";

  ODE_OPTIONS *list = lsode_option_table;

  char *keyword;
  while ((keyword = list->keyword) != 0)
    {
      output_buf.form ("  %-40s ", keyword);

      double val = (lsode_opts.*list->d_get_fcn) ();
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
do_lsode_option (char *keyword, double val)
{
  ODE_OPTIONS *list = lsode_option_table;

  while (list->keyword != 0)
    {
      if (keyword_almost_match (list->kw_tok, list->min_len, keyword,
				list->min_toks_to_match, MAX_TOKENS))
	{
	  (lsode_opts.*list->d_set_fcn) (val);

	  return;
	}
      list++;
    }

  warning ("lsode_options: no match for `%s'", keyword);
}

DEFUN_DLD ("lsode_options", Flsode_options, Slsode_options, -1, 1,
  "lsode_options (KEYWORD, VALUE)\n\
\n\
Set or show options for lsode.  Keywords may be abbreviated\n\
to the shortest match.")
{
  Octave_object retval;

  int nargin = args.length ();

  if (nargin == 1)
    {
      print_lsode_option_list ();
    }
  else if (nargin == 3)
    {
      if (args(1).is_string_type ())
	{
	  char *keyword = args(1).string_value ();
	  double val = args(2).double_value ();
	  do_lsode_option (keyword, val);
	}
      else
	print_usage ("lsode_options");
    }
  else
    print_usage ("lsode_options");

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
