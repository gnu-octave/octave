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

#include <iostream.h>

#include "LSODE.h"

#include "defun-dld.h"
#include "error.h"
#include "gripes.h"
#include "help.h"
#include "oct-obj.h"
#include "pager.h"
#include "utils.h"
#include "variables.h"

// Global pointer for user defined function required by lsode.
static tree_fvc *lsode_fcn;

static LSODE_options lsode_opts;

ColumnVector
lsode_user_function (const ColumnVector& x, double t)
{
  ColumnVector retval;

  int nstates = x.capacity ();

  octave_value_list args;
  args(1) = t;

  if (nstates > 1)
    {
      Matrix m (nstates, 1);
      for (int i = 0; i < nstates; i++)
	m (i, 0) = x (i);
      octave_value state (m);
      args(0) = state;
    }
  else
    {
      double d = x (0);
      octave_value state (d);
      args(0) = state;
    }

  if (lsode_fcn)
    {
      octave_value_list tmp = lsode_fcn->eval (0, 1, args);

      if (error_state)
	{
	  gripe_user_supplied_eval ("lsode");
	  return retval;
	}

      if (tmp.length () > 0 && tmp(0).is_defined ())
	{
	  retval = tmp(0).vector_value ();

	  if (error_state || retval.length () == 0)
	    gripe_user_supplied_eval ("lsode");
	}
      else
	gripe_user_supplied_eval ("lsode");
    }

  return retval;
}

DEFUN_DLD_BUILTIN (lsode, args, nargout,
  "lsode (F, X0, T_OUT, T_CRIT)\n\
\n\
The first argument is the name of the function to call to\n\
compute the vector of right hand sides.  It must have the form\n\
\n\
  xdot = f (x, t)\n\
\n\
where xdot and x are vectors and t is a scalar.\n")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin < 3 || nargin > 4 || nargout > 1)
    {
      print_usage ("lsode");
      return retval;
    }

  lsode_fcn = is_valid_function (args(0), "lsode", 1);
  if (! lsode_fcn)
    return retval;

  ColumnVector state = args(1).vector_value ();

  if (error_state)
    {
      error ("lsode: expecting state vector as second argument");
      return retval;
    }

  ColumnVector out_times = args(2).vector_value ();

  if (error_state)
    {
      error ("lsode: expecting output time vector as third argument");
      return retval;
    }

  ColumnVector crit_times;

  int crit_times_set = 0;
  if (nargin > 3)
    {
      crit_times = args(3).vector_value ();

      if (error_state)
	{
	  error ("lsode: expecting critical time vector as fourth argument");
	  return retval;
	}

      crit_times_set = 1;
    }

  double tzero = out_times (0);
  int nsteps = out_times.capacity ();

  ODEFunc func (lsode_user_function);
  LSODE ode (state, tzero, func);
  ode.copy (lsode_opts);

  int nstates = state.capacity ();
  Matrix output (nsteps, nstates + 1);

  if (crit_times_set)
    output = ode.integrate (out_times, crit_times);
  else
    output = ode.integrate (out_times);

  if (! error_state)
    {
      retval.resize (1);
      retval(0) = output;
    }

  return retval;
}

typedef void (LSODE_options::*d_set_opt_mf) (double);
typedef double (LSODE_options::*d_get_opt_mf) (void);

#define MAX_TOKENS 3

struct LSODE_OPTIONS
{
  const char *keyword;
  const char *kw_tok[MAX_TOKENS + 1];
  int min_len[MAX_TOKENS + 1];
  int min_toks_to_match;
  d_set_opt_mf d_set_fcn;
  d_get_opt_mf d_get_fcn;
};

static LSODE_OPTIONS lsode_option_table [] =
{
  { "absolute tolerance",
    { "absolute", "tolerance", 0, 0, },
    { 1, 0, 0, 0, }, 1,
    LSODE_options::set_absolute_tolerance,
    LSODE_options::absolute_tolerance, },

  { "initial step size",
    { "initial", "step", "size", 0, },
    { 1, 0, 0, 0, }, 1,
    LSODE_options::set_initial_step_size,
    LSODE_options::initial_step_size, },

  { "maximum step size",
    { "maximum", "step", "size", 0, },
    { 2, 0, 0, 0, }, 1,
    LSODE_options::set_maximum_step_size,
    LSODE_options::maximum_step_size, },

  { "minimum step size",
    { "minimum", "step", "size", 0, },
    { 2, 0, 0, 0, }, 1,
    LSODE_options::set_minimum_step_size,
    LSODE_options::minimum_step_size, },

  { "relative tolerance",
    { "relative", "tolerance", 0, 0, },
    { 1, 0, 0, 0, }, 1,
    LSODE_options::set_relative_tolerance,
    LSODE_options::relative_tolerance, },

  { 0,
    { 0, 0, 0, 0, },
    { 0, 0, 0, 0, }, 0,
    0, 0, },
};

static void
print_lsode_option_list (ostream& os)
{
  print_usage ("lsode_options", 1);

  os << "\n"
     << "Options for lsode include:\n\n"
     << "  keyword                                  value\n"
     << "  -------                                  -----\n\n";

  LSODE_OPTIONS *list = lsode_option_table;

  const char *keyword;
  while ((keyword = list->keyword) != 0)
    {
      os.form ("  %-40s ", keyword);

      double val = (lsode_opts.*list->d_get_fcn) ();
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
set_lsode_option (const string& keyword, double val)
{
  LSODE_OPTIONS *list = lsode_option_table;

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

  warning ("lsode_options: no match for `%s'", keyword.c_str ());
}

static octave_value_list
show_lsode_option (const string& keyword)
{
  octave_value_list retval;

  LSODE_OPTIONS *list = lsode_option_table;

  while (list->keyword != 0)
    {
      if (keyword_almost_match (list->kw_tok, list->min_len, keyword,
				list->min_toks_to_match, MAX_TOKENS))
	{
	  return (lsode_opts.*list->d_get_fcn) ();
	}
      list++;
    }

  warning ("lsode_options: no match for `%s'", keyword.c_str ());

  return retval;
}

DEFUN_DLD_BUILTIN (lsode_options, args, ,
  "lsode_options (KEYWORD, VALUE)\n\
\n\
Set or show options for lsode.  Keywords may be abbreviated\n\
to the shortest match.")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin == 0)
    {
      print_lsode_option_list (octave_stdout);
      return retval;
    }
  else if (nargin == 1 || nargin == 2)
    {
      string keyword = args(0).string_value ();

      if (! error_state)
	{
	  if (nargin == 1)
	    return show_lsode_option (keyword);
	  else
	    {
	      double val = args(1).double_value ();

	      if (! error_state)
		{
		  set_lsode_option (keyword, val);
		  return retval;
		}
	    }
	}
    }

  print_usage ("lsode_options");

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
