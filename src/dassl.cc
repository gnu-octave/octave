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

#include "DASSL.h"

#include "defun-dld.h"
#include "error.h"
#include "gripes.h"
#include "help.h"
#include "oct-obj.h"
#include "pager.h"
#include "pt-fvc.h"
#include "utils.h"
#include "variables.h"

// Global pointer for user defined function required by dassl.
static tree_fvc *dassl_fcn;

static DASSL_options dassl_opts;

ColumnVector
dassl_user_function (const ColumnVector& x, const ColumnVector& xdot, double t)
{
  ColumnVector retval;

  int nstates = x.capacity ();

  assert (nstates == xdot.capacity ());

  octave_value_list args;
  args(2) = t;

  if (nstates > 1)
    {
      Matrix m1 (nstates, 1);
      Matrix m2 (nstates, 1);
      for (int i = 0; i < nstates; i++)
	{
	  m1 (i, 0) = x (i);
	  m2 (i, 0) = xdot (i);
	}
      octave_value state (m1);
      octave_value deriv (m2);
      args(1) = deriv;
      args(0) = state;
    }
  else
    {
      double d1 = x (0);
      double d2 = xdot (0);
      octave_value state (d1);
      octave_value deriv (d2);
      args(1) = deriv;
      args(0) = state;
    }

  if (dassl_fcn)
    {
      octave_value_list tmp = dassl_fcn->eval (0, 1, args);

      if (error_state)
	{
	  gripe_user_supplied_eval ("dassl");
	  return retval;
	}

      if (tmp.length () > 0 && tmp(0).is_defined ())
	{
	  retval = tmp(0).vector_value ();

	  if (error_state || retval.length () == 0)
	    gripe_user_supplied_eval ("dassl");
	}
      else
	gripe_user_supplied_eval ("dassl");
    }

  return retval;
}

DEFUN_DLD (dassl, args, ,
  "dassl (\"function_name\", x_0, xdot_0, t_out)\n\
dassl (F, X_0, XDOT_0, T_OUT, T_CRIT)\n\
\n\
The first argument is the name of the function to call to\n\
compute the vector of residuals.  It must have the form\n\
\n\
  res = f (x, xdot, t)\n\
\n\
where x, xdot, and res are vectors, and t is a scalar.")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin < 4 || nargin > 5)
    {
      print_usage ("dassl");
      return retval;
    }

  dassl_fcn = extract_function
    (args(0), "dassl", "__dassl_fcn__",
     "function res = __dassl_fcn__ (x, xdot, t) res = ",
     "; endfunction");

  if (! dassl_fcn)
    return retval;

  ColumnVector state = args(1).vector_value ();

  if (error_state)
    {
      error ("dassl: expecting state vector as second argument");
      return retval;
    }

  ColumnVector deriv = args(2).vector_value ();

  if (error_state)
    {
      error ("dassl: expecting derivative vector as third argument");
      return retval;
    }

  ColumnVector out_times = args(3).vector_value ();

  if (error_state)
    {
      error ("dassl: expecting output time vector as fourth argument");
      return retval;
    }

  ColumnVector crit_times;
  int crit_times_set = 0;
  if (nargin > 4)
    {
      crit_times = args(4).vector_value ();

      if (error_state)
	{
	  error ("dassl: expecting critical time vector as fifth argument");
	  return retval;
	}

      crit_times_set = 1;
    }

  if (state.capacity () != deriv.capacity ())
    {
      error ("dassl: x and xdot must have the same size");
      return retval;
    }

  double tzero = out_times (0);

  DAEFunc func (dassl_user_function);
  DASSL dae (state, deriv, tzero, func);
  dae.copy (dassl_opts);

  Matrix output;
  Matrix deriv_output;

  if (crit_times_set)
    output = dae.integrate (out_times, deriv_output, crit_times);
  else
    output = dae.integrate (out_times, deriv_output);

  if (! error_state)
    {
      retval.resize (2);

      retval(0) = output;
      retval(1) = deriv_output;
    }

  return retval;
}

typedef void (DASSL_options::*d_set_opt_mf) (double);
typedef double (DASSL_options::*d_get_opt_mf) (void);

#define MAX_TOKENS 3

struct DASSL_OPTIONS
{
  const char *keyword;
  const char *kw_tok[MAX_TOKENS + 1];
  int min_len[MAX_TOKENS + 1];
  int min_toks_to_match;
  d_set_opt_mf d_set_fcn;
  d_get_opt_mf d_get_fcn;
};

static DASSL_OPTIONS dassl_option_table [] =
{
  { "absolute tolerance",
    { "absolute", "tolerance", 0, 0, },
    { 1, 0, 0, 0, }, 1,
    DASSL_options::set_absolute_tolerance,
    DASSL_options::absolute_tolerance, },

  { "initial step size",
    { "initial", "step", "size", 0, },
    { 1, 0, 0, 0, }, 1,
    DASSL_options::set_initial_step_size,
    DASSL_options::initial_step_size, },

  { "maximum step size",
    { "maximum", "step", "size", 0, },
    { 2, 0, 0, 0, }, 1,
    DASSL_options::set_maximum_step_size,
    DASSL_options::maximum_step_size, },

  { "relative tolerance",
    { "relative", "tolerance", 0, 0, },
    { 1, 0, 0, 0, }, 1,
    DASSL_options::set_relative_tolerance,
    DASSL_options::relative_tolerance, },

  { 0,
    { 0, 0, 0, 0, },
    { 0, 0, 0, 0, }, 0,
    0, 0, },
};

static void
print_dassl_option_list (ostream& os)
{
  print_usage ("dassl_options", 1);

  os << "\n"
     << "Options for dassl include:\n\n"
     << "  keyword                                  value\n"
     << "  -------                                  -----\n\n";

  DASSL_OPTIONS *list = dassl_option_table;

  const char *keyword;
  while ((keyword = list->keyword) != 0)
    {
      os.form ("  %-40s ", keyword);

      double val = (dassl_opts.*list->d_get_fcn) ();
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
set_dassl_option (const string& keyword, double val)
{
  DASSL_OPTIONS *list = dassl_option_table;

  while (list->keyword != 0)
    {
      if (keyword_almost_match (list->kw_tok, list->min_len, keyword,
				list->min_toks_to_match, MAX_TOKENS))
	{
	  (dassl_opts.*list->d_set_fcn) (val);

	  return;
	}
      list++;
    }

  warning ("dassl_options: no match for `%s'", keyword.c_str ());
}

static octave_value_list
show_dassl_option (const string& keyword)
{
  octave_value retval;

  DASSL_OPTIONS *list = dassl_option_table;

  while (list->keyword != 0)
    {
      if (keyword_almost_match (list->kw_tok, list->min_len, keyword,
				list->min_toks_to_match, MAX_TOKENS))
	{
	  double val = (dassl_opts.*list->d_get_fcn) ();
	  if (val < 0.0)
	    retval = "computed automatically";
	  else
	    retval = val;

	  return retval;
	}
      list++;
    }

  warning ("dassl_options: no match for `%s'", keyword.c_str ());

  return retval;
}

DEFUN_DLD (dassl_options, args, ,
  "dassl_options (KEYWORD, VALUE)\n\
\n\
Set or show options for dassl.  Keywords may be abbreviated\n\
to the shortest match.")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin == 0)
    {
      print_dassl_option_list (octave_stdout);
      return retval;
    }
  else if (nargin == 1 || nargin == 2)
    {
      string keyword = args(0).string_value ();

      if (! error_state)
	{
	  if (nargin == 1)
	    return show_dassl_option (keyword);
	  else
	    {
	      double val = args(1).double_value ();

	      if (! error_state)
		{
		  set_dassl_option (keyword, val);
		  return retval;
		}
	    }
	}
    }

  print_usage ("dassl_options");

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
