// f-fsolve.cc                                           -*- C++ -*-
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

#include <string>

#include <strstream.h>

#include "NLEqn.h"

#include "defun-dld.h"
#include "error.h"
#include "gripes.h"
#include "help.h"
#include "pager.h"
#include "oct-obj.h"
#include "utils.h"
#include "variables.h"

// Global pointer for user defined function required by hybrd1.
static tree_fvc *fsolve_fcn;

static NLEqn_options fsolve_opts;

int
hybrd_info_to_fsolve_info (int info)
{
  switch (info)
    {
    case -1:
      info = -2;
      break;

    case 0:
      info = -1;
      break;

    case 1:
      break;

    case 2:
      info = 4;
      break;

    case 3:
    case 4:
    case 5:
      info = 3;
      break;

    default:
      panic_impossible ();
      break;
    }

  return info;
}

ColumnVector
fsolve_user_function (const ColumnVector& x)
{
  ColumnVector retval;

  int n = x.capacity ();

  Octave_object args;
  args.resize (1);

  if (n > 1)
    {
      Matrix m (n, 1);
      for (int i = 0; i < n; i++)
	m (i, 0) = x.elem (i);
      tree_constant vars (m);
      args(0) = vars;
    }
  else
    {
      double d = x.elem (0);
      tree_constant vars (d);
      args(0) = vars;
    }

  if (fsolve_fcn)
    {
      Octave_object tmp = fsolve_fcn->eval (0, 1, args);
      if (tmp.length () > 0 && tmp(0).is_defined ())
	{
	  retval = tmp(0).vector_value ();

	  if (error_state || retval.length () <= 0)
	    gripe_user_supplied_eval ("fsolve");
	}
      else
	gripe_user_supplied_eval ("fsolve");
    }

  return retval;
}

DEFUN_DLD_BUILTIN ("fsolve", Ffsolve, Sfsolve, FSfsolve, 11,
  "Solve nonlinear equations using Minpack.  Usage:\n\
\n\
  [X, INFO] = fsolve (F, X0)\n\
\n\
Where the first argument is the name of the  function to call to\n\
compute the vector of function values.  It must have the form\n\
\n\
  y = f (x)
\n\
where y and x are vectors.")
{
  Octave_object retval;

  int nargin = args.length ();

  if (nargin != 2 || nargout > 3)
    {
      print_usage ("fsolve");
      return retval;
    }

  fsolve_fcn = is_valid_function (args(0), "fsolve", 1);
  if (! fsolve_fcn)
    return retval;

  ColumnVector x = args(1).vector_value ();

  if (error_state)
    {
      error ("fsolve: expecting vector as second argument");
      return retval;
    }

  if (nargin > 2)
    warning ("fsolve: ignoring extra arguments");

  if (nargout > 2)
    warning ("fsolve: can't compute path output yet");

  NLFunc foo_fcn (fsolve_user_function);
  NLEqn foo (x, foo_fcn);
  foo.copy (fsolve_opts);

  int info;
  ColumnVector soln = foo.solve (info);

  info = hybrd_info_to_fsolve_info (info);

  retval.resize (nargout ? nargout : 1);
  retval(0) = soln, 1;

  if (nargout > 1)
    retval(1) = (double) info;

  return retval;
}

typedef void (NLEqn_options::*d_set_opt_mf) (double);
typedef double (NLEqn_options::*d_get_opt_mf) (void);

#define MAX_TOKENS 1

struct NLEQN_OPTIONS
{
  const char *keyword;
  const char *kw_tok[MAX_TOKENS + 1];
  int min_len[MAX_TOKENS + 1];
  int min_toks_to_match;
  d_set_opt_mf d_set_fcn;
  d_get_opt_mf d_get_fcn;
};

static NLEQN_OPTIONS fsolve_option_table [] =
{
  { "tolerance",
    { "tolerance", 0, },
    { 1, 0, }, 1,
    NLEqn_options::set_tolerance,
    NLEqn_options::tolerance, },

  { 0,
    { 0, 0, },
    { 0, 0, }, 0,
    0, 0, },
};

static void
print_fsolve_option_list (void)
{
  ostrstream output_buf;

  print_usage ("fsolve_options", 1);

  output_buf << "\n"
	     << "Options for fsolve include:\n\n"
	     << "  keyword                                  value\n"
	     << "  -------                                  -----\n\n";

  NLEQN_OPTIONS *list = fsolve_option_table;

  const char *keyword;
  while ((keyword = list->keyword) != 0)
    {
      output_buf.form ("  %-40s ", keyword);

      double val = (fsolve_opts.*list->d_get_fcn) ();
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
set_fsolve_option (const char *keyword, double val)
{
  NLEQN_OPTIONS *list = fsolve_option_table;

  while (list->keyword != 0)
    {
      if (keyword_almost_match (list->kw_tok, list->min_len, keyword,
				list->min_toks_to_match, MAX_TOKENS))
	{
	  (fsolve_opts.*list->d_set_fcn) (val);

	  return;
	}
      list++;
    }

  warning ("fsolve_options: no match for `%s'", keyword);
}

static Octave_object
show_fsolve_option (const char *keyword)
{
  Octave_object retval;

  NLEQN_OPTIONS *list = fsolve_option_table;

  while (list->keyword != 0)
    {
      if (keyword_almost_match (list->kw_tok, list->min_len, keyword,
				list->min_toks_to_match, MAX_TOKENS))
	{
	  return (fsolve_opts.*list->d_get_fcn) ();
	}
      list++;
    }

  warning ("fsolve_options: no match for `%s'", keyword);

  return retval;
}

DEFUN_DLD_BUILTIN ("fsolve_options", Ffsolve_options, Sfsolve_options,
		   FSfsolve_options, 10,
  "fsolve_options (KEYWORD, VALUE)\n\
\n\
Set or show options for fsolve.  Keywords may be abbreviated\n\
to the shortest match.")
{
  Octave_object retval;

  int nargin = args.length ();

  if (nargin == 0)
    {
      print_fsolve_option_list ();
      return retval;
    }
  else if (nargin == 1 || nargin == 2)
    {
      string tstr = args(0).string_value ();
      const char *keyword = tstr.c_str ();

      if (! error_state)
	{
	  if (nargin == 1)
	    return show_fsolve_option (keyword);
	  else
	    {
	      double val = args(1).double_value ();

	      if (! error_state)
		{
		  set_fsolve_option (keyword, val);
		  return retval;
		}
	    }
	}
    }

  print_usage ("fsolve_options");

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
