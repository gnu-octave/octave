// f-fsolve.cc                                           -*- C++ -*-
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

#include "NLEqn.h"

#include "tree-const.h"
#include "variables.h"
#include "gripes.h"
#include "error.h"
#include "utils.h"
#include "pager.h"
#include "f-fsolve.h"

// Global pointer for user defined function required by hybrd1.
static tree_fvc *fsolve_fcn;

#ifdef WITH_DLD
Octave_object
builtin_fsolve_2 (const Octave_object& args, int nargin, int nargout)
{
  return fsolve (args, nargin, nargout);
}

Octave_object
builtin_fsolve_options (const Octave_object& args, int nargin, int nargout)
{
  return fsolve_options (args, nargin, nargout);
}
#endif

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

//  tree_constant name = tree_constant (fsolve_fcn->name ());
  Octave_object args (2);
//  args(0) = name;

  if (n > 1)
    {
      Matrix m (n, 1);
      for (int i = 0; i < n; i++)
	m (i, 0) = x.elem (i);
      tree_constant vars (m);
      args(1) = vars;
    }
  else
    {
      double d = x.elem (0);
      tree_constant vars (d);
      args(1) = vars;
    }

  if (fsolve_fcn != (tree_fvc *) NULL)
    {
      Octave_object tmp = fsolve_fcn->eval (0, 1, args, 2);
      if (tmp.length () > 0 && tmp(0).is_defined ())
	{
	  retval = tmp(0).to_vector ();

	  if (retval.length () <= 0)
	    gripe_user_supplied_eval ("fsolve");
	}
      else
	gripe_user_supplied_eval ("fsolve");
    }

  return retval;
}

Octave_object
fsolve (const Octave_object& args, int nargin, int nargout)
{
// Assumes that we have been given the correct number of arguments.

  Octave_object retval;

  fsolve_fcn = is_valid_function (args(1), "fsolve", 1);
  if (fsolve_fcn == (tree_fvc *) NULL
      || takes_correct_nargs (fsolve_fcn, 2, "fsolve", 1) != 1)
    return retval;

  ColumnVector x = args(2).to_vector ();

  if (nargin > 3)
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
  retval(0) = tree_constant (soln, 1);

  if (nargout > 1)
    retval(1) = tree_constant ((double) info);

  if (nargout > 2)
    retval(2) = tree_constant ();

  return retval;
}

typedef void (NLEqn_options::*d_set_opt_mf) (double);
typedef double (NLEqn_options::*d_get_opt_mf) (void);

#define MAX_TOKENS 1

struct NLEQN_OPTIONS
{
  char *keyword;
  char *kw_tok[MAX_TOKENS + 1];
  int min_len[MAX_TOKENS + 1];
  int min_toks_to_match;
  d_set_opt_mf d_set_fcn;
  d_get_opt_mf d_get_fcn;
};

static NLEQN_OPTIONS fsolve_option_table [] =
{
  { "tolerance",
    { "tolerance", NULL, },
    { 1, 0, }, 1,
    NLEqn_options::set_tolerance,
    NLEqn_options::tolerance, },

  { NULL,
    { NULL, NULL, },
    { 0, 0, }, 0,
    NULL, NULL, },
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

  char *keyword;
  while ((keyword = list->keyword) != (char *) NULL)
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
do_fsolve_option (char *keyword, double val)
{
  NLEQN_OPTIONS *list = fsolve_option_table;

  while (list->keyword != (char *) NULL)
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

Octave_object
fsolve_options (const Octave_object& args, int nargin, int nargout)
{
  Octave_object retval;

  if (nargin == 1)
    print_fsolve_option_list ();
  else if (nargin == 3)
    {
      if (args(1).is_string_type ())
	{
	  char *keyword = args(1).string_value ();
	  double val = args(2).double_value ();
	  do_fsolve_option (keyword, val);
	}
      else
	print_usage ("fsolve_options");
    }
  else
    print_usage ("fsolve_options");

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
