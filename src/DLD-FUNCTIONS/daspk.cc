/*

Copyright (C) 1996, 1997, 2002 John W. Eaton

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

#include <iomanip>
#include <iostream>

#include "DASPK.h"

#include "defun-dld.h"
#include "error.h"
#include "gripes.h"
#include "oct-obj.h"
#include "ov-fcn.h"
#include "pager.h"
#include "unwind-prot.h"
#include "utils.h"
#include "variables.h"

// Global pointer for user defined function required by daspk.
static octave_function *daspk_fcn;

static DASPK_options daspk_opts;

// Is this a recursive call?
static int call_depth = 0;

ColumnVector
daspk_user_function (const ColumnVector& x, const ColumnVector& xdot,
		     double t, int& ires)
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

  if (daspk_fcn)
    {
      octave_value_list tmp = daspk_fcn->do_multi_index_op (1, args);

      if (error_state)
	{
	  gripe_user_supplied_eval ("daspk");
	  return retval;
	}

      int tlen = tmp.length ();
      if (tlen > 0 && tmp(0).is_defined ())
	{
	  retval = ColumnVector (tmp(0).vector_value ());

	  if (tlen > 1)
	    ires = tmp(1).int_value ();

	  if (error_state || retval.length () == 0)
	    gripe_user_supplied_eval ("daspk");
	}
      else
	gripe_user_supplied_eval ("daspk");
    }

  return retval;
}

#define DASPK_ABORT() \
  do \
    { \
      unwind_protect::run_frame ("Fdaspk"); \
      return retval; \
    } \
  while (0)

#define DASPK_ABORT1(msg) \
  do \
    { \
      ::error ("daspk: " msg); \
      DASPK_ABORT (); \
    } \
  while (0)

#define DASPK_ABORT2(fmt, arg) \
  do \
    { \
      ::error ("daspk: " fmt, arg); \
      DASPK_ABORT (); \
    } \
  while (0)

DEFUN_DLD (daspk, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {[@var{x}, @var{xdot}] =} daspk (@var{fcn}, @var{x0}, @var{xdot0}, @var{t}, @var{t_crit})\n\
Return a matrix of states and their first derivatives with respect to\n\
@var{t}.  Each row in the result matrices correspond to one of the\n\
elements in the vector @var{t}.  The first element of @var{t}\n\
corresponds to the initial state @var{x0} and derivative @var{xdot0}, so\n\
that the first row of the output @var{x} is @var{x0} and the first row\n\
of the output @var{xdot} is @var{xdot0}.\n\
\n\
The first argument, @var{fcn}, is a string that names the function to\n\
call to compute the vector of residuals for the set of equations.\n\
It must have the form\n\
\n\
@example\n\
@var{res} = f (@var{x}, @var{xdot}, @var{t})\n\
@end example\n\
\n\
@noindent\n\
where @var{x}, @var{xdot}, and @var{res} are vectors, and @var{t} is a\n\
scalar.\n\
\n\
The second and third arguments to @code{daspk} specify the initial\n\
condition of the states and their derivatives, and the fourth argument\n\
specifies a vector of output times at which the solution is desired, \n\
including the time corresponding to the initial condition.\n\
\n\
The set of initial states and derivatives are not strictly required to\n\
be consistent.  In practice, however, @sc{Dassl} is not very good at\n\
determining a consistent set for you, so it is best if you ensure that\n\
the initial values result in the function evaluating to zero.\n\
\n\
The fifth argument is optional, and may be used to specify a set of\n\
times that the DAE solver should not integrate past.  It is useful for\n\
avoiding difficulties with singularities and points where there is a\n\
discontinuity in the derivative.\n\
\n\
You can use the function @code{daspk_options} to set optional\n\
parameters for @code{daspk}.\n\
@end deftypefn")
{
  octave_value_list retval;

  unwind_protect::begin_frame ("Fdaspk");

  unwind_protect_int (call_depth);
  call_depth++;

  if (call_depth > 1)
    DASPK_ABORT1 ("invalid recursive call");

  int nargin = args.length ();

  if (nargin > 3 && nargin < 6)
    {
      daspk_fcn = extract_function
	(args(0), "daspk", "__daspk_fcn__",
	 "function res = __daspk_fcn__ (x, xdot, t) res = ",
	 "; endfunction");

      if (! daspk_fcn)
	DASPK_ABORT ();

      ColumnVector state = ColumnVector (args(1).vector_value ());

      if (error_state)
	DASPK_ABORT1 ("expecting state vector as second argument");

      ColumnVector deriv (args(2).vector_value ());

      if (error_state)
	DASPK_ABORT1 ("expecting derivative vector as third argument");

      ColumnVector out_times (args(3).vector_value ());

      if (error_state)
	DASPK_ABORT1 ("expecting output time vector as fourth argument");

      ColumnVector crit_times;
      int crit_times_set = 0;
      if (nargin > 4)
	{
	  crit_times = ColumnVector (args(4).vector_value ());

	  if (error_state)
	    DASPK_ABORT1 ("expecting critical time vector as fifth argument");

	  crit_times_set = 1;
	}

      if (state.capacity () != deriv.capacity ())
	DASPK_ABORT1 ("x and xdot must have the same size");

      double tzero = out_times (0);

      DAEFunc func (daspk_user_function);
      DASPK dae (state, deriv, tzero, func);
      dae.copy (daspk_opts);

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
    }
  else
    print_usage ("daspk");

  unwind_protect::run_frame ("Fdaspk");

  return retval;
}

typedef void (DASPK_options::*d_set_opt_mf) (double);
typedef double (DASPK_options::*d_get_opt_mf) (void);

#define MAX_TOKENS 3

struct DASPK_OPTIONS
{
  const char *keyword;
  const char *kw_tok[MAX_TOKENS + 1];
  int min_len[MAX_TOKENS + 1];
  int min_toks_to_match;
  d_set_opt_mf d_set_fcn;
  d_get_opt_mf d_get_fcn;
};

static DASPK_OPTIONS daspk_option_table [] =
{
  { "absolute tolerance",
    { "absolute", "tolerance", 0, 0, },
    { 1, 0, 0, 0, }, 1,
    &DASPK_options::set_absolute_tolerance,
    &DASPK_options::absolute_tolerance, },

  { "initial step size",
    { "initial", "step", "size", 0, },
    { 1, 0, 0, 0, }, 1,
    &DASPK_options::set_initial_step_size,
    &DASPK_options::initial_step_size, },

  { "maximum step size",
    { "maximum", "step", "size", 0, },
    { 2, 0, 0, 0, }, 1,
    &DASPK_options::set_maximum_step_size,
    &DASPK_options::maximum_step_size, },

  { "relative tolerance",
    { "relative", "tolerance", 0, 0, },
    { 1, 0, 0, 0, }, 1,
    &DASPK_options::set_relative_tolerance,
    &DASPK_options::relative_tolerance, },

  { 0,
    { 0, 0, 0, 0, },
    { 0, 0, 0, 0, }, 0,
    0, 0, },
};

static void
print_daspk_option_list (std::ostream& os)
{
  print_usage ("daspk_options", 1);

  os << "\n"
     << "Options for daspk include:\n\n"
     << "  keyword                                  value\n"
     << "  -------                                  -----\n\n";

  DASPK_OPTIONS *list = daspk_option_table;

  const char *keyword;
  while ((keyword = list->keyword) != 0)
    {
      os << "  "
	 << std::setiosflags (std::ios::left) << std::setw (40)
	 << keyword
	 << std::resetiosflags (std::ios::left)
	 << " ";

      double val = (daspk_opts.*list->d_get_fcn) ();
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
set_daspk_option (const std::string& keyword, double val)
{
  DASPK_OPTIONS *list = daspk_option_table;

  while (list->keyword != 0)
    {
      if (keyword_almost_match (list->kw_tok, list->min_len, keyword,
				list->min_toks_to_match, MAX_TOKENS))
	{
	  (daspk_opts.*list->d_set_fcn) (val);

	  return;
	}
      list++;
    }

  warning ("daspk_options: no match for `%s'", keyword.c_str ());
}

static octave_value_list
show_daspk_option (const std::string& keyword)
{
  octave_value retval;

  DASPK_OPTIONS *list = daspk_option_table;

  while (list->keyword != 0)
    {
      if (keyword_almost_match (list->kw_tok, list->min_len, keyword,
				list->min_toks_to_match, MAX_TOKENS))
	{
	  double val = (daspk_opts.*list->d_get_fcn) ();
	  if (val < 0.0)
	    retval = "computed automatically";
	  else
	    retval = val;

	  return retval;
	}
      list++;
    }

  warning ("daspk_options: no match for `%s'", keyword.c_str ());

  return retval;
}

DEFUN_DLD (daspk_options, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {} daspk_options (@var{opt}, @var{val})\n\
When called with two arguments, this function allows you set options\n\
parameters for the function @code{lsode}.  Given one argument,\n\
@code{daspk_options} returns the value of the corresponding option.  If\n\
no arguments are supplied, the names of all the available options and\n\
their current values are displayed.\n\
@end deftypefn")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin == 0)
    {
      print_daspk_option_list (octave_stdout);
      return retval;
    }
  else if (nargin == 1 || nargin == 2)
    {
      std::string keyword = args(0).string_value ();

      if (! error_state)
	{
	  if (nargin == 1)
	    return show_daspk_option (keyword);
	  else
	    {
	      double val = args(1).double_value ();

	      if (! error_state)
		{
		  set_daspk_option (keyword, val);
		  return retval;
		}
	    }
	}
    }

  print_usage ("daspk_options");

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
