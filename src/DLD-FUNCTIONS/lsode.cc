/*

Copyright (C) 1996, 1997 John W. Eaton

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

#include "LSODE.h"
#include "lo-mappers.h"

#include "defun-dld.h"
#include "error.h"
#include "gripes.h"
#include "oct-obj.h"
#include "ov-fcn.h"
#include "pager.h"
#include "pr-output.h"
#include "unwind-prot.h"
#include "utils.h"
#include "variables.h"

// Global pointer for user defined function required by lsode.
static octave_function *lsode_fcn;

// Global pointer for optional user defined jacobian function used by lsode.
static octave_function *lsode_jac;

static LSODE_options lsode_opts;

// Is this a recursive call?
static int call_depth = 0;

ColumnVector
lsode_user_function (const ColumnVector& x, double t)
{
  ColumnVector retval;

  int nstates = x.capacity ();

  octave_value_list args;
  args(1) = t;

  Matrix m (nstates, 1);
  for (int i = 0; i < nstates; i++)
    m (i, 0) = x (i);
  octave_value state (m);
  args(0) = state;

  if (lsode_fcn)
    {
      octave_value_list tmp = lsode_fcn->do_multi_index_op (1, args);

      if (error_state)
	{
	  gripe_user_supplied_eval ("lsode");
	  return retval;
	}

      if (tmp.length () > 0 && tmp(0).is_defined ())
	{
	  retval = ColumnVector (tmp(0).vector_value ());

	  if (error_state || retval.length () == 0)
	    gripe_user_supplied_eval ("lsode");
	}
      else
	gripe_user_supplied_eval ("lsode");
    }

  return retval;
}

Matrix
lsode_user_jacobian (const ColumnVector& x, double t)
{
  Matrix retval;

  int nstates = x.capacity ();

  octave_value_list args;
  args(1) = t;

  Matrix m (nstates, 1);
  for (int i = 0; i < nstates; i++)
    m (i, 0) = x (i);
  octave_value state (m);
  args(0) = state;

  if (lsode_jac)
    {
      octave_value_list tmp = lsode_jac->do_multi_index_op (1, args);

      if (error_state)
	{
	  gripe_user_supplied_eval ("lsode");
	  return retval;
	}

      if (tmp.length () > 0 && tmp(0).is_defined ())
	{
	  retval = tmp(0).matrix_value ();

	  if (error_state || retval.length () == 0)
	    gripe_user_supplied_eval ("lsode");
	}
      else
	gripe_user_supplied_eval ("lsode");
    }

  return retval;
}

#define LSODE_ABORT() \
  do \
    { \
      unwind_protect::run_frame ("Flsode"); \
      return retval; \
    } \
  while (0)
 
#define LSODE_ABORT1(msg) \
  do \
    { \
      ::error ("lsode: " msg); \
      LSODE_ABORT (); \
    } \
  while (0)

#define LSODE_ABORT2(fmt, arg) \
  do \
    { \
      ::error ("lsode: " fmt, arg); \
      LSODE_ABORT (); \
    } \
  while (0)

DEFUN_DLD (lsode, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {} lsode (@var{fcn}, @var{x0}, @var{t}, @var{t_crit})\n\
Return a matrix of @var{x} as a function of @var{t}, given the initial\n\
state of the system @var{x0}.  Each row in the result matrix corresponds\n\
to one of the elements in the vector @var{t}.  The first element of\n\
@var{t} corresponds to the initial state @var{x0}, so that the first row\n\
of the output is @var{x0}.\n\
\n\
The first argument, @var{fcn}, is a string that names the function to\n\
call to compute the vector of right hand sides for the set of equations.\n\
It must have the form\n\
\n\
@example\n\
@var{xdot} = f (@var{x}, @var{t})\n\
@end example\n\
\n\
@noindent\n\
where @var{xdot} and @var{x} are vectors and @var{t} is a scalar.\n\
\n\
The fourth argument is optional, and may be used to specify a set of\n\
times that the ODE solver should not integrate past.  It is useful for\n\
avoiding difficulties with singularities and points where there is a\n\
discontinuity in the derivative.\n\
@end deftypefn")
{
  octave_value_list retval;

  unwind_protect::begin_frame ("Flsode");

  unwind_protect_int (call_depth);
  call_depth++;

  if (call_depth > 1)
    LSODE_ABORT1 ("invalid recursive call");

  int nargin = args.length ();

  if (nargin > 2 && nargin < 5 && nargout < 2)
    {
      octave_value f_arg = args(0);

      switch (f_arg.rows ())
	{
	case 1:
	  lsode_fcn = extract_function
	    (args(0), "lsode", "__lsode_fcn__",
	     "function xdot = __lsode_fcn__ (x, t) xdot = ",
	     "; endfunction");
	  break;

	case 2:
	  {
	    string_vector tmp = args(0).all_strings ();

	    if (! error_state)
	      {
		lsode_fcn = extract_function
		  (tmp(0), "lsode", "__lsode_fcn__",
		   "function xdot = __lsode_fcn__ (x, t) xdot = ",
		   "; endfunction");

		if (lsode_fcn)
		  {
		    lsode_jac = extract_function
		      (tmp(1), "lsode", "__lsode_jac__",
		       "function jac = __lsode_jac__ (x, t) jac = ",
		       "; endfunction");

		    if (! lsode_jac)
		      lsode_fcn = 0;
		  }
	      }
	  }
	  break;

	default:
	  LSODE_ABORT1
	    ("first arg should be a string or 2-element string array");
	}

      if (error_state || ! lsode_fcn)
	LSODE_ABORT ();

      ColumnVector state (args(1).vector_value ());

      if (error_state)
	LSODE_ABORT1 ("expecting state vector as second argument");

      ColumnVector out_times (args(2).vector_value ());

      if (error_state)
	LSODE_ABORT1 ("expecting output time vector as third argument");

      ColumnVector crit_times;

      int crit_times_set = 0;
      if (nargin > 3)
	{
	  crit_times = ColumnVector (args(3).vector_value ());

	  if (error_state)
	    LSODE_ABORT1 ("expecting critical time vector as fourth argument");

	  crit_times_set = 1;
	}

      double tzero = out_times (0);

      ODEFunc func (lsode_user_function);
      if (lsode_jac)
	func.set_jacobian_function (lsode_user_jacobian);

      LSODE ode (state, tzero, func);

      ode.copy (lsode_opts);

      Matrix output;
      if (crit_times_set)
	output = ode.integrate (out_times, crit_times);
      else
	output = ode.integrate (out_times);

      if (! error_state)
	{
	  retval.resize (1);
	  retval(0) = output;
	}
    }
  else
    print_usage ("lsode");

  unwind_protect::run_frame ("Flsode");

  return retval;
}

typedef void (LSODE_options::*da_set_opt_mf) (const Array<double>&);
typedef void (LSODE_options::*d_set_opt_mf) (double);
typedef void (LSODE_options::*i_set_opt_mf) (int);
typedef Array<double> (LSODE_options::*da_get_opt_mf) (void) const;
typedef double (LSODE_options::*d_get_opt_mf) (void) const;
typedef int (LSODE_options::*i_get_opt_mf) (void) const;

#define MAX_TOKENS 3

struct LSODE_OPTIONS
{
  const char *keyword;
  const char *kw_tok[MAX_TOKENS + 1];
  int min_len[MAX_TOKENS + 1];
  int min_toks_to_match;
  da_set_opt_mf da_set_fcn;
  d_set_opt_mf d_set_fcn;
  i_set_opt_mf i_set_fcn;
  da_get_opt_mf da_get_fcn;
  d_get_opt_mf d_get_fcn;
  i_get_opt_mf i_get_fcn;
};

static LSODE_OPTIONS lsode_option_table [] =
{
  { "absolute tolerance",
    { "absolute", "tolerance", 0, 0, },
    { 1, 0, 0, 0, }, 1,
    &LSODE_options::set_absolute_tolerance, 0, 0,
    &LSODE_options::absolute_tolerance, 0, 0, },

  { "initial step size",
    { "initial", "step", "size", 0, },
    { 1, 0, 0, 0, }, 1,
    0, &LSODE_options::set_initial_step_size, 0,
    0, &LSODE_options::initial_step_size, 0, },

  { "maximum step size",
    { "maximum", "step", "size", 0, },
    { 2, 0, 0, 0, }, 1,
    0, &LSODE_options::set_maximum_step_size, 0,
    0, &LSODE_options::maximum_step_size, 0, },

  { "minimum step size",
    { "minimum", "step", "size", 0, },
    { 2, 0, 0, 0, }, 1,
    0, &LSODE_options::set_minimum_step_size, 0,
    0, &LSODE_options::minimum_step_size, 0, },

  { "relative tolerance",
    { "relative", "tolerance", 0, 0, },
    { 1, 0, 0, 0, }, 1,
    0, &LSODE_options::set_relative_tolerance, 0,
    0, &LSODE_options::relative_tolerance, 0, },

  { "step limit",
    { "step", "limit", 0, 0, },
    { 1, 0, 0, 0, }, 1,
    0, 0, &LSODE_options::set_step_limit,
    0, 0, &LSODE_options::step_limit, },

  { 0,
    { 0, 0, 0, 0, },
    { 0, 0, 0, 0, }, 0,
    0, 0, 0, 0, 0, 0, },
};

static void
print_lsode_option_list (std::ostream& os)
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
      os << "  "
	 << std::setiosflags (std::ios::left) << std::setw (40)
	 << keyword
	 << std::resetiosflags (std::ios::left)
	 << " ";

      if (list->da_get_fcn)
	{
	  Array<double> val = (lsode_opts.*list->da_get_fcn) ();
	  if (val.length () == 1)
	    {
	      if (val(0) < 0.0)
		os << "computed automatically";
	      else
		os << val(0);
	    }
	  else
	    {
	      os << "\n\n";
	      // XXX FIXME XXX
	      Matrix tmp = Matrix (ColumnVector (val));
	      octave_print_internal (os, tmp, false, 2);
	      os << "\n";
	    }
	}
      else if (list->d_get_fcn)
	{
	  double val = (lsode_opts.*list->d_get_fcn) ();
	  if (val < 0.0)
	    os << "computed automatically";
	  else
	    os << val;
	}
      else
	{
	  int val = (lsode_opts.*list->i_get_fcn) ();
	  if (val < 0)
	    os << "infinite";
	  else
	    os << val;
	}
      os << "\n";
      list++;
    }

  os << "\n";
}

static void
set_lsode_option (const std::string& keyword, double val)
{
  LSODE_OPTIONS *list = lsode_option_table;

  while (list->keyword != 0)
    {
      if (keyword_almost_match (list->kw_tok, list->min_len, keyword,
				list->min_toks_to_match, MAX_TOKENS))
	{
	  if (list->d_set_fcn)
	    (lsode_opts.*list->d_set_fcn) (val);
	  else
	    {
	      if (xisnan (val))
		{
		  error ("lsode_options: %s: expecting integer, found NaN",
			 keyword.c_str ());
		}
	      else
		(lsode_opts.*list->i_set_fcn) (NINT (val));
	    }
	  return;
	}
      list++;
    }

  warning ("lsode_options: no match for `%s'", keyword.c_str ());
}

static void
set_lsode_option (const std::string& keyword, const Array<double>& val)
{
  LSODE_OPTIONS *list = lsode_option_table;

  while (list->keyword != 0)
    {
      if (keyword_almost_match (list->kw_tok, list->min_len, keyword,
				list->min_toks_to_match, MAX_TOKENS))
	{
	  if (list->da_set_fcn)
	    (lsode_opts.*list->da_set_fcn) (val);
	  else
	    error ("lsode_options: no function to handle vector option");

	  return;
	}
      list++;
    }

  warning ("lsode_options: no match for `%s'", keyword.c_str ());
}

static octave_value_list
show_lsode_option (const std::string& keyword)
{
  octave_value retval;

  LSODE_OPTIONS *list = lsode_option_table;

  while (list->keyword != 0)
    {
      if (keyword_almost_match (list->kw_tok, list->min_len, keyword,
				list->min_toks_to_match, MAX_TOKENS))
	{
	  if (list->da_get_fcn)
	    {
	      Array<double> val = (lsode_opts.*list->da_get_fcn) ();
	      if (val.length () == 1)
		{
		  if (val(0) < 0.0)
		    retval = "computed automatically";
		  else
		    retval = val(0);
		}
	      else
		retval = ColumnVector (val);
	    }
	  else if (list->d_get_fcn)
	    {
	      double val = (lsode_opts.*list->d_get_fcn) ();
	      if (val < 0.0)
		retval = "computed automatically";
	      else
		retval = val;
	    }
	  else
	    {
	      int val = (lsode_opts.*list->i_get_fcn) ();
	      if (val < 0)
		retval = "infinite";
	      else
		retval = static_cast<double> (val);
	    }

	  return retval;
	}
      list++;
    }

  warning ("lsode_options: no match for `%s'", keyword.c_str ());

  return retval;
}

DEFUN_DLD (lsode_options, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {} lsode_options (@var{opt}, @var{val})\n\
When called with two arguments, this function allows you set options\n\
parameters for the function @code{lsode}.  Given one argument,\n\
@code{lsode_options} returns the value of the corresponding option.  If\n\
no arguments are supplied, the names of all the available options and\n\
their current values are displayed.\n\
@end deftypefn")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin == 0)
    {
      print_lsode_option_list (octave_stdout);
    }
  else if (nargin == 1 || nargin == 2)
    {
      std::string keyword = args(0).string_value ();

      if (! error_state)
	{
	  if (nargin == 1)
	    retval = show_lsode_option (keyword);
	  else
	    {
	      if (args(1).is_scalar_type ())
		{
		  double val = args(1).double_value ();

		  if (! error_state)
		    set_lsode_option (keyword, val);
		}
	      else
		{
		  Array<double> val = args(1).vector_value ();

		  if (! error_state)
		    set_lsode_option (keyword, val);
		}
	    }
	}
    }
  else
    print_usage ("lsode_options");

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
