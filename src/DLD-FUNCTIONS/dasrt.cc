/*

Copyright (C) 2002 John W. Eaton

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

#include "DASRT.h"
#include "lo-mappers.h"

#include "defun-dld.h"
#include "error.h"
#include "gripes.h"
#include "oct-obj.h"
#include "ov-fcn.h"
#include "pager.h"
#include "parse.h"
#include "unwind-prot.h"
#include "utils.h"
#include "variables.h"

// Global pointers for user defined function required by dassl.
static octave_function *dasrt_f;
static octave_function *dasrt_j;
static octave_function *dasrt_cf;

static DASRT_options dasrt_opts;

// Is this a recursive call?
static int call_depth = 0;

static ColumnVector
dasrt_user_f (const ColumnVector& x, const ColumnVector& xprime,
	      double t, int& ires)
{
  ColumnVector retval;

  octave_value_list args;

  int n = x.length ();

  args(2) = t;

  if (n > 1)
    {
      args(1) = xprime;
      args(0) = x;
    }
  else if (n == 1)
    {
      args(1) = xprime(0);
      args(0) = x(0);
    }
  else
    {
      args(1) = Matrix ();
      args(0) = Matrix ();
    }

  if (dasrt_f)
    {
      octave_value_list tmp = dasrt_f->do_multi_index_op (1, args);

      if (error_state)
	{
	  gripe_user_supplied_eval ("dasrt");
	  return retval;
	}

      if (tmp.length () > 0 && tmp(0).is_defined ())
	{
	  retval = ColumnVector (tmp(0).vector_value ());

	  if (error_state || retval.length () == 0)
	    gripe_user_supplied_eval ("dasrt");
	}
      else
	gripe_user_supplied_eval ("dasrt");
    }

  return retval;
}

static ColumnVector
dasrt_user_cf (const ColumnVector& x, double t)
{
  ColumnVector retval;

  octave_value_list args;

  int n = x.length ();

  if (n > 1)
    args(0) = x;
  else if (n == 1)
    args(0) = x(0);
  else
    args(0) = Matrix ();

  args(1) = t;

  if (dasrt_cf)
    {
      octave_value_list tmp = dasrt_cf->do_multi_index_op (1, args);

      if (error_state)
	{
	  gripe_user_supplied_eval ("dasrt");
	  return retval;
	}

      if (tmp.length () > 0 && tmp(0).is_defined ())
	{
	  retval = ColumnVector (tmp(0).vector_value ());

	  if (error_state || retval.length () == 0)
	    gripe_user_supplied_eval ("dasrt");
	}
      else
	gripe_user_supplied_eval ("dasrt");
    }

  return retval;
}

static Matrix
dasrt_user_j (const ColumnVector& x, const ColumnVector& xdot,
	      double t, double cj)
{
  Matrix retval;

  int nstates = x.capacity ();

  assert (nstates == xdot.capacity ());

  octave_value_list args;

  args(3) = cj;
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

  if (dasrt_j)
    {
      octave_value_list tmp = dasrt_j->do_multi_index_op (1, args);

      if (error_state)
	{
	  gripe_user_supplied_eval ("dasrt");
	  return retval;
	}

      int tlen = tmp.length ();
      if (tlen > 0 && tmp(0).is_defined ())
	{
	  retval = tmp(0).matrix_value ();

	  if (error_state || retval.length () == 0)
	    gripe_user_supplied_eval ("dasrt");
	}
      else
	gripe_user_supplied_eval ("dasrt");
    }

  return retval;
}

#define DASRT_ABORT \
  do \
    { \
      unwind_protect::run_frame ("Fdasrt"); \
      return retval; \
    } \
  while (0)

#define DASRT_ABORT1(msg) \
  do \
    { \
      ::error ("dasrt: " ## msg); \
      DASRT_ABORT; \
    } \
  while (0)

#define DASRT_ABORT2(fmt, arg) \
  do \
    { \
      ::error ("dasrt: " ## fmt, arg); \
      DASRT_ABORT; \
    } \
  while (0)

DEFUN_DLD (dasrt, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {[@var{x}, @var{xdot}, @var{t}] =} dasrt (@var{fj} [, @var{g}], @var{x_0}, @var{xdot_0}, @var{t_out} [, @var{t_crit}])\n\
Solve a system of differential/algebraic equations with functional\n\
stopping criteria.\n\
\n\
The function to be integrated must be of the form:\n\
@example\n\
@var{res} = f (@var{x}, @var{xdot}, @var{t}) = 0\n\
@end example\n\
\n\
The stopping condition must be of the form:\n\
\n\
@example\n\
@var{res} = g (@var{x}, @var{t}) = 0\n\
@end example\n\
\n\
The Jacobian (if included) must be of the form:\n\
\n\
@example\n\
@var{jac} = j (@var{x}, @var{xdot}, @var{t}, @var{cj})\n\
   =  df/dx + cj*df/dxdot\n\
@end example\n\
\n\
@noindent\n\
The following inputs are entered:\n\
\n\
@table @var\n\
@item fj\n\
The string vector containing either @var{f} or both @var{f} and @var{j}.\n\
\n\
@item f\n\
The function to be integrated.\n\
\n\
@item g\n\
The function with the stopping conditions.\n\
\n\
@item j\n\
The optional Jacobian function.  If not included, it will be approximated\n\
by finite differences.\n\
\n\
@item x_0\n\
The initial state.\n\
\n\
@item xdot_0\n\
The time derivative of the initial state.\n\
\n\
@item t_out\n\
The times at which the solution should be returned.  This vector should\n\
include the initial time a the first value.\n\
\n\
@item t_crit\n\
The times at which the function is non-smooth or poorly behaved.\n\
\n\
@end table\n\
\n\
@noindent\n\
The following outputs are returned:\n\
\n\
@table @var\n\
@item x\n\
The states at each time in @var{t}.\n\
\n\
@item xdot\n\
The time derivative of the states at each time in @var{t}.\n\
\n\
@item t\n\
All the times in the requested output time vector up to the stopping\n\
criteria.  The time at which the stopping criteria is achieved is returned\n\
as the last time in the vector.\n\
@end table\n\
\n\
You can use the function @code{dasrt_options} to set optional\n\
parameters for @code{dasrt}.\n\
@end deftypefn")
{
  octave_value_list retval;

  unwind_protect::begin_frame ("Fdasrt");

  unwind_protect_int (call_depth);
  call_depth++;

  if (call_depth > 1)
    DASRT_ABORT1 ("invalid recursive call");

  int argp = 0;

  int nargin = args.length ();

  if (nargin < 4 || nargin > 6)
    {
      print_usage ("dasrt");
      unwind_protect::run_frame ("Fdasrt");
      return retval;
    }

  dasrt_f = 0;
  dasrt_j = 0;
  dasrt_cf = 0;

  // Check all the arguments.  Are they the right animals?

  // Here's where I take care of f and j in one shot:

  octave_value f_arg = args(0);

  switch (f_arg.rows ())
    {
    case 1:
      dasrt_f = extract_function
	(args(0), "dasrt", "__dasrt_fcn__",
	 "function res = __dasrt_fcn__ (x, xdot, t) res = ",
	 "; endfunction");
      break;
      
    case 2:
      {
	string_vector tmp = args(0).all_strings ();
	
	if (! error_state)
	  {
	    dasrt_f = extract_function
	      (tmp(0), "dasrt", "__dasrt_fcn__",
	       "function res = __dasrt_fcn__ (x, xdot, t) res = ",
	       "; endfunction");
	    
	    if (dasrt_f)
	      {
		dasrt_j = extract_function
		  (tmp(1), "dasrt", "__dasrt_jac__",
		   "function jac = __dasrt_jac__ (x, xdot, t, cj) jac = ",
		   "; endfunction");
		
		if (! dasrt_j)
		  dasrt_f = 0;
	      }
	  }
      }
      break;
      
    default:
      DASRT_ABORT1
	("first arg should be a string or 2-element string array");
    }
  
  if (error_state || (! dasrt_f))
    DASRT_ABORT;
  
  DAERTFunc func (dasrt_user_f);
  
  argp++;
  
  if (args(1).is_string ())
    {
      dasrt_cf = is_valid_function (args(1), "dasrt", true);

      if (! dasrt_cf)
	DASRT_ABORT1 ("expecting function name as argument 2");

      argp++;

      func.set_constraint_function (dasrt_user_cf);
    }

  ColumnVector state (args(argp++).vector_value ());

  if (error_state)
    DASRT_ABORT2 ("expecting state vector as argument %d", argp);

  ColumnVector stateprime (args(argp++).vector_value ());

  if (error_state)
    DASRT_ABORT2 
       ("expecting time derivative of state vector as argument %d", argp);

  ColumnVector out_times (args(argp++).vector_value ());

  if (error_state)
    DASRT_ABORT2
	("expecting output time vector as %s argument %d", argp);

  double tzero = out_times (0);

  ColumnVector crit_times;

  bool crit_times_set = false;

  if (argp < nargin)
    {
      crit_times = ColumnVector (args(argp++).vector_value ());

      if (error_state)
	DASRT_ABORT2
	  ("expecting critical time vector as argument %d", argp);

      crit_times_set = true;
    }

  if (dasrt_j)
    func.set_jacobian_function (dasrt_user_j);

  DASRT_result output;

  DASRT dae = DASRT (state, stateprime, tzero, func);

  dae.copy (dasrt_opts);

  if (crit_times_set)
    output = dae.integrate (out_times, crit_times);
  else
    output = dae.integrate (out_times);

  if (! error_state)
    {
      std::string msg = dae.error_message ();

      retval(4) = msg;
      retval(3) = static_cast<double> (dae.integration_state ());

      if (dae.integration_ok ())
	{
	  retval(2) = output.times ();
	  retval(1) = output.deriv ();
	  retval(0) = output.state ();
	}
      else
	{
	  retval(2) = Matrix ();
	  retval(1) = Matrix ();
	  retval(0) = Matrix ();

	  if (nargout < 4)
	    error ("dasrt: %s", msg.c_str ());
	}
    }

  unwind_protect::run_frame ("Fdasrt");

  return retval;
}

typedef void (DASRT_options::*d_set_opt_mf) (double);
typedef void (DASRT_options::*i_set_opt_mf) (int);
typedef double (DASRT_options::*d_get_opt_mf) (void);
typedef int (DASRT_options::*i_get_opt_mf) (void);

#define MAX_TOKENS 3

struct DASRT_OPTIONS
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

static DASRT_OPTIONS dasrt_option_table [] =
{
  { "absolute tolerance",
    { "absolute", "tolerance", 0, 0, },
    { 1, 0, 0, 0, }, 1,
    &DASRT_options::set_absolute_tolerance, 0,
    &DASRT_options::absolute_tolerance, 0, },

  { "initial step size",
    { "initial", "step", "size", 0, },
    { 1, 0, 0, 0, }, 1,
    &DASRT_options::set_initial_step_size, 0,
    &DASRT_options::initial_step_size, 0, },

  { "maximum step size",
    { "maximum", "step", "size", 0, },
    { 2, 0, 0, 0, }, 1,
    &DASRT_options::set_maximum_step_size, 0,
    &DASRT_options::maximum_step_size, 0, },

  { "minimum step size",
    { "minimum", "step", "size", 0, },
    { 2, 0, 0, 0, }, 1,
    &DASRT_options::set_minimum_step_size, 0,
    &DASRT_options::minimum_step_size, 0, },

  { "relative tolerance",
    { "relative", "tolerance", 0, 0, },
    { 1, 0, 0, 0, }, 1,
    &DASRT_options::set_relative_tolerance, 0,
    &DASRT_options::relative_tolerance, 0, },

  { "step limit",
    { "step", "limit", 0, 0, },
    { 1, 0, 0, 0, }, 1,
    0, &DASRT_options::set_step_limit,
    0, &DASRT_options::step_limit, },

  { 0,
    { 0, 0, 0, 0, },
    { 0, 0, 0, 0, }, 0,
    0, 0, 0, 0, },
};

static void
print_dasrt_option_list (ostream& os)
{
  print_usage ("dasrt_options", 1);

  os << "\n"
     << "Options for dasrt include:\n\n"
     << "  keyword                                  value\n"
     << "  -------                                  -----\n\n";

  DASRT_OPTIONS *list = dasrt_option_table;

  const char *keyword;
  while ((keyword = list->keyword) != 0)
    {
      os.form ("  %-40s ", keyword);

      if (list->d_get_fcn)
	{
	  double val = (dasrt_opts.*list->d_get_fcn) ();
	  if (val < 0.0)
	    os << "computed automatically";
	  else
	    os << val;
	}
      else
	{
	  int val = (dasrt_opts.*list->i_get_fcn) ();
	  if (val < 0)
	    os << "computed automatically";
	  else
	    os << val;
	}
      os << "\n";
      list++;
    }

  os << "\n";
}

static void
set_dasrt_option (const string& keyword, double val)
{
  DASRT_OPTIONS *list = dasrt_option_table;

  while (list->keyword != 0)
    {
      if (keyword_almost_match (list->kw_tok, list->min_len, keyword,
				list->min_toks_to_match, MAX_TOKENS))
	{
	  if (list->d_set_fcn)
	    (dasrt_opts.*list->d_set_fcn) (val);
	  else
	    {
	      if (xisnan (val))
		{
		  error ("dasrt_options: %s: expecting integer, found NaN",
			 keyword.c_str ());
		}
	      else
		(dasrt_opts.*list->i_set_fcn) (NINT (val));
	    }
	  return;
	}
      list++;
    }

  warning ("dasrt_options: no match for `%s'", keyword.c_str ());
}

static octave_value_list
show_dasrt_option (const string& keyword)
{
  octave_value retval;

  DASRT_OPTIONS *list = dasrt_option_table;

  while (list->keyword != 0)
    {
      if (keyword_almost_match (list->kw_tok, list->min_len, keyword,
				list->min_toks_to_match, MAX_TOKENS))
	{
	  if (list->d_get_fcn)
	    {
	      double val = (dasrt_opts.*list->d_get_fcn) ();
	      if (val < 0.0)
		retval = "computed automatically";
	      else
		retval = val;
	    }
	  else
	    {
	      int val = (dasrt_opts.*list->i_get_fcn) ();
	      if (val < 0)
		retval = "computed automatically";
	      else
		retval = static_cast<double> (val);
	    }

	  return retval;
	}
      list++;
    }

  warning ("dasrt_options: no match for `%s'", keyword.c_str ());

  return retval;
}

DEFUN_DLD (dasrt_options, args, ,
  "dasrt_options (KEYWORD, VALUE)\n\
\n\
Set or show options for dasrt.  Keywords may be abbreviated\n\
to the shortest match.")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin == 0)
    {
      print_dasrt_option_list (octave_stdout);
      return retval;
    }
  else if (nargin == 1 || nargin == 2)
    {
      string keyword = args(0).string_value ();

      if (! error_state)
	{
	  if (nargin == 1)
	    return show_dasrt_option (keyword);
	  else
	    {
	      double val = args(1).double_value ();

	      if (! error_state)
		{
		  set_dasrt_option (keyword, val);
		  return retval;
		}
	    }
	}
    }

  print_usage ("dasrt_options");

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
