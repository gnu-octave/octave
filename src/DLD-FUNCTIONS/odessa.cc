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

#include <iomanip>
#include <iostream>

#include "ODESSA.h"
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
#include "parse.h"

#include "ODESSA-opts.cc"

// Global pointer for user defined function required by odessa.
static octave_function *odessa_f;
static octave_function *odessa_j;
static octave_function *odessa_b;

// Is this a recursive call?
static int call_depth = 0;

static ColumnVector
odessa_user_f (double t, const ColumnVector& x, const ColumnVector& theta)
{
  ColumnVector retval;

  octave_value_list args;

  int n = x.length ();
  int npar = theta.length ();

  if (npar > 1)
    args(2) = theta;
  else if (npar == 1)
    args(2) = theta(0);
  else
    args(2) = Matrix ();

  if (n > 1)
    args(1) = x;
  else if (n == 1)
    args(1) = x(0);
  else
    args(1) = Matrix ();

  args(0) = t;

  if (odessa_f)
    {
      octave_value_list tmp = odessa_f->do_multi_index_op (1, args);

      if (error_state)
	{
	  gripe_user_supplied_eval ("odessa");
	  return retval;
	}

      if (tmp.length () > 0 && tmp(0).is_defined ())
	{
	  retval = ColumnVector (tmp(0).vector_value ());

	  if (error_state || retval.length () == 0)
	    gripe_user_supplied_eval ("odessa");
	}
      else
	gripe_user_supplied_eval ("odessa");
    }

  return retval;
}

static Matrix
odessa_user_mf (double t, const ColumnVector& x, const ColumnVector& theta,
		octave_function *mf)
{
  Matrix retval;

  if (mf)
    {
      octave_value_list args;

      int n = x.length ();
      int npar = theta.length ();

      if (npar > 1)
	args(2) = theta;
      else if (npar == 1)
	args(2) = theta(0);
      else
	args(2) = Matrix ();

      if (n > 1)
	args(1) = x;
      else if (n == 1)
	args(1) = x(0);
      else
	args(1) = Matrix ();

      args(0) = t;

      octave_value_list tmp = mf->do_multi_index_op (1, args);

      if (error_state)
	{
	  gripe_user_supplied_eval ("odessa");
	  return retval;
	}

      if (tmp.length () > 0 && tmp(0).is_defined ())
	{
	  retval = tmp(0).matrix_value ();

	  if (error_state || retval.length () == 0)
	    gripe_user_supplied_eval ("odessa");
	}
      else
	gripe_user_supplied_eval ("odessa");
    }

  return retval;
}

static Matrix
odessa_user_j (double t, const ColumnVector& x, const ColumnVector& theta)
{
  return odessa_user_mf (t, x, theta, odessa_j);
}

static ColumnVector
odessa_user_b (double t, const ColumnVector& x,
	       const ColumnVector& theta, int column)
{
  ColumnVector retval;

  if (odessa_b)
    {
      octave_value_list args;

      int n = x.length ();
      int npar = theta.length ();

      args(3) = static_cast<double> (column);

      if (npar > 1)
	args(2) = theta;
      else if (npar == 1)
	args(2) = theta(0);
      else
	args(2) = Matrix ();

      if (n > 1)
	args(1) = x;
      else if (n == 1)
	args(1) = x(0);
      else
	args(1) = Matrix ();

      args(0) = t;

      octave_value_list tmp = odessa_b->do_multi_index_op (1, args);

      if (error_state)
	{
	  gripe_user_supplied_eval ("odessa");
	  return retval;
	}

      if (tmp.length () > 0 && tmp(0).is_defined ())
	{
	  retval = ColumnVector (tmp(0).vector_value ());

	  if (error_state || retval.length () == 0)
	    gripe_user_supplied_eval ("odessa");
	}
      else
	gripe_user_supplied_eval ("odessa");
    }

  return retval;
}

static octave_value_list
make_list (const Array<Matrix>& m_array)
{
  octave_value_list retval;

  int len = m_array.length ();

  retval.resize (len);

  for (int i = 0; i < len; i++)
    retval(i) = m_array(i);

  return retval;
}

#define ODESSA_ABORT() \
  do \
    { \
      unwind_protect::run_frame ("Fodessa"); \
      return retval; \
    } \
  while (0)
 
#define ODESSA_ABORT1(msg) \
  do \
    { \
      ::error ("odessa: " msg); \
      ODESSA_ABORT (); \
    } \
  while (0)

#define ODESSA_ABORT2(fmt, arg) \
  do \
    { \
      ::error ("odessa: " fmt, arg); \
      ODESSA_ABORT (); \
    } \
  while (0)

// --------------------------------
// Everthing is so great above here
// --------------------------------

DEFUN_DLD (odessa, args, nargout,
  "odessa (\"f\", x_0, theta, sx_0, t_out, t_crit)\n\
\n\
The string \"f\" may be substituted for the vector of strings\n\
\n\
               [\"f\"; \"j\"; \"b\"] \n\
\n\
You can use the function @code{odessa_options} to set optional\n\
parameters for @code{odessa}.")
{
  octave_value_list retval;

  unwind_protect::begin_frame ("Fodessa");

  unwind_protect_int (call_depth);
  call_depth++;

  if (call_depth > 1)
    ODESSA_ABORT1 ("invalid recursive call");

  int nargin = args.length ();

  if (nargin < 5 || nargin > 6)
    {
      print_usage ("odessa");
      unwind_protect::run_frame ("Fodessa");
      return retval;
    }

  odessa_f = 0;
  odessa_j = 0;
  odessa_b = 0;

  octave_value f_arg = args(0);

  if (f_arg.is_string ())
    {
      string_vector f_str_arg = f_arg.all_strings ();
      
      int len = f_str_arg.length ();
      
      if (len > 2)
	{
	  std::string t = f_str_arg(2);

	  if (t.length () > 0)
	    {
	      odessa_b = is_valid_function (t, "odessa", true);
	      
	      if (! odessa_b)
		ODESSA_ABORT1
		  ("expecting b function name as argument 1");
	    }
	}

      if (len > 1)
	{
	  std::string t = f_str_arg(1);
	  
	  if (t.length () > 0)
	    {
	      odessa_j = is_valid_function (t, "odessa", true);
	      
	      if (! odessa_j)
		ODESSA_ABORT1
		  ("expecting function name as argument 1");
	    }
	}
      
      if (len > 0)
	odessa_f = is_valid_function (f_str_arg(0), "odessa", true);
      
      if (! odessa_f)
	ODESSA_ABORT1 ("expecting function name as argument 1");
    }
  
  ColumnVector state (args(1).vector_value ());

  if (error_state)
    ODESSA_ABORT1 ("expecting state vector as argument 2");

  bool have_parameters = false;
  
  ColumnVector theta;
  Matrix sensitivity_guess;

  if (nargin == 5 || nargin == 6)
    {
      octave_value theta_arg = args(2);

      if (! theta_arg.is_empty ())
	{
	  theta = ColumnVector (theta_arg.vector_value ());
	  
	  if (error_state)
	    ODESSA_ABORT1
	      ("expecting parameter vector as argument 3");
	}

      have_parameters = (theta.length () > 0);
      
      if (have_parameters)
	{
	  sensitivity_guess = args(3).matrix_value ();
	  
	  if (error_state)
	    ODESSA_ABORT1
	      ("expecting sensitivity guess as argument 4");
	  
	  if (sensitivity_guess.rows () != state.length ()
	      || sensitivity_guess.columns () != theta.length ())
	    ODESSA_ABORT1
	      ("incorrect dimension for sensitivity guess");
	}
    }

      ColumnVector out_times (args(4).vector_value ());

      if (error_state)
	ODESSA_ABORT1
	  ("expecting output time vector as %s argument 5");

      ColumnVector crit_times;

      bool crit_times_set = false;

      if (nargin==6)
	{
	  crit_times = ColumnVector (args(5).vector_value ());

	  if (error_state)
	    ODESSA_ABORT1
	      ("expecting critical time vector as argument 6");

	  crit_times_set = true;
	}

      ODESFunc func (odessa_user_f);

      if (odessa_j)
	func.set_jsub_function (odessa_user_j);

      if (odessa_b)
	func.set_bsub_function (odessa_user_b);

      double tzero = out_times (0);

      ODESSA_result output;

      ODESSA ode = have_parameters
	? ODESSA (state, theta, sensitivity_guess, tzero, func)
	: ODESSA (state, tzero, func);
	  
      ode.copy (odessa_opts);

      if (crit_times_set)
	output = ode.integrate (out_times, crit_times);
      else
	output = ode.integrate (out_times);

      if (! error_state)
	{
	  int k = have_parameters ? 3 : 2;

	  std::string msg = ode.error_message ();

	  retval(k--) = msg;
	  retval(k--) = static_cast<double> (ode.integration_state ());

	  if (ode.integration_ok ())
	    {
	      if (have_parameters)
		retval(1) = make_list (output.state_sensitivity ());

	      retval(0) = output.state ();
	    }
	  else
	    {
	      if (have_parameters)
		retval(1) = Matrix ();

	      retval(0) = Matrix ();

	      if ((have_parameters && nargout < 3) || nargout < 2)
		error ("odessa: %s", msg.c_str ());
	    }
	}

  unwind_protect::run_frame ("Fodessa");

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
