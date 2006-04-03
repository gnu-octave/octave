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
Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <string>

#include <iomanip>
#include <iostream>

#include "NLEqn.h"

#include "defun-dld.h"
#include "error.h"
#include "gripes.h"
#include "oct-obj.h"
#include "ov-fcn.h"
#include "ov-cell.h"
#include "pager.h"
#include "unwind-prot.h"
#include "utils.h"
#include "variables.h"

#include "NLEqn-opts.cc"

// Global pointer for user defined function required by hybrd1.
static octave_function *fsolve_fcn;

// Global pointer for optional user defined jacobian function.
static octave_function *fsolve_jac;

// Have we warned about imaginary values returned from user function?
static bool warned_fcn_imaginary = false;
static bool warned_jac_imaginary = false;

// Is this a recursive call?
static int call_depth = 0;

octave_idx_type
hybrd_info_to_fsolve_info (octave_idx_type info)
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

  octave_idx_type n = x.length ();

  octave_value_list args;
  args.resize (1);

  if (n > 1)
    {
      Matrix m (n, 1);
      for (octave_idx_type i = 0; i < n; i++)
	m (i, 0) = x (i);
      octave_value vars (m);
      args(0) = vars;
    }
  else
    {
      double d = x (0);
      octave_value vars (d);
      args(0) = vars;
    }

  if (fsolve_fcn)
    {
      octave_value_list tmp = fsolve_fcn->do_multi_index_op (1, args);

      if (tmp.length () > 0 && tmp(0).is_defined ())
	{
	  if (! warned_fcn_imaginary && tmp(0).is_complex_type ())
	    {
	      warning ("fsolve: ignoring imaginary part returned from user-supplied function");
	      warned_fcn_imaginary = true;
	    }

	  retval = ColumnVector (tmp(0).vector_value ());

	  if (error_state || retval.length () <= 0)
	    gripe_user_supplied_eval ("fsolve");
	}
      else
	gripe_user_supplied_eval ("fsolve");
    }

  return retval;
}

Matrix
fsolve_user_jacobian (const ColumnVector& x)
{
  Matrix retval;

  octave_idx_type n = x.length ();

  octave_value_list args;
  args.resize (1);

  if (n > 1)
    {
      Matrix m (n, 1);
      for (octave_idx_type i = 0; i < n; i++)
	m(i,0) = x(i);
      octave_value vars (m);
      args(0) = vars;
    }
  else
    {
      double d = x(0);
      octave_value vars (d);
      args(0) = vars;
    }

  if (fsolve_fcn)
    {
      octave_value_list tmp = fsolve_jac->do_multi_index_op (1, args);

      if (tmp.length () > 0 && tmp(0).is_defined ())
	{
	  if (! warned_fcn_imaginary && tmp(0).is_complex_type ())
	    {
	      warning ("fsolve: ignoring imaginary part returned from user-supplied jacobian function");
	      warned_fcn_imaginary = true;
	    }

	  retval = tmp(0).matrix_value ();

	  if (error_state || retval.length () <= 0)
	    gripe_user_supplied_eval ("fsolve");
	}
      else
	gripe_user_supplied_eval ("fsolve");
    }

  return retval;
}

#define FSOLVE_ABORT() \
  do \
    { \
      unwind_protect::run_frame ("Ffsolve"); \
      return retval; \
    } \
  while (0)

#define FSOLVE_ABORT1(msg) \
  do \
    { \
      ::error ("fsolve: " msg); \
      FSOLVE_ABORT (); \
    } \
  while (0)

#define FSOLVE_ABORT2(fmt, arg) \
  do \
    { \
      ::error ("fsolve: " fmt, arg); \
      FSOLVE_ABORT (); \
    } \
  while (0)

DEFUN_DLD (fsolve, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {[@var{x}, @var{info}, @var{msg}] =} fsolve (@var{fcn}, @var{x0})\n\
Given @var{fcn}, the name of a function of the form @code{f (@var{x})}\n\
and an initial starting point @var{x0}, @code{fsolve} solves the set of\n\
equations such that @code{f(@var{x}) == 0}.\n\
\n\
If @var{fcn} is a two-element string array, or a two element cell array\n\
containing either the function name or inline or function handle. The\n\
first element names the function @math{f} described above, and the second\n\
element names a function of the form @code{j (@var{x})} to compute the\n\
Jacobian matrix with elements\n\
@tex\n\
$$ J = {\\partial f_i \\over \\partial x_j} $$\n\
@end tex\n\
@ifinfo\n\
\n\
@example\n\
           df_i\n\
jac(i,j) = ----\n\
           dx_j\n\
@end example\n\
@end ifinfo\n\
\n\
You can use the function @code{fsolve_options} to set optional\n\
parameters for @code{fsolve}.\n\
@end deftypefn")
{
  octave_value_list retval;

  warned_fcn_imaginary = false;
  warned_jac_imaginary = false;

  unwind_protect::begin_frame ("Ffsolve");

  unwind_protect_int (call_depth);
  call_depth++;

  if (call_depth > 1)
    FSOLVE_ABORT1 ("invalid recursive call");

  int nargin = args.length ();

  if (nargin == 2 && nargout < 4)
    {
      std::string fcn_name, fname, jac_name, jname;
      fsolve_fcn = 0;
      fsolve_jac = 0;

      octave_value f_arg = args(0);

      if (f_arg.is_cell ())
  	{
	  Cell c = f_arg.cell_value ();
	  if (c.length() == 1)
	    f_arg = c(0);
	  else if (c.length() == 2)
	    {
	      if (c(0).is_function_handle () || c(0).is_inline_function ())
		fsolve_fcn = c(0).function_value ();
	      else
		{
		  fcn_name = unique_symbol_name ("__fsolve_fcn__");
		  fname = "function y = ";
		  fname.append (fcn_name);
		  fname.append (" (x) y = ");
		  fsolve_fcn = extract_function
		    (c(0), "fsolve", fcn_name, fname, "; endfunction");
		}
	      
	      if (fsolve_fcn)
		{
		  if (c(1).is_function_handle () || c(1).is_inline_function ())
		    fsolve_jac = c(1).function_value ();
		  else
		    {
		      jac_name = unique_symbol_name ("__fsolve_jac__");
		      jname = "function y = ";
		      jname.append (jac_name);
		      jname.append (" (x) jac = ");
		      fsolve_jac = extract_function
			(c(1), "fsolve", jac_name, jname, "; endfunction");

		      if (!fsolve_jac)
			{
			  if (fcn_name.length())
			    clear_function (fcn_name);
			  fsolve_fcn = 0;
			}
		    }
		}
	    }
	  else
	    FSOLVE_ABORT1 ("incorrect number of elements in cell array");
	}

      if (!fsolve_fcn && ! f_arg.is_cell())
	{
	  if (f_arg.is_function_handle () || f_arg.is_inline_function ())
	    fsolve_fcn = f_arg.function_value ();
	  else
	    {
	      switch (f_arg.rows ())
		{
		case 1:
		  do
		    {
		      fcn_name = unique_symbol_name ("__fsolve_fcn__");
		      fname = "function y = ";
		      fname.append (fcn_name);
		      fname.append (" (x) y = ");
		      fsolve_fcn = extract_function
			(f_arg, "fsolve", fcn_name, fname, "; endfunction");
		    }
		  while (0);
		  break;

		case 2:
		  {
		    string_vector tmp = f_arg.all_strings ();

		    if (! error_state)
		      {
			fcn_name = unique_symbol_name ("__fsolve_fcn__");
			fname = "function y = ";
			fname.append (fcn_name);
			fname.append (" (x) y = ");
			fsolve_fcn = extract_function
			  (tmp(0), "fsolve", fcn_name, fname, "; endfunction");

			if (fsolve_fcn)
			  {
			    jac_name = unique_symbol_name ("__fsolve_jac__");
			    jname = "function y = ";
			    jname.append (jac_name);
			    jname.append (" (x) jac = ");
			    fsolve_jac = extract_function
			      (tmp(1), "fsolve", jac_name, jname, 
			       "; endfunction");

			    if (!fsolve_jac)
			      {
				if (fcn_name.length())
				  clear_function (fcn_name);
				fsolve_fcn = 0;
			      }
			  }
		      }
		  }
		}
	    }
	}

      if (error_state || ! fsolve_fcn)
	FSOLVE_ABORT ();

      ColumnVector x (args(1).vector_value ());

      if (error_state)
	FSOLVE_ABORT1 ("expecting vector as second argument");

      if (nargin > 3)
	warning ("fsolve: ignoring extra arguments");

      if (nargout > 3)
	warning ("fsolve: can't compute path output yet");

      NLFunc nleqn_fcn (fsolve_user_function);
      if (fsolve_jac)
	nleqn_fcn.set_jacobian_function (fsolve_user_jacobian);

      NLEqn nleqn (x, nleqn_fcn);
      nleqn.set_options (fsolve_opts);

      octave_idx_type info;
      ColumnVector soln = nleqn.solve (info);

      if (fcn_name.length())
	clear_function (fcn_name);
      if (jac_name.length())
	clear_function (jac_name);

      if (! error_state)
	{
	  std::string msg = nleqn.error_message ();

	  retval(2) = msg;
	  retval(1) = static_cast<double> (hybrd_info_to_fsolve_info (info));

	  retval(0) = soln;

	  if (! nleqn.solution_ok () && nargout < 2)
	    error ("fsolve: %s", msg.c_str ());
	}
    }
  else
    print_usage ("fsolve");

  unwind_protect::run_frame ("Ffsolve");

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
