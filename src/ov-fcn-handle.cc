/*

Copyright (C) 2003 John W. Eaton

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

#if defined (__GNUG__) && defined (USE_PRAGMA_INTERFACE_IMPLEMENTATION)
#pragma implementation
#endif

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <iostream>

#include "defun.h"
#include "error.h"
#include "gripes.h"
#include "oct-map.h"
#include "ov-base.h"
#include "ov-base-mat.h"
#include "ov-base-mat.cc"
#include "ov-fcn-handle.h"
#include "pr-output.h"
#include "variables.h"

// Instantiate Arrays of fcn_handle_elt values.

#include "Array.h"
#include "Array.cc"

INSTANTIATE_ARRAY_AND_ASSIGN (fcn_handle_elt);

#include "Array2.h"

template class Array2<fcn_handle_elt>;

#include "ArrayN.h"
#include "ArrayN.cc"

template class ArrayN<fcn_handle_elt>;

template class octave_base_matrix<fcn_handle_array>;

boolNDArray
fcn_handle_array::all (int) const
{
  error ("all: invalid call for function handle object");
  return boolNDArray ();
}

boolNDArray
fcn_handle_array::any (int) const
{
  error ("any: invalid call for function handle object");
  return boolNDArray ();
}

DEFINE_OCTAVE_ALLOCATOR (octave_fcn_handle);

DEFINE_OV_TYPEID_FUNCTIONS_AND_DATA (octave_fcn_handle,
				     "function handle",
				     "function handle");

octave_function *
octave_fcn_handle::function_value (bool)
{
  octave_function *retval = 0;

  if (numel () > 0)
    {
      // XXX FIXME XXX -- is warn_fortran_indexing the right variable here?
      if (Vwarn_fortran_indexing)
	gripe_implicit_conversion ("function handle array",
				   "scalar function handle");

      fcn_handle_elt elt = matrix(0);

      retval = elt.function_value ();
    }
  else
    gripe_invalid_conversion ("function handle array",
			      "scalar function handle");

  return retval;
}

std::string
octave_fcn_handle::name (void) const
{
  std::string retval;

  if (numel () > 0)
    {
      // XXX FIXME XXX -- is warn_fortran_indexing the right variable here?
      if (Vwarn_fortran_indexing)
	gripe_implicit_conversion ("function handle array",
				   "scalar function handle");

      fcn_handle_elt elt = matrix(0);

      retval = elt.name ();
    }
  else
    gripe_invalid_conversion ("function handle array",
			      "scalar function handle");

  return retval;
}

ArrayN<std::string>
fcn_handle_array::names (void) const
{
  ArrayN<std::string> retval (dims ());

  int nel = length ();

  for (int i = 0; i < nel; i++)
    {
      fcn_handle_elt elt = elem (i);

      retval(i) = elt.name ();
    }

  return retval;
}

void
octave_fcn_handle::print (std::ostream& os, bool pr_as_read_syntax) const
{
  print_raw (os, pr_as_read_syntax);
  newline (os);
}

void
octave_fcn_handle::print_raw (std::ostream& os, bool pr_as_read_syntax) const
{
  octave_print_internal (os, name_array (), pr_as_read_syntax,
			 current_print_indent_level ());
}

octave_value
make_fcn_handle (const std::string& nm)
{
  octave_value retval;

  octave_function *f = lookup_function (nm);

  if (f)
    return fcn_handle_array (f, nm);
  else
    error ("error creating function handle \"@%s\"", nm.c_str ());

  return retval;
}

DEFUN (functions, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} functions (@var{fcn_handle})\n\
Return a struct containing information about the function handle\n\
@var{fcn_handle}.\n\
@end deftypefn")
{
  octave_value retval;

  if (args.length () == 1)
    {
      octave_fcn_handle *fh = args(0).fcn_handle_value ();

      if (! error_state)
	{
	  octave_function *fcn = fh ? fh->function_value (true) : 0;

	  if (fcn)
	    {
	      Octave_map m;

	      std::string fh_nm = fh->name ();

	      m ["function"](0) = fh_nm.substr (1);

	      if (fcn->is_nested_function ())
		m ["type"](0) = "subfunction";
	      else
		m ["type"](0) = "simple";

	      std::string nm = fcn->fcn_file_name ();

	      if (nm.empty ())
		m ["file"](0) = "built-in function";
	      else
		m ["file"](0) = nm;

	      retval = m;
	    }
	  else
	    error ("functions: invalid function handle object");
	}
      else
	error ("functions: argument must be a function handle object");
    }
  else
    print_usage ("functions");

  return retval;
}

DEFUN (func2str, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} func2str (@var{fcn_handle})\n\
Return a string containing the name of the function referenced by\n\
the function handle @var{fcn_handle}.\n\
@end deftypefn")
{
  octave_value retval;

  if (args.length () == 1)
    {
      octave_fcn_handle *fh = args(0).fcn_handle_value ();

      if (! error_state && fh)
	{
	  std::string fh_nm = fh->name ();
	  retval = fh_nm.substr (1);
	}
      else
	error ("func2str: expecting valid function handle as first argument");
    }
  else
    print_usage ("func2str");

  return retval;
}

DEFUN (str2func, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} str2func (@var{fcn_name})\n\
Return a function handle constructed from the string @var{fcn_name}.\n\
@end deftypefn")
{
  octave_value retval;

  if (args.length () == 1)
    {
      std::string nm = args(0).string_value ();

      if (! error_state)
	retval = make_fcn_handle (nm);
      else
	error ("str2func: expecting string as first argument");
    }
  else
    print_usage ("str2func");

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
