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
#include "ov-fcn-handle.h"
#include "pr-output.h"
#include "variables.h"
#include "parse.h"

DEFINE_OCTAVE_ALLOCATOR (octave_fcn_handle);

DEFINE_OV_TYPEID_FUNCTIONS_AND_DATA (octave_fcn_handle,
				     "function handle",
				     "function handle");

octave_fcn_handle::octave_fcn_handle (const std::string& f,
				      const string_vector& a, 
				      const std::string& n)
  : typ (fcn_inline), nm (n), iftext (f), ifargs (a) 
{
  // Find a function name that isn't already in the symbol table.

  std::string fname = "__inline__";

  while (symbol_exist (fname))
    fname.append ("X");

  // Form a string representing the function. 

  OSSTREAM buf;

  buf << "function __retval__ = " << fname << "(";

  for (int i = 0; i < ifargs.length (); i++)
    {
      if (i > 0)
	buf << ", ";

      buf << ifargs(i);
    }

  buf << ")\n  __retval__ = " << iftext << ";\nendfunction" << OSSTREAM_ENDS;
  
  // Parse this function and create a user function.

  octave_value eval_args (OSSTREAM_STR (buf)); 

  feval ("eval", eval_args, 0);

  OSSTREAM_FREEZE (buf);

  octave_value tmp = lookup_function (fname);

  if (tmp.is_function ())
    {
      fcn = tmp;

      // XXX FIXME XXX -- probably shouldn't be directly altering the
      // symbol table here.

      fbi_sym_tab->clear_function (fname);
    }
  else
    error ("inline: unable to define function");
}

octave_value_list
octave_fcn_handle::subsref (const std::string& type,
			    const std::list<octave_value_list>& idx,
			    int nargout)
{
  octave_value_list retval;

  switch (type[0])
    {
    case '(':
      {
	octave_function *f = function_value ();
	retval = f->subsref (type, idx, nargout);
      }
      break;


    case '{':
    case '.':
      {
	std::string typ_nm = type_name ();
	error ("%s cannot be indexed with %c", typ_nm.c_str (), type[0]);
      }
      break;

    default:
      panic_impossible ();
    }

  // XXX FIXME XXX -- perhaps there should be an
  // octave_value_list::next_subsref member function?  See also
  // octave_builtin::subsref.

  if (idx.size () > 1)
    retval = retval(0).next_subsref (type, idx);

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
  if (is_inline ())
    {
      OSSTREAM buf;

      if (nm.empty ())
	buf << "@f(";
      else
	buf << nm << "(";

      for (int i = 0; i < ifargs.length (); i++)
	{
	  if (i)
	    buf << ", ";

	  buf << ifargs(i);
	}

      buf << ") = " << iftext << OSSTREAM_ENDS;

      octave_print_internal (os, OSSTREAM_STR (buf), pr_as_read_syntax,
			     current_print_indent_level ());
      OSSTREAM_FREEZE (buf);
    }
  else
    octave_print_internal (os, nm, pr_as_read_syntax,
			   current_print_indent_level ());
}

octave_value
octave_fcn_handle::convert_to_str_internal (bool, bool) const
{
  octave_value retval;

  if (is_inline ())
    retval = octave_value (inline_fcn_text ());
  else
    error ("convert_to_str_internal: must be an inline function");

  return retval;
}

octave_value
make_fcn_handle (const std::string& nm)
{
  octave_value retval;

  octave_value f = lookup_function (nm);

  if (f.is_function ())
    retval = octave_value (new octave_fcn_handle (f, nm));
  else
    error ("error creating function handle \"@%s\"", nm.c_str ());

  return retval;
}

DEFUN (inline, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} inline (@var{str})\n\
@deftypefnx {Built-in Function} {} inline (@var{str}, @var{arg1}, ...)\n\
@deftypefnx {Built-in Function} {} inline (@var{str}, @var{n})\n\
Define a function from a string @var{str}.\n\
\n\
Create an inline function.  Called with a single argument, the\n\
function is assumed to have a single argument and will be defined\n\
as the first isolated lower case character, except i or j.\n\
\n\
If the second and subsequent arguments are strings, they are the names of\n\
the arguments of the function.\n\
\n\
If the second argument is an integer @var{n}, the arguments are\n\
@code{\"x\"}, @code{\"P1\"}, @dots{}, @code{\"P@var{N}\"}.\n\
@end deftypefn\n\
@seealso{argnames, formula, vectorize}")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin > 0)
    {
      std::string fun = args(0).string_value ();

      if (! error_state)
	{
	  string_vector fargs;

	  if (nargin == 1)
	    {
	      fargs.resize (1);

	      // Find the first isolated string as the argument of the
	      // function.

	      // XXX FIXME XXX -- use just "x" for now.
	      fargs(0) = "x";
	    }
	  else if (nargin == 2 && args(1).is_numeric_type ())
	    {
	      int n = args(1).int_value ();

	      if (! error_state)
		{
		  if (n >= 0)
		    {
		      fargs.resize (n+1);

		      fargs(0) = "x";

		      for (int i = 1; i < n+1; i++)
			{
			  OSSTREAM buf;
			  buf << "P" << i << OSSTREAM_ENDS;
			  fargs(i) = OSSTREAM_STR (buf);
			  OSSTREAM_FREEZE (buf);
			}
		    }
		  else
		    {
		      error ("inline: numeric argument must be nonnegative");
		      return retval;
		    }
		}
	      else
		{
		  error ("inline: expecting second argument to be an integer");
		  return retval;
		}
	    }
	  else
	    {
	      fargs.resize (nargin - 1);

	      for (int i = 1; i < nargin; i++)
		{
		  std::string s = args(i).string_value ();

		  if (! error_state)
		    fargs(i-1) = s;
		  else
		    {
		      error ("inline: expecting string arguments");
		      return retval;
		    }
		}
	    }

	  retval = octave_value (new octave_fcn_handle (fun, fargs));
	}
      else
	error ("inline: first argument must be a string");
    }
  else
    print_usage ("inline");

  return retval;
}

DEFUN (formula, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} formula (@var{fun})\n\
Return a string representing the inline function @var{fun}.\n\
@end deftypefn\n\
@seealso{argnames, inline, vectorize}")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin == 1)
    {
      octave_fcn_handle* fn = args(0).fcn_handle_value (true);

      if (fn && fn->is_inline ())
	retval = octave_value (fn->inline_fcn_text ());
      else
	error ("formula: must be an inline function");
    }
  else
    print_usage ("formula");

  return retval;
}

DEFUN (argnames, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} argnames (@var{fun})\n\
Return a cell array of strings containing the names of the arguments\n\
of the inline function @var{fun}.\n\
@end deftypefn\n\
@seealso{argnames, inline, formula, vectorize}")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin == 1)
    {
      octave_fcn_handle *fn = args(0).fcn_handle_value (true);

      if (fn && fn->is_inline ())
	{
	  string_vector t1 = fn->inline_fcn_arg_names ();

	  Cell t2 (dim_vector (t1.length (), 1));

	  for (int i = 0; i < t1.length (); i++)
	    t2(i) = t1(i);

	  retval = t2;
	}
      else
	error ("argnames: argument must be an inline function");
    }
  else
    print_usage ("argnames");

  return retval;
}

DEFUN (vectorize, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} argnames (@var{fun})\n\
Create a vectorized version of the inline function @var{fun}\n\
by replacing all occurrences of @code{*}, @code{/}, etc., with\n\
@code{.*}, @code{./}, etc.\n\
@end deftypefn\n\
@seealso{argnames, inline, formula, vectorize}")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin == 1)
    {
      octave_fcn_handle* old = args(0).fcn_handle_value (true);

      if (old && old->is_inline ())
	{
	  std::string old_func = old->inline_fcn_text ();
	  std::string new_func;

	  size_t i = 0;

	  while (i < old_func.length ())
	    {
	      std::string t1 = old_func.substr (i, 1);

	      if (t1 == "*" || t1 == "/" || t1 == "\\" || t1 == "^")
		{
		  if (i && old_func.substr (i-1, 1) != ".")
		    new_func.append (".");

		  // Special case for ** operator.
		  if (t1 == "*" && i < (old_func.length () - 1) 
		      && old_func.substr (i+1, 1) == "*")
		    {
		      new_func.append ("*");
		      i++;
		    }
		}
	      new_func.append (t1);
	      i++;
	    }

	  retval = octave_value (new octave_fcn_handle (new_func, old->inline_fcn_arg_names ()));
	}
      else
	error ("vectorize: must be an inline function");
    }
  else
    print_usage ("vectorize");

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
