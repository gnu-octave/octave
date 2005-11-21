/*

Copyright (C) 2005 Mohamed Kamoun

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

#include "lo-mappers.h"

#include "Cell.h"
#include "defun-dld.h"
#include "parse.h"
#include "variables.h"
#include "ov-colon.h"

DEFUN_DLD (cellfun, args, ,
  " -*- texinfo -*-\n\
@deftypefn {Lodable Function} {} cellfun (@var{name}, @var{c})\n\
@deftypefnx {Lodable Function} {} cellfun (\"size\", @var{c}, @var{k})\n\
@deftypefnx {Lodable Function} {} cellfun (\"isclass\", @var{c}, @var{class})\n\
@deftypefnx {Lodable Function} {} cellfun (@var{func}, @var{c})\n\
\n\
Evaluate the function named @var{name} on the elements of the cell array\n\
@var{c}.  Elements in @var{c} are passed on to the named function\n\
individually.  The function @var{name} can be one of the functions\n\
\n\
@table @code\n\
@item isempty\n\
Return 1 for empty elements.\n\
@item islogical\n\
Return 1 for logical elements.\n\
@item isreal\n\
Return 1 for real elements.\n\
@item length\n\
Return a vector of the lengths of cell elements.\n\
@item ndims\n\
Return the number of dimensions of each element.\n\
@item prodofsize\n\
Return the product of dimensions of each element.\n\
@item size\n\
Return the size along the @var{k}-th dimension.\n\
@item isclass\n\
Return 1 for elements of @var{class}.\n\
@end table\n\
\n\
Additionally, @code{cellfun} accepts an arbitrary function @var{func}\n\
in the form of an inline function, function handle, or the name of a\n\
function (in a character string).  The function should take a single\n\
argument and return a single value, and in the case of a character string\n\
argument, the argument must be named @var{x}.  For example\n\
\n\
@example\n\
@group\n\
cellfun (\"tolower(x)\", @{\"Foo\", \"Bar\", \"FooBar\"@})\n\
@result{} ans = @{\"foo\", \"bar\", \"foobar\"@}\n\
@end group\n\
@end example\n\
\n\
@end deftypefn\n\
@seealso{isempty, islogical, isreal, length, ndims, numel, size, isclass}")
{
  octave_value retval;

  std::string name = "function";

  octave_function *func = 0;

  int nargin = args.length ();

  if (nargin < 2)
    {
      error ("cellfun: you must supply at least 2 arguments");
      print_usage ("cellfun");
      return retval;
    }

  if (args(0).is_function_handle () || args(0).is_inline_function ())
    {
      func = args(0).function_value ();

      if (error_state)
	return retval;
    }
  else if (args(0).is_string ())
    name = args(0).string_value ();
  else
    {
      error ("cellfun: first argument must be a string or function handle");
      return retval;
    }	

  if (! args(1).is_cell ())
    {
      error ("cellfun: second argument must be a cell array");

      return retval;
    }
  
  Cell f_args = args(1).cell_value ();
  
  int k = f_args.numel ();

  if (name == "isempty")
    {      
      boolNDArray result (f_args.dims ());
      for (int count = 0; count < k ; count++)
        result(count) = f_args.elem(count).is_empty ();
      retval = result;
    }
  else if (name == "islogical")
    {
      boolNDArray result (f_args.dims ());
      for (int  count= 0; count < k ; count++)
        result(count) = f_args.elem(count).is_bool_type ();
      retval = result;
    }
  else if (name == "isreal")
    {
      boolNDArray result (f_args.dims ());
      for (int  count= 0; count < k ; count++)
        result(count) = f_args.elem(count).is_real_type ();
      retval = result;
    }
  else if (name == "length")
    {
      NDArray result (f_args.dims ());
      for (int  count= 0; count < k ; count++)
        result(count) = double (f_args.elem(count).length ());
      retval = result;
    }
  else if (name == "ndims")
    {
      NDArray result (f_args.dims ());
      for (int count = 0; count < k ; count++)
        result(count) = double (f_args.elem(count).ndims ());
      retval = result;
    }
  else if (name == "prodofsize")
    {
      NDArray result (f_args.dims ());
      for (int count = 0; count < k ; count++)
        result(count) = double (f_args.elem(count).numel ());
      retval = result;
    }
  else if (name == "size")
    {
      if (nargin == 3)
        {
          int d = args(2).nint_value () - 1;

          if (d < 0)
	    error ("cellfun: third argument must be a postive integer");

	  if (!error_state)
            {
              NDArray result (f_args.dims ());
              for (int count = 0; count < k ; count++)
                {
                  dim_vector dv = f_args.elem(count).dims ();
                  if (d < dv.length ())
	            result(count) = double (dv(d));
                  else
	            result(count) = 1.0;
                }
              retval = result;
            }
        }
      else
        error ("not enough arguments for `size'");
    }
  else if (name == "isclass")
    {
      if (nargin == 3)
        {
          std::string class_name = args(2).string_value();
          boolNDArray result (f_args.dims ());
          for (int count = 0; count < k ; count++)
            result(count) = (f_args.elem(count).class_name() == class_name);
          
          retval = result;
        }
      else
        error ("not enough arguments for `isclass'");
    }
  else 
    {
      std::string fcn_name;
      
      if (! func)
	{
	  fcn_name = unique_symbol_name ("__cellfun_fcn_");
	  std::string fname = "function y = ";
	  fname.append (fcn_name);
	  fname.append ("(x) y = ");
	  func = extract_function (args(0), "cellfun", fcn_name, fname,
				       "; endfunction");
	}

      if (! func)
	error ("unknown function");
      else
	{
	  Cell result (f_args.dims ());

          for (int count = 0; count < k ; count++)
	    {
	      octave_value_list tmp
		= func->do_multi_index_op (1, f_args.elem (count));
	      result(count) = tmp(0);

	      if (error_state)
		break;
	    }

	  if (! error_state)
	    retval = result;

	  if (! fcn_name.empty ())
	    clear_function (fcn_name);
	}
    }

  return retval;
}

DEFUN_DLD (num2cell, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {@var{c} =} num2cell (@var{m})\n\
@deftypefnx {Loadable Function} {@var{c} =} num2cell (@var{m}, @var{d})\n\
Convert to matrix @var{m} into a cell array. If @var{d} is defined the\n\
value @var{c} is of dimension 1 in this dimension and the elements of\n\
@var{m} are placed in slices in @var{c}.\n\
@end deftypefn\n\
@seealso{mat2cell}") 
{
  int nargin =  args.length();
  octave_value retval;

  if (nargin < 1 || nargin > 2)
    print_usage ("num2cell");
  else
    {
      dim_vector dv = args(0).dims ();
      Array<int> sings;

      if (nargin == 2)
	{
	  ColumnVector dsings = ColumnVector (args(1).vector_value 
						  (false, true));
	  sings.resize (dsings.length());

	  if (!error_state)
	    for (int i = 0; i < dsings.length(); i++)
	      if (dsings(i) > dv.length() || dsings(i) < 1 ||
		  D_NINT(dsings(i)) != dsings(i))
		{
		  error ("invalid dimension specified");
		  break;
		}
	      else
		sings(i) = NINT(dsings(i)) - 1;
	}

      if (! error_state)
	{
	  Array<bool> idx_colon (dv.length());
	  dim_vector new_dv (dv);
	  octave_value_list lst (new_dv.length(), octave_value());

	  for (int i = 0; i < dv.length(); i++)
	    {
	      idx_colon(i) = false;
	      for (int j = 0; j < sings.length(); j++)
		{
		  if (sings(j) == i)
		    {
		      new_dv(i) = 1;
		      idx_colon(i) = true;
		      lst(i) = octave_value (octave_value::magic_colon_t); 
		      break;
		    }
		}
	    }

	  Cell ret (new_dv);
	  octave_idx_type nel = new_dv.numel();
	  octave_idx_type ntot = 1;

	  for (int j = 0; j < new_dv.length()-1; j++)
	    ntot *= new_dv(j);

	  for (octave_idx_type i = 0; i <  nel; i++)
	    {
	      octave_idx_type n = ntot;
	      octave_idx_type ii = i;
	      for (int j = new_dv.length() - 1; j >= 0 ; j--)
		{
		  if (! idx_colon(j))
		    lst (j) = ii/n + 1;
		  ii = ii % n;
		  if (j != 0)
		    n /= new_dv(j-1);
		}
	      ret(i) = args(0).do_index_op(lst, 0);
	    }

	  retval = ret;
	}
    }

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
