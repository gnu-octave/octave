/*

Copyright (C) 2005, 2006, 2007 Mohamed Kamoun

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <string>
#include <vector>
#include <list>

#include "lo-mappers.h"

#include "Cell.h"
#include "oct-map.h"
#include "defun-dld.h"
#include "parse.h"
#include "variables.h"
#include "ov-colon.h"
#include "unwind-prot.h"

DEFUN_DLD (cellfun, args, nargout,
  " -*- texinfo -*-\n\
@deftypefn {Loadable Function} {} cellfun (@var{name}, @var{c})\n\
@deftypefnx {Loadable Function} {} cellfun (\"size\", @var{c}, @var{k})\n\
@deftypefnx {Loadable Function} {} cellfun (\"isclass\", @var{c}, @var{class})\n\
@deftypefnx {Loadable Function} {} cellfun (@var{func}, @var{c})\n\
@deftypefnx {Loadable Function} {} cellfun (@var{func}, @var{c}, @var{d})\n\
@deftypefnx {Loadable Function} {[@var{a}, @var{b}] =} cellfun (@dots{})\n\
@deftypefnx {Loadable Function} {} cellfun (@dots{}, 'ErrorHandler', @var{errfunc})\n\
@deftypefnx {Loadable Function} {} cellfun (@dots{}, 'UniformOutput', @var{val})\n\
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
function (in a character string). In the case of a character string\n\
argument, the function must accept a single argument named @var{x}, and\n\
it must return a string value. The function can take one or more arguments,\n\
with the inputs args given by @var{c}, @var{d}, etc. Equally the function\n\
can return one or more output arguments. For example\n\
\n\
@example\n\
@group\n\
cellfun (@@atan2, @{1, 0@}, @{0, 1@})\n\
@result{}ans = [1.57080   0.00000]\n\
@end group\n\
@end example\n\
\n\
Note that the default output argument is an array of the same size as the\n\
input arguments.\n\
\n\
If the param 'UniformOutput' is set to true (the default), then the function\n\
must return a single element which will be concatenated into the\n\
return value. If 'UniformOutput' is false, the outputs are concatenated in\n\
a cell array. For example\n\
\n\
@example\n\
@group\n\
cellfun (\"tolower(x)\", @{\"Foo\", \"Bar\", \"FooBar\"@},\n\
         \"UniformOutput\",false)\n\
@result{} ans = @{\"foo\", \"bar\", \"foobar\"@}\n\
@end group\n\
@end example\n\
\n\
Given the parameter 'ErrorHandler', then @var{errfunc} defines a function to\n\
call in case @var{func} generates an error. The form of the function is\n\
\n\
@example\n\
function [@dots{}] = errfunc (@var{s}, @dots{})\n\
@end example\n\
\n\
where there is an additional input argument to @var{errfunc} relative to\n\
@var{func}, given by @var{s}. This is a structure with the elements\n\
'identifier', 'message' and 'index', giving respectively the error\n\
identifier, the error message, and the index into the input arguments\n\
of the element that caused the error. For example\n\
\n\
@example\n\
@group\n\
function y = foo (s, x), y = NaN; endfunction\n\
cellfun (@@factorial, @{-1,2@},'ErrorHandler',@@foo)\n\
@result{} ans = [NaN 2]\n\
@end group\n\
@end example\n\
\n\
@seealso{isempty, islogical, isreal, length, ndims, numel, size}\n\
@end deftypefn")
{
  octave_value_list retval;
  std::string name = "function";
  octave_function *func = 0;
  int nargin = args.length ();
  nargout = (nargout < 1 ? 1 : nargout);

  if (nargin < 2)
    {
      error ("cellfun: you must supply at least 2 arguments");
      print_usage ();
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
  
  octave_idx_type k = f_args.numel ();

  if (name == "isempty")
    {      
      boolNDArray result (f_args.dims ());
      for (octave_idx_type count = 0; count < k ; count++)
        result(count) = f_args.elem(count).is_empty ();
      retval(0) = result;
    }
  else if (name == "islogical")
    {
      boolNDArray result (f_args.dims ());
      for (octave_idx_type  count= 0; count < k ; count++)
        result(count) = f_args.elem(count).is_bool_type ();
      retval(0) = result;
    }
  else if (name == "isreal")
    {
      boolNDArray result (f_args.dims ());
      for (octave_idx_type  count= 0; count < k ; count++)
        result(count) = f_args.elem(count).is_real_type ();
      retval(0) = result;
    }
  else if (name == "length")
    {
      NDArray result (f_args.dims ());
      for (octave_idx_type  count= 0; count < k ; count++)
        result(count) = static_cast<double> (f_args.elem(count).length ());
      retval(0) = result;
    }
  else if (name == "ndims")
    {
      NDArray result (f_args.dims ());
      for (octave_idx_type count = 0; count < k ; count++)
        result(count) = static_cast<double> (f_args.elem(count).ndims ());
      retval(0) = result;
    }
  else if (name == "prodofsize")
    {
      NDArray result (f_args.dims ());
      for (octave_idx_type count = 0; count < k ; count++)
        result(count) = static_cast<double> (f_args.elem(count).numel ());
      retval(0) = result;
    }
  else if (name == "size")
    {
      if (nargin == 3)
        {
          int d = args(2).nint_value () - 1;

          if (d < 0)
	    error ("cellfun: third argument must be a positive integer");

	  if (! error_state)
            {
              NDArray result (f_args.dims ());
              for (octave_idx_type count = 0; count < k ; count++)
                {
                  dim_vector dv = f_args.elem(count).dims ();
                  if (d < dv.length ())
	            result(count) = static_cast<double> (dv(d));
                  else
	            result(count) = 1.0;
                }
              retval(0) = result;
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
          for (octave_idx_type count = 0; count < k ; count++)
            result(count) = (f_args.elem(count).class_name() == class_name);
          
          retval(0) = result;
        }
      else
        error ("not enough arguments for `isclass'");
    }
  else 
    {
      unwind_protect::begin_frame ("Fcellfun");
      unwind_protect_int (buffer_error_messages);

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
	  octave_value_list idx;
	  octave_value_list inputlist;
	  bool uniform_output = true;
	  bool have_error_handler = false;
	  std::string err_name;
	  octave_function *error_handler = 0;
	  int offset = 1;
	  int i = 1;
	  OCTAVE_LOCAL_BUFFER (Cell, inputs, nargin);

	  while (i < nargin)
	    {
	      if (args(i).is_string())
		{
		  std::string arg = args(i++).string_value();
		  if (i == nargin)
		    {
		      error ("cellfun: parameter value is missing");
		      goto cellfun_err;
		    }

		  std::transform (arg.begin (), arg.end (), 
				  arg.begin (), tolower);

		  if (arg == "uniformoutput")
		    uniform_output = args(i++).bool_value();
		  else if (arg == "errorhandler")
		    {
		      if (args(i).is_function_handle () || 
			  args(i).is_inline_function ())
			{
			  error_handler = args(i).function_value ();

			  if (error_state)
			    goto cellfun_err;
			}
		      else if (args(i).is_string ())
			{
			  err_name = unique_symbol_name ("__cellfun_fcn_");
			  std::string fname = "function y = ";
			  fname.append (fcn_name);
			  fname.append ("(x) y = ");
			  error_handler = extract_function (args(i), "cellfun", 
							    err_name, fname,
							    "; endfunction");
			}

		      if (! error_handler)
			goto cellfun_err;

		      have_error_handler = true;
		      i++;
		    }
		  else
		    {
		      error ("cellfun: unrecognized parameter %s", 
			     arg.c_str());
		      goto cellfun_err;
		    }
		  offset += 2;
		}
	      else
		{
		  inputs[i-offset] = args(i).cell_value ();
		  if (f_args.dims() != inputs[i-offset].dims())
		    {
		      error ("cellfun: Dimension mismatch");
		      goto cellfun_err;

		    }
		  i++;
		}
	    }

	  inputlist.resize(nargin-offset);

	  if (have_error_handler)
	    buffer_error_messages++;

	  if (uniform_output)
	    {
	      retval.resize(nargout);

	      for (octave_idx_type count = 0; count < k ; count++)
		{
		  for (int j = 0; j < nargin-offset; j++)
		    inputlist(j) = inputs[j](count);

		  octave_value_list tmp = feval (func, inputlist, nargout);

		  if (error_state && have_error_handler)
		    {
		      Octave_map msg;
		      msg.assign ("identifier", last_error_id ());
		      msg.assign ("message", last_error_message ());
		      msg.assign ("index", octave_value(double (count + static_cast<octave_idx_type>(1))));
		      octave_value_list errlist = inputlist;
		      errlist.prepend (msg);
		      buffer_error_messages--;
		      error_state = 0;
		      tmp = feval (error_handler, errlist, nargout);
		      buffer_error_messages++;

		      if (error_state)
			goto cellfun_err;
		    }

		  if (tmp.length() < nargout)
		    {
		      error ("cellfun: too many output arguments");
		      goto cellfun_err;
		    }

		  if (error_state)
		    break;

		  if (count == 0)
		    {
		      for (int j = 0; j < nargout; j++)
			{
			  octave_value val;
			  val = tmp(j);

			  if (error_state)
			    goto cellfun_err;

			  retval(j) = val.resize(f_args.dims());
			}
		    }
		  else
		    {
		      idx(0) = octave_value (static_cast<double>(count+1));
		      for (int j = 0; j < nargout; j++)
			{
			  // FIXME -- need an easier way to express
			  // this test.
			  octave_value val = tmp(j);

			  if (val.ndims () == 2
			      && val.rows () == 1 && val.columns () == 1)
			    retval(j) = 
			      retval(j).subsasgn ("(", 
						  std::list<octave_value_list> 
						  (1, idx(0)), val);
			  else
			    error ("cellfun: expecting all values to be scalars for UniformOutput = true");
			}
		    }

		  if (error_state)
		    break;
		}
	    }
	  else
	    {
	      OCTAVE_LOCAL_BUFFER (Cell, results, nargout);
	      for (int j = 0; j < nargout; j++)
		results[j].resize(f_args.dims());

	      for (octave_idx_type count = 0; count < k ; count++)
		{
		  for (int j = 0; j < nargin-offset; j++)
		    inputlist(j) = inputs[j](count);

		  octave_value_list tmp = feval (func, inputlist, nargout);

		  if (error_state && have_error_handler)
		    {
		      Octave_map msg;
		      msg.assign ("identifier", last_error_id ());
		      msg.assign ("message", last_error_message ());
		      msg.assign ("index", octave_value(double (count + static_cast<octave_idx_type>(1))));
		      octave_value_list errlist = inputlist;
		      errlist.prepend (msg);
		      buffer_error_messages--;
		      error_state = 0;
		      tmp = feval (error_handler, errlist, nargout);
		      buffer_error_messages++;

		      if (error_state)
			goto cellfun_err;
		    }

		  if (tmp.length() < nargout)
		    {
		      error ("cellfun: too many output arguments");
		      goto cellfun_err;
		    }

		  if (error_state)
		    break;


		  for (int j = 0; j < nargout; j++)
		    results[j](count) = tmp(j);
		}

	      retval.resize(nargout);
	      for (int j = 0; j < nargout; j++)
		retval(j) = results[j];
	    }

	cellfun_err:
	  if (error_state)
	    retval = octave_value_list();

	  if (! fcn_name.empty ())
	    clear_function (fcn_name);

	  if (! err_name.empty ())
	    clear_function (err_name);
	}

      unwind_protect::run_frame ("Fcellfun");
    }

  return retval;
}

/*

%% Test function to check the "Errorhandler" option
%!function [z] = cellfunerror (S, varargin)
%!    z = S;
%!  endfunction

%% First input argument can be a string, an inline function,
%% a function_handle or an anonymous function
%!test
%!  A = cellfun ("islogical", {true, 0.1, false, i*2});
%!  assert (A, [true, false, true, false]);
%!test
%!  A = cellfun (inline ("islogical (x)", "x"), {true, 0.1, false, i*2});
%!  assert (A, [true, false, true, false]);
%!test
%!  A = cellfun (@islogical, {true, 0.1, false, i*2});
%!  assert (A, [true, false, true, false]);
%!test
%!  A = cellfun (@(x) islogical(x), {true, 0.1, false, i*2});
%!  assert (A, [true, false, true, false]);

%% First input argument can be the special string "isreal",
%% "isempty", "islogical", "length", "ndims" or "prodofsize"
%!test
%!  A = cellfun ("isreal", {true, 0.1, false, i*2, [], "abc"});
%!  assert (A, [true, true, true, false, true, false]);
%!test
%!  A = cellfun ("isempty", {true, 0.1, false, i*2, [], "abc"});
%!  assert (A, [false, false, false, false, true, false]);
%!test
%!  A = cellfun ("islogical", {true, 0.1, false, i*2, [], "abc"});
%!  assert (A, [true, false, true, false, false, false]);
%!test
%!  A = cellfun ("length", {true, 0.1, false, i*2, [], "abc"});
%!  assert (A, [1, 1, 1, 1, 0, 3]);
%!test
%!  A = cellfun ("ndims", {[1, 2; 3, 4]; (cell (1,2,3,4))});
%!  assert (A, [2; 4]);
%!test
%!  A = cellfun ("prodofsize", {[1, 2; 3, 4], (cell (1,2,3,4))});
%!  assert (A, [4, 24]);

%% Number of input and output arguments may not be limited to one
%!test
%!  A = cellfun (@(x,y,z) x + y + z, {1, 1, 1}, {2, 2, 2}, {3, 4, 5});
%!  assert (A, [6, 7, 8]);
%!test
%!  A = cellfun (@(x,y,z) x + y + z, {1, 1, 1}, {2, 2, 2}, {3, 4, 5}, \
%!    "UniformOutput", false);
%!  assert (A, {6, 7, 8});
%!test %% Two input arguments of different types
%!  A = cellfun (@(x,y) islogical (x) && ischar (y), {false, true}, {"a", 3});
%!  assert (A, [true, false]);
%!test %% Pass another variable to the anonymous function
%!  y = true; A = cellfun (@(x) islogical (x) && y, {false, 0.3});
%!  assert (A, [true, false]);
%!test %% Three ouptut arguments of different type
%!  [A, B, C] = cellfun (@find, {10, 11; 0, 12}, "UniformOutput", false);
%!  assert (isequal (A, {true, true; [], true}));
%!  assert (isequal (B, {true, true; [], true}));
%!  assert (isequal (C, {10, 11; [], 12}));

%% Input arguments can be of type cell array of logical
%!test
%!  A = cellfun (@(x,y) x == y, {false, true}, {true, true});
%!  assert (A, [false, true]);
%!test
%!  A = cellfun (@(x,y) x == y, {false; true}, {true; true}, \
%!    "UniformOutput", true);
%!  assert (A, [false; true]);
%!test
%!  A = cellfun (@(x) x, {false, true; false, true}, "UniformOutput", false);
%!  assert (A, {false, true; false, true});
%!test %% Three ouptut arguments of same type
%!  [A, B, C] = cellfun (@find, {true, false; false, true}, \
%!    "UniformOutput", false);
%!  assert (isequal (A, {true, []; [], true}));
%!  assert (isequal (B, {true, []; [], true}));
%!  assert (isequal (C, {true, []; [], true}));
%!test
%!  A = cellfun (@(x,y) cell2str (x,y), {true}, {true}, \
%!    "ErrorHandler", @cellfunerror);
%!  assert (isfield (A, "identifier"), true);
%!  assert (isfield (A, "message"), true);
%!  assert (isfield (A, "index"), true);
%!  assert (isempty (A.message), false);
%!  assert (A.index, 1);
%!test %% Overwriting setting of "UniformOutput" true
%!  A = cellfun (@(x,y) cell2str (x,y), {true}, {true}, \
%!    "UniformOutput", true, "ErrorHandler", @cellfunerror);
%!  assert (isfield (A, "identifier"), true);
%!  assert (isfield (A, "message"), true);
%!  assert (isfield (A, "index"), true);
%!  assert (isempty (A.message), false);
%!  assert (A.index, 1);

%% Input arguments can be of type cell array of numeric
%!test
%!  A = cellfun (@(x,y) x>y, {1.1, 4.2}, {3.1, 2+6*i});
%!  assert (A, [false, true]);
%!test
%!  A = cellfun (@(x,y) x>y, {1.1, 4.2; 2, 4}, {3.1, 2; 2, 4+2*i}, \
%!    "UniformOutput", true);
%!  assert (A, [false, true; false, false]);
%!test
%!  A = cellfun (@(x,y) x:y, {1.1, 4}, {3.1, 6}, "UniformOutput", false);
%!  assert (isequal (A{1}, [1.1, 2.1, 3.1]));
%!  assert (isequal (A{2}, [4, 5, 6]));
%!test %% Three ouptut arguments of different type
%!  [A, B, C] = cellfun (@find, {10, 11; 0, 12}, "UniformOutput", false);
%!  assert (isequal (A, {true, true; [], true}));
%!  assert (isequal (B, {true, true; [], true}));
%!  assert (isequal (C, {10, 11; [], 12}));
%!test
%!  A = cellfun (@(x,y) cell2str(x,y), {1.1, 4}, {3.1, 6}, \
%!    "ErrorHandler", @cellfunerror);
%!  B = isfield (A(1), "message") && isfield (A(1), "index");
%!  assert ([(isfield (A(1), "identifier")), (isfield (A(2), "identifier"))], [true, true]);
%!  assert ([(isfield (A(1), "message")), (isfield (A(2), "message"))], [true, true]);
%!  assert ([(isfield (A(1), "index")), (isfield (A(2), "index"))], [true, true]);
%!  assert ([(isempty (A(1).message)), (isempty (A(2).message))], [false, false]);
%!  assert ([A(1).index, A(2).index], [1, 2]);
%!test %% Overwriting setting of "UniformOutput" true
%!  A = cellfun (@(x,y) cell2str(x,y), {1.1, 4}, {3.1, 6}, \
%!    "UniformOutput", true, "ErrorHandler", @cellfunerror);
%!  B = isfield (A(1), "message") && isfield (A(1), "index");
%!  assert ([(isfield (A(1), "identifier")), (isfield (A(2), "identifier"))], [true, true]);
%!  assert ([(isfield (A(1), "message")), (isfield (A(2), "message"))], [true, true]);
%!  assert ([(isfield (A(1), "index")), (isfield (A(2), "index"))], [true, true]);
%!  assert ([(isempty (A(1).message)), (isempty (A(2).message))], [false, false]);
%!  assert ([A(1).index, A(2).index], [1, 2]);

%% Input arguments can be of type cell arrays of character or strings
%!error %% "UniformOutput" false should be used
%!  A = cellfun (@(x,y) x>y, {"ad", "c", "ghi"}, {"cc", "d", "fgh"});
%!test
%!  A = cellfun (@(x,y) x>y, {"a"; "f"}, {"c"; "d"}, "UniformOutput", true);
%!  assert (A, [false; true]);
%!test
%!  A = cellfun (@(x,y) x:y, {"a", "d"}, {"c", "f"}, "UniformOutput", false);
%!  assert (A, {"abc", "def"});
%!test
%!  A = cellfun (@(x,y) cell2str(x,y), {"a", "d"}, {"c", "f"}, \
%!    "ErrorHandler", @cellfunerror);
%!  assert ([(isfield (A(1), "identifier")), (isfield (A(2), "identifier"))], [true, true]);
%!  assert ([(isfield (A(1), "message")), (isfield (A(2), "message"))], [true, true]);
%!  assert ([(isfield (A(1), "index")), (isfield (A(2), "index"))], [true, true]);
%!  assert ([(isempty (A(1).message)), (isempty (A(2).message))], [false, false]);
%!  assert ([A(1).index, A(2).index], [1, 2]);
%!test %% Overwriting setting of "UniformOutput" true
%!  A = cellfun (@(x,y) cell2str(x,y), {"a", "d"}, {"c", "f"}, \
%!    "UniformOutput", true, "ErrorHandler", @cellfunerror);
%!  assert ([(isfield (A(1), "identifier")), (isfield (A(2), "identifier"))], [true, true]);
%!  assert ([(isfield (A(1), "message")), (isfield (A(2), "message"))], [true, true]);
%!  assert ([(isfield (A(1), "index")), (isfield (A(2), "index"))], [true, true]);
%!  assert ([(isempty (A(1).message)), (isempty (A(2).message))], [false, false]);
%!  assert ([A(1).index, A(2).index], [1, 2]);

%% Structures cannot be handled by cellfun
%!error
%!  vst1.a = 1.1; vst1.b = 4.2; vst2.a = 3.1; vst2.b = 2;
%!  A = cellfun (@(x,y) (x.a < y.a) && (x.b > y.b), vst1, vst2);

%% Input arguments can be of type cell array of cell arrays
%!test
%!  A = cellfun (@(x,y) x{1} < y{1}, {{1.1}, {4.2}}, {{3.1}, {2}});
%!  assert (A, [1, 0], 1e-16);
%!test
%!  A = cellfun (@(x,y) x{1} < y{1}, {{1.1}; {4.2}}, {{3.1}; {2}}, \
%!    "UniformOutput", true);
%!  assert (A, [1; 0], 1e-16);
%!test
%!  A = cellfun (@(x,y) x{1} < y{1}, {{1.1}, {4.2}}, {{3.1}, {2}}, \
%!    "UniformOutput", false);
%!  assert (A, {true, false});
%!test
%!  A = cellfun (@(x,y) mat2str(x,y), {{1.1}, {4.2}}, {{3.1}, {2}}, \
%!    "ErrorHandler", @cellfunerror);
%!  assert ([(isfield (A(1), "identifier")), (isfield (A(2), "identifier"))], [true, true]);
%!  assert ([(isfield (A(1), "message")), (isfield (A(2), "message"))], [true, true]);
%!  assert ([(isfield (A(1), "index")), (isfield (A(2), "index"))], [true, true]);
%!  assert ([(isempty (A(1).message)), (isempty (A(2).message))], [false, false]);
%!  assert ([A(1).index, A(2).index], [1, 2]);
%!test %% Overwriting setting of "UniformOutput" true
%!  A = cellfun (@(x,y) mat2str(x,y), {{1.1}, {4.2}}, {{3.1}, {2}}, \
%!    "UniformOutput", true, "ErrorHandler", @cellfunerror);
%!  assert ([(isfield (A(1), "identifier")), (isfield (A(2), "identifier"))], [true, true]);
%!  assert ([(isfield (A(1), "message")), (isfield (A(2), "message"))], [true, true]);
%!  assert ([(isfield (A(1), "index")), (isfield (A(2), "index"))], [true, true]);
%!  assert ([(isempty (A(1).message)), (isempty (A(2).message))], [false, false]);
%!  assert ([A(1).index, A(2).index], [1, 2]);

%% Input arguments can be of type cell array of structure arrays
%!test
%!  a = struct ("a", 1, "b", 2); b = struct ("a", 1, "b", 3);
%!  A = cellfun (@(x,y) (x.a == y.a) && (x.b < y.b), {a}, {b});
%!  assert (A, true);
%!test
%!  a = struct ("a", 1, "b", 2); b = struct ("a", 1, "b", 3);
%!  A = cellfun (@(x,y) (x.a == y.a) && (x.b < y.b) , {a}, {b}, \
%!    "UniformOutput", true);
%!  assert (A, true);
%!test
%!  a = struct ("a", 1, "b", 2); b = struct ("a", 1, "b", 3);
%!  A = cellfun (@(x,y) (x.a == y.a) && (x.b < y.b) , {a}, {b}, \
%!    "UniformOutput", false);
%!  assert (A, {true});
%!test
%!  a = struct ("a", 1, "b", 2); b = struct ("a", 1, "b", 3);
%!  A = cellfun (@(x,y) cell2str (x.a, y.a), {a}, {b}, \
%!    "ErrorHandler", @cellfunerror);
%!  assert (isfield (A, "identifier"), true);
%!  assert (isfield (A, "message"), true);
%!  assert (isfield (A, "index"), true);
%!  assert (isempty (A.message), false);
%!  assert (A.index, 1);
%!test %% Overwriting setting of "UniformOutput" true
%!  a = struct ("a", 1, "b", 2); b = struct ("a", 1, "b", 3);
%!  A = cellfun (@(x,y) cell2str (x.a, y.a), {a}, {b}, \
%!    "UniformOutput", true, "ErrorHandler", @cellfunerror);
%!  assert (isfield (A, "identifier"), true);
%!  assert (isfield (A, "message"), true);
%!  assert (isfield (A, "index"), true);
%!  assert (isempty (A.message), false);
%!  assert (A.index, 1);

%% A lot of other tests
%!error(cellfun(1))
%!error(cellfun('isclass',1))
%!error(cellfun('size',1))
%!error(cellfun(@sin,{[]},'BadParam',false))
%!error(cellfun(@sin,{[]},'UniformOuput'))
%!error(cellfun(@sin,{[]},'ErrorHandler'))
%!assert(cellfun(@sin,{0,1}),sin([0,1]))
%!assert(cellfun(inline('sin(x)'),{0,1}),sin([0,1]))
%!assert(cellfun('sin',{0,1}),sin([0,1]))
%!assert(cellfun('isempty',{1,[]}),[false,true])
%!assert(cellfun('islogical',{false,pi}),[true,false])
%!assert(cellfun('isreal',{1i,1}),[false,true])
%!assert(cellfun('length',{zeros(2,2),1}),[2,1])
%!assert(cellfun('prodofsize',{zeros(2,2),1}),[4,1])
%!assert(cellfun('ndims',{zeros([2,2,2]),1}),[3,2])
%!assert(cellfun('isclass',{zeros([2,2,2]),'test'},'double'),[true,false])
%!assert(cellfun('size',{zeros([1,2,3]),1},1),[1,1])
%!assert(cellfun('size',{zeros([1,2,3]),1},2),[2,1])
%!assert(cellfun('size',{zeros([1,2,3]),1},3),[3,1])
%!assert(cellfun(@atan2,{1,1},{1,2}),[atan2(1,1),atan2(1,2)])
%!assert(cellfun(@atan2,{1,1},{1,2},'UniformOutput',false),{atan2(1,1),atan2(1,2)})
%!assert(cellfun(@sin,{1,2;3,4}),sin([1,2;3,4]))
%!assert(cellfun(@atan2,{1,1;1,1},{1,2;1,2}),atan2([1,1;1,1],[1,2;1,2]))
%!error(cellfun(@factorial,{-1,3}))
%!assert(cellfun(@factorial,{-1,3},'ErrorHandler',@(x,y) NaN),[NaN,6])
%!test
%! [a,b,c]=cellfun(@fileparts,{fullfile("a","b","c.d"),fullfile("e","f","g.h")},'UniformOutput',false);
%! assert(a,{fullfile("a","b"),fullfile("e","f")})
%! assert(b,{'c','g'})
%! assert(c,{'.d','.h'})

*/

DEFUN_DLD (num2cell, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {@var{c} =} num2cell (@var{m})\n\
@deftypefnx {Loadable Function} {@var{c} =} num2cell (@var{m}, @var{dim})\n\
Convert the matrix @var{m} into a cell array. If @var{dim} is defined, the\n\
value @var{c} is of dimension 1 in this dimension and the elements of\n\
@var{m} are placed in slices in @var{c}.\n\
@seealso{mat2cell}\n\
@end deftypefn") 
{
  int nargin =  args.length();
  octave_value retval;

  if (nargin < 1 || nargin > 2)
    print_usage ();
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
	    for (octave_idx_type i = 0; i < dsings.length(); i++)
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

%!assert(num2cell([1,2;3,4]),{1,2;3,4})
%!assert(num2cell([1,2;3,4],1),{[1;3],[2;4]})
%!assert(num2cell([1,2;3,4],2),{[1,2];[3,4]})

*/

DEFUN_DLD (mat2cell, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {@var{b} =} mat2cell (@var{a}, @var{m}, @var{n})\n\
@deftypefnx {Loadable Function} {@var{b} =} mat2cell (@var{a}, @var{d1}, @var{d2}, @dots{})\n\
@deftypefnx {Loadable Function} {@var{b} =} mat2cell (@var{a}, @var{r})\n\
Convert the matrix @var{a} to a cell array. If @var{a} is 2-D, then\n\
it is required that @code{sum (@var{m}) == size (@var{a}, 1)} and\n\
@code{sum (@var{n}) == size (@var{a}, 2)}. Similarly, if @var{a} is\n\
a multi-dimensional and the number of dimensional arguments is equal\n\
to the dimensions of @var{a}, then it is required that @code{sum (@var{di})\n\
== size (@var{a}, i)}.\n\
\n\
Given a single dimensional argument @var{r}, the other dimensional\n\
arguments are assumed to equal @code{size (@var{a},@var{i})}.\n\
\n\
An example of the use of mat2cell is\n\
\n\
@example\n\
@group\n\
mat2cell (reshape(1:16,4,4),[3,1],[3,1])\n\
@result{} @{\n\
  [1,1] =\n\
\n\
     1   5   9\n\
     2   6  10\n\
     3   7  11\n\
\n\
  [2,1] =\n\
\n\
     4   8  12\n\
\n\
  [1,2] =\n\
\n\
    13\n\
    14\n\
    15\n\
\n\
  [2,2] = 16\n\
@}\n\
@end group\n\
@end example\n\
@seealso{num2cell, cell2mat}\n\
@end deftypefn")
{
  int nargin = args.length();
  octave_value retval;

  if (nargin < 2)
    print_usage ();
  else
    {
      dim_vector dv = args(0).dims();
      dim_vector new_dv;
      new_dv.resize(dv.length());
      
      if (nargin > 2)
	{
	  octave_idx_type nmax = -1;

	  if (nargin - 1 != dv.length())
	    error ("mat2cell: Incorrect number of dimensions");
	  else
	    {
	      for (octave_idx_type j = 0; j < dv.length(); j++)
		{
		  ColumnVector d = ColumnVector (args(j+1).vector_value 
						 (false, true));

		  if (d.length() < 1)
		    {
		      error ("mat2cell: dimension can not be empty");
		      break;
		    }
		  else
		    {
		      if (nmax < d.length())
			nmax = d.length();

		      for (octave_idx_type i = 1; i < d.length(); i++)
			{
			  OCTAVE_QUIT;

			  if (d(i) >= 0)
			    d(i) += d(i-1);
			  else
			    {
			      error ("mat2cell: invalid dimensional argument");
			      break;
			    }
			}

		      if (d(0) < 0)
			error ("mat2cell: invalid dimensional argument");
		      
		      if (d(d.length() - 1) != dv(j))
			error ("mat2cell: inconsistent dimensions");

		      if (error_state)
			break;

		      new_dv(j) = d.length();
		    }
		}
	    }

	  if (! error_state)
	    {
	      // Construct a matrix with the index values
	      Matrix dimargs(nmax, new_dv.length());
	      for (octave_idx_type j = 0; j < new_dv.length(); j++)
		{
		  OCTAVE_QUIT;

		  ColumnVector d = ColumnVector (args(j+1).vector_value 
						 (false, true));

		  dimargs(0,j) = d(0);
		  for (octave_idx_type i = 1; i < d.length(); i++)
		    dimargs(i,j) = dimargs(i-1,j) + d(i);
		}


	      octave_value_list lst (new_dv.length(), octave_value());
	      Cell ret (new_dv);
	      octave_idx_type nel = new_dv.numel();
	      octave_idx_type ntot = 1;

	      for (int j = 0; j < new_dv.length()-1; j++)
		ntot *= new_dv(j);

	      for (octave_idx_type i = 0; i <  nel; i++)
		{
		  octave_idx_type n = ntot;
		  octave_idx_type ii = i;
		  for (octave_idx_type j =  new_dv.length() - 1;  j >= 0; j--)
		    {
		      OCTAVE_QUIT;
		  
		      octave_idx_type idx = ii / n;
		      lst (j) = Range((idx == 0 ? 1. : dimargs(idx-1,j)+1.),
				      dimargs(idx,j));
		      ii = ii % n;
		      if (j != 0)
			n /= new_dv(j-1);
		    }
		  ret(i) = args(0).do_index_op(lst, 0);
		  if (error_state)
		    break;
		}
	  
	      if (!error_state)
		retval = ret;
	    }
	}
      else
	{
	  ColumnVector d = ColumnVector (args(1).vector_value 
					 (false, true));

	  double sumd = 0.;
	  for (octave_idx_type i = 0; i < d.length(); i++)
	    {
	      OCTAVE_QUIT;

	      if (d(i) >= 0)
		sumd += d(i);
	      else
		{
		  error ("mat2cell: invalid dimensional argument");
		  break;
		}
	    }

	  if (sumd != dv(0))
	    error ("mat2cell: inconsistent dimensions");

	  new_dv(0) = d.length();
	  for (octave_idx_type i = 1; i < dv.length(); i++)
	    new_dv(i) = 1;

	  if (! error_state)
	    {
	      octave_value_list lst (new_dv.length(), octave_value());
	      Cell ret (new_dv);

	      for (octave_idx_type i = 1; i < new_dv.length(); i++)
		lst (i) = Range (1., static_cast<double>(dv(i)));
	      
	      double idx = 0.;
	      for (octave_idx_type i = 0; i <  new_dv(0); i++)
		{
		  OCTAVE_QUIT;

		  lst(0) = Range(idx + 1., idx + d(i));
		  ret(i) = args(0).do_index_op(lst, 0);
		  idx += d(i);
		  if (error_state)
		    break;
		}
	  
	      if (!error_state)
		retval = ret;
	    }
	}
    }

  return retval;
}

/*

%!test
%! x = reshape(1:20,5,4);
%! c = mat2cell(x,[3,2],[3,1]);
%! assert(c,{[1,6,11;2,7,12;3,8,13],[16;17;18];[4,9,14;5,10,15],[19;20]})

%!test
%! x = 'abcdefghij';
%! c = mat2cell(x,1,[0,4,2,0,4,0]);
%! empty1by0str = resize('',1,0);
%! assert(c,{empty1by0str,'abcd','ef',empty1by0str,'ghij',empty1by0str})

*/
	  
/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
