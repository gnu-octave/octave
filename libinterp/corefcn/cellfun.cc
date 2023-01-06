////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2005-2023 The Octave Project Developers
//
// See the file COPYRIGHT.md in the top-level directory of this
// distribution or <https://octave.org/copyright/>.
//
// This file is part of Octave.
//
// Octave is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// Octave is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Octave; see the file COPYING.  If not, see
// <https://www.gnu.org/licenses/>.
//
////////////////////////////////////////////////////////////////////////

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <string>
#include <vector>
#include <list>
#include <memory>

#include "lo-mappers.h"
#include "oct-locbuf.h"
#include "oct-string.h"

#include "Cell.h"
#include "oct-map.h"
#include "defun.h"
#include "interpreter-private.h"
#include "interpreter.h"
#include "parse.h"
#include "variables.h"
#include "unwind-prot.h"
#include "errwarn.h"
#include "utils.h"

#include "ov-bool.h"
#include "ov-class.h"
#include "ov-colon.h"
#include "ov-complex.h"
#include "ov-float.h"
#include "ov-flt-complex.h"
#include "ov-int16.h"
#include "ov-int32.h"
#include "ov-int64.h"
#include "ov-int8.h"
#include "ov-scalar.h"
#include "ov-uint16.h"
#include "ov-uint32.h"
#include "ov-uint64.h"
#include "ov-uint8.h"

#include "ov-fcn-handle.h"

OCTAVE_BEGIN_NAMESPACE(octave)

static octave_value_list
get_output_list (error_system& es,
                 octave_idx_type count, octave_idx_type nargout,
                 const octave_value_list& inputlist,
                 octave_value& fcn,
                 octave_value& error_handler)
{
  octave_value_list tmp;

  bool execution_error = false;

  try
    {
      tmp = feval (fcn, inputlist, nargout);
    }
  catch (const execution_exception& ee)
    {
      if (error_handler.is_defined ())
        {
          interpreter& interp = __get_interpreter__ ();

          es.save_exception (ee);
          interp.recover_from_exception ();

          execution_error = true;
        }
      else
        throw;
    }

  if (execution_error)
    {
      if (error_handler.is_defined ())
        {
          octave_scalar_map msg;
          msg.assign ("identifier", es.last_error_id ());
          msg.assign ("message", es.last_error_message ());
          msg.assign ("index",
                      static_cast<double> (count
                                           + static_cast<octave_idx_type> (1)));

          octave_value_list errlist = inputlist;
          errlist.prepend (msg);

          tmp = feval (error_handler, errlist, nargout);
        }
      else
        tmp.clear ();
    }

  return tmp;
}

// Templated function because the user can be stubborn enough to request
// a cell array as an output even in these cases where the output fits
// in an ordinary array
template <typename BNDA, typename NDA>
static octave_value_list
try_cellfun_internal_ops (const octave_value_list& args, int nargin)
{
  octave_value_list retval;

  std::string name = args(0).string_value ();

  const Cell f_args = args(1).cell_value ();

  octave_idx_type k = f_args.numel ();

  if (name == "isempty")
    {
      BNDA result (f_args.dims ());
      for (octave_idx_type count = 0; count < k; count++)
        result(count) = f_args.elem (count).isempty ();
      retval(0) = result;
    }
  else if (name == "islogical")
    {
      BNDA result (f_args.dims ());
      for (octave_idx_type count= 0; count < k; count++)
        result(count) = f_args.elem (count).islogical ();
      retval(0) = result;
    }
  else if (name == "isnumeric")
    {
      BNDA result (f_args.dims ());
      for (octave_idx_type count= 0; count < k; count++)
        result(count) = f_args.elem (count).isnumeric ();
      retval(0) = result;
    }
  else if (name == "isreal")
    {
      BNDA result (f_args.dims ());
      for (octave_idx_type count= 0; count < k; count++)
        result(count) = f_args.elem (count).isreal ();
      retval(0) = result;
    }
  else if (name == "length")
    {
      NDA result (f_args.dims ());
      for (octave_idx_type count= 0; count < k; count++)
        result(count) = static_cast<double> (f_args.elem (count).length ());
      retval(0) = result;
    }
  else if (name == "ndims")
    {
      NDA result (f_args.dims ());
      for (octave_idx_type count = 0; count < k; count++)
        result(count) = static_cast<double> (f_args.elem (count).ndims ());
      retval(0) = result;
    }
  else if (name == "numel" || name == "prodofsize")
    {
      NDA result (f_args.dims ());
      for (octave_idx_type count = 0; count < k; count++)
        result(count) = static_cast<double> (f_args.elem (count).numel ());
      retval(0) = result;
    }
  else if (name == "size")
    {
      if (nargin != 3)
        error (R"(cellfun: not enough arguments for "size")");

      int d = args(2).nint_value () - 1;

      if (d < 0)
        error ("cellfun: K must be a positive integer");

      NDA result (f_args.dims ());

      for (octave_idx_type count = 0; count < k; count++)
        {
          dim_vector dv = f_args.elem (count).dims ();
          if (d < dv.ndims ())
            result(count) = static_cast<double> (dv(d));
          else
            result(count) = 1.0;
        }

      retval(0) = result;
    }
  else if (name == "isclass")
    {
      if (nargin != 3)
        error (R"(cellfun: not enough arguments for "isclass")");

      std::string class_name = args(2).string_value ();
      BNDA result (f_args.dims ());
      for (octave_idx_type count = 0; count < k; count++)
        result(count) = (f_args.elem (count).class_name () == class_name);

      retval(0) = result;
    }

  return retval;
}

static void
get_mapper_fun_options (symbol_table& symtab,
                        const octave_value_list& args,
                        int& nargin, bool& uniform_output,
                        octave_value& error_handler)
{
  while (nargin > 3 && args(nargin-2).is_string ())
    {
      std::string arg = args(nargin-2).string_value ();

      std::size_t compare_len
        = std::max (arg.length (), static_cast<std::size_t> (2));

      if (string::strncmpi (arg, "uniformoutput", compare_len))
        uniform_output = args(nargin-1).bool_value ();
      else if (string::strncmpi (arg, "errorhandler", compare_len))
        {
          if (args(nargin-1).is_function_handle ()
              || args(nargin-1).is_inline_function ())
            {
              error_handler = args(nargin-1);
            }
          else if (args(nargin-1).is_string ())
            {
              std::string err_name = args(nargin-1).string_value ();

              error_handler = symtab.find_function (err_name);

              if (error_handler.is_undefined ())
                error ("cellfun: invalid function NAME: %s",
                       err_name.c_str ());
            }
          else
            error ("cellfun: invalid value for 'ErrorHandler' function");
        }
      else
        error ("cellfun: unrecognized parameter %s", arg.c_str ());

      nargin -= 2;
    }

  nargin -= 1;
}

DEFMETHOD (cellfun, interp, args, nargout,
           doc: /* -*- texinfo -*-
@deftypefn  {} {@var{A} =} cellfun ("@var{fcn}", @var{C})
@deftypefnx {} {@var{A} =} cellfun ("size", @var{C}, @var{k})
@deftypefnx {} {@var{A} =} cellfun ("isclass", @var{C}, @var{class})
@deftypefnx {} {@var{A} =} cellfun (@@@var{fcn}, @var{C})
@deftypefnx {} {@var{A} =} cellfun (@var{fcn}, @var{C})
@deftypefnx {} {@var{A} =} cellfun (@var{fcn}, @var{C1}, @var{C2}, @dots{})
@deftypefnx {} {[@var{A1}, @var{A2}, @dots{}] =} cellfun (@dots{})
@deftypefnx {} {@var{A} =} cellfun (@dots{}, "ErrorHandler", @var{errfcn})
@deftypefnx {} {@var{A} =} cellfun (@dots{}, "UniformOutput", @var{val})

Evaluate the function named "@var{fcn}" on the elements of the cell array
@var{C}.

Elements in @var{C} are passed on to the named function individually.  The
function @var{fcn} can be one of the functions

@table @code
@item isempty
Return 1 for empty elements.

@item islogical
Return 1 for logical elements.

@item isnumeric
Return 1 for numeric elements.

@item isreal
Return 1 for real elements.

@item length
Return a vector of the lengths of cell elements.

@item ndims
Return the number of dimensions of each element.

@item  numel
@itemx prodofsize
Return the number of elements contained within each cell element.  The
number is the product of the dimensions of the object at each cell element.

@item size
Return the size along the @var{k}-th dimension.

@item isclass
Return 1 for elements of @var{class}.
@end table

Additionally, @code{cellfun} accepts an arbitrary function @var{fcn} in the
form of an inline function, function handle, or the name of a function (in a
character string).  The function can take one or more arguments, with the
inputs arguments given by @var{C1}, @var{C2}, etc.  For example:

@example
@group
cellfun ("atan2", @{1, 0@}, @{0, 1@})
     @result{} [ 1.57080   0.00000 ]
@end group
@end example

The number of output arguments of @code{cellfun} matches the number of output
arguments of the function and can be greater than one.  When there are multiple
outputs of the function they will be collected into the output arguments of
@code{cellfun} like this:

@example
@group
function [a, b] = twoouts (x)
  a = x;
  b = x*x;
endfunction
[aa, bb] = cellfun (@@twoouts, @{1, 2, 3@})
     @result{}
        aa =
           1 2 3
        bb =
           1 4 9
@end group
@end example

Note that, per default, the output argument(s) are arrays of the same size as
the input arguments.  Input arguments that are singleton (1x1) cells will be
automatically expanded to the size of the other arguments.

If the parameter @qcode{"UniformOutput"} is set to true (the default), then the
function must return scalars which will be concatenated into the return
array(s).  If @qcode{"UniformOutput"} is false, the outputs are concatenated
into a cell array (or cell arrays).  For example:

@example
@group
cellfun ("tolower", @{"Foo", "Bar", "FooBar"@},
         "UniformOutput", false)
@result{} @{"foo", "bar", "foobar"@}
@end group
@end example

Given the parameter @qcode{"ErrorHandler"}, then @var{errfcn} defines a
function to call in case @var{fcn} generates an error.  The form of the
function is

@example
function [@dots{}] = errfcn (@var{s}, @dots{})
@end example

@noindent
where there is an additional input argument to @var{errfcn} relative to
@var{fcn}, given by @var{s}.  This is a structure with the elements
@qcode{"identifier"}, @qcode{"message"}, and @qcode{"index"} giving
respectively the error identifier, the error message, and the index into the
input arguments of the element that caused the error.  For example:

@example
@group
function y = foo (s, x), y = NaN; endfunction
cellfun ("factorial", @{-1,2@}, "ErrorHandler", @@foo)
@result{} [NaN 2]
@end group
@end example

Use @code{cellfun} intelligently.  The @code{cellfun} function is a useful tool
for avoiding loops.  It is often used with anonymous function handles; however,
calling an anonymous function involves an overhead quite comparable to the
overhead of an m-file function.  Passing a handle to a built-in function is
faster, because the interpreter is not involved in the internal loop.  For
example:

@example
@group
C = @{@dots{}@}
v = cellfun (@@(x) det (x), C); # compute determinants
v = cellfun (@@det, C);         # 40% faster
@end group
@end example

@seealso{arrayfun, structfun, spfun}
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin < 2)
    print_usage ();

  if (! args(1).iscell ())
    error ("cellfun: C must be a cell array");

  octave_value_list retval;
  int nargout1 = (nargout < 1 ? 1 : nargout);

  octave_value fcn = args(0);

  symbol_table& symtab = interp.get_symbol_table ();

  if (fcn.is_string ())
    {
      retval = try_cellfun_internal_ops<boolNDArray, NDArray> (args, nargin);

      if (! retval.empty ())
        return retval;

      // See if we can convert the string into a function.

      std::string name = args(0).string_value ();

      if (! valid_identifier (name))
        fcn = get_function_handle (interp, args(0), "x");
      else
        {
          fcn = symtab.find_function (name);

          if (fcn.is_undefined ())
            error ("cellfun: invalid function NAME: %s", name.c_str ());
        }
    }

  if (! fcn.is_function_handle () && ! fcn.is_inline_function ()
      && ! fcn.is_function ())
    error ("cellfun: argument NAME must be a string or function handle");

  bool uniform_output = true;
  octave_value error_handler;

  get_mapper_fun_options (symtab, args, nargin, uniform_output, error_handler);

  // The following is an optimization because the symbol table can give a
  // more specific function class, so this can result in fewer polymorphic
  // function calls as the function gets called for each value of the array.
  {
    if (fcn.is_function_handle () || fcn.is_inline_function ())
      {
        // FIXME: instead of checking for overloaded functions as we did
        // previously but is no longer possible, we could check whether
        // the types of all the cell array elements are the same, then
        // lookup the function for that type just once.

        goto nevermind;
      }

    std::string name = fcn.function_value () -> name ();
    octave_value f = symtab.find_function (name);

    if (f.is_defined ())
      {
        // Except for these two which are special cases...
        if (name != "size" && name != "class")
          {
            // Try first the optimized code path for built-in functions
            octave_value_list tmp_args = args;
            tmp_args(0) = name;

            if (uniform_output)
              retval = try_cellfun_internal_ops<boolNDArray, NDArray> (tmp_args, nargin);
            else
              retval = try_cellfun_internal_ops<Cell, Cell> (tmp_args, nargin);

            if (! retval.empty ())
              return retval;
          }

        // Okay, we tried, doesn't work, let's do the best we can instead
        // and avoid polymorphic calls for each element of the array.
        fcn = f;
      }
  }

nevermind:

  // Extract cell arguments.

  octave_value_list inputlist (nargin, octave_value ());

  OCTAVE_LOCAL_BUFFER (Cell, inputs, nargin);
  OCTAVE_LOCAL_BUFFER (bool, mask, nargin);

  // This is to prevent copy-on-write.
  const Cell *cinputs = inputs;

  octave_idx_type k = 1;

  dim_vector fdims (1, 1);

  // Collect arguments.  Pre-fill scalar elements of inputlist array.

  for (int j = 0; j < nargin; j++)
    {
      if (! args(j+1).iscell ())
        error ("cellfun: arguments must be cells");

      inputs[j] = args(j+1).cell_value ();
      mask[j] = inputs[j].numel () != 1;
      if (! mask[j])
        inputlist(j) = cinputs[j](0);
    }

  for (int j = 0; j < nargin; j++)
    {
      if (mask[j])
        {
          fdims = inputs[j].dims ();
          k = inputs[j].numel ();
          for (int i = j+1; i < nargin; i++)
            {
              if (mask[i] && inputs[i].dims () != fdims)
                error ("cellfun: dimensions mismatch");
            }
          break;
        }
    }

  error_system& es = interp.get_error_system ();

  // Apply functions.

  if (uniform_output)
    {
      std::list<octave_value_list> idx_list (1);
      idx_list.front ().resize (1);
      std::string idx_type = "(";

      OCTAVE_LOCAL_BUFFER (octave_value, retv, nargout1);

      int expected_nargout;
      for (octave_idx_type count = 0; count < k; count++)
        {
          for (int j = 0; j < nargin; j++)
            {
              if (mask[j])
                inputlist.xelem (j) = cinputs[j](count);
            }

          const octave_value_list tmp
            = get_output_list (es, count, nargout, inputlist, fcn,
                               error_handler);

          int tmp_numel = tmp.length ();
          if (count == 0)
            expected_nargout = tmp_numel;
          else if (tmp_numel != expected_nargout)
            error ("cellfun: function returned unexpected number of values");

          if (nargout > 0 && tmp_numel < nargout)
            error ("cellfun: function returned fewer than nargout values");

          if (nargout > 0
              || (nargout == 0 && tmp_numel > 0 && tmp(0).is_defined ()))
            {
              int num_to_copy = tmp.length ();

              if (num_to_copy > nargout1)
                num_to_copy = nargout1;

              if (count == 0)
                {
                  for (int j = 0; j < num_to_copy; j++)
                    {
                      if (tmp(j).is_defined ())
                        {
                          octave_value val = tmp(j);

                          if (val.numel () != 1)
                            error ("cellfun: all values must be scalars when UniformOutput = true");

                          retv[j] = val.resize (fdims);
                        }
                    }
                }
              else
                {
                  for (int j = 0; j < num_to_copy; j++)
                    {
                      if (tmp(j).is_defined ())
                        {
                          octave_value val = tmp(j);

                          if (! retv[j].fast_elem_insert (count, val))
                            {
                              if (val.numel () != 1)
                                error ("cellfun: all values must be scalars when UniformOutput = true");

                              idx_list.front ()(0) = count + 1.0;
                              retv[j].assign (octave_value::op_asn_eq,
                                              idx_type, idx_list, val);
                            }
                        }
                    }
                }
            }
        }

      retval.resize (nargout1);

      for (int j = 0; j < nargout1; j++)
        {
          if (nargout > 0 && retv[j].is_undefined ())
            retval(j) = NDArray (fdims);
          else
            retval(j) = retv[j];
        }
    }
  else
    {
      OCTAVE_LOCAL_BUFFER (Cell, results, nargout1);

      for (int j = 0; j < nargout1; j++)
        results[j].resize (fdims, Matrix ());

      bool have_some_output = false;

      for (octave_idx_type count = 0; count < k; count++)
        {
          for (int j = 0; j < nargin; j++)
            {
              if (mask[j])
                inputlist.xelem (j) = cinputs[j](count);
            }

          const octave_value_list tmp
            = get_output_list (es, count, nargout, inputlist, fcn,
                               error_handler);

          if (nargout > 0 && tmp.length () < nargout)
            error ("cellfun: function returned fewer than nargout values");

          if (nargout > 0
              || (nargout == 0
                  && tmp.length () > 0 && tmp(0).is_defined ()))
            {
              int num_to_copy = tmp.length ();

              if (num_to_copy > nargout1)
                num_to_copy = nargout1;

              if (num_to_copy > 0)
                have_some_output = true;

              for (int j = 0; j < num_to_copy; j++)
                results[j](count) = tmp(j);
            }
        }

      if (have_some_output || fdims.any_zero ())
        {
          retval.resize (nargout1);

          for (int j = 0; j < nargout1; j++)
            retval(j) = results[j];
        }
    }

  return retval;
}

/*

%!function r = __f11 (x)
%!  r = x;
%!endfunction

%!function __f01 (x)
%!  ## Empty function
%!endfunction

%!function varargout = __f02 (out)
%!  if (out)
%!    varargout{1} = out;
%!  endif
%!endfunction

%!test
%! __cellfun_test_num_outputs__ = -1;
%!
%! function r = __subf11 (x)
%!   __cellfun_test_num_outputs__ = nargout;
%!   r = x;
%! endfunction
%!
%! cellfun ("__subf11", {1});
%! assert (__cellfun_test_num_outputs__, 0);
%!
%! __cellfun_test_num_outputs__ = -1;
%! x = cellfun ("__subf11", {1});
%! assert (__cellfun_test_num_outputs__, 1);

%!test
%! __cellfun_test_num_outputs__ = -1;
%! function __subf01 (x)
%!   __cellfun_test_num_outputs__ = nargout;
%! endfunction
%! cellfun ("__subf01", {1});
%! assert (__cellfun_test_num_outputs__, 0);

%!error x = cellfun (@__f01, {1, 2})
%!error x = cellfun (@__f01, {1, 2})

%!error x = cellfun (@__f02, {0, 2}, "uniformoutput", false)
%!error x = cellfun (@__f02, {0, 2}, "uniformoutput", true)

%!test
%! assert (cellfun (@__f11, {1, 2}), [1, 2]);
%! assert (cellfun (@__f11, {1, 2}, 'uniformoutput', false), {1, 2});

%!test
%! [a,b] = cellfun (@(x) x, cell (2, 0));
%! assert (a, zeros (2, 0));
%! assert (b, zeros (2, 0));

%!test
%! [a,b] = cellfun (@(x) x, cell (2, 0), "uniformoutput", false);
%! assert (a, cell (2, 0));
%! assert (b, cell (2, 0));

## Test function to check the "Errorhandler" option
%!function z = __cellfunerror (S, varargin)
%!  z = S;
%!endfunction

## First input argument can be a string, an inline function,
## a function_handle or an anonymous function
%!test
%! A = cellfun ("islogical", {true, 0.1, false, i*2});
%! assert (A, [true, false, true, false]);
%!test
%! A = cellfun (inline ("islogical (x)", "x"), {true, 0.1, false, i*2});
%! assert (A, [true, false, true, false]);
%!test
%! A = cellfun (@islogical, {true, 0.1, false, i*2});
%! assert (A, [true, false, true, false]);
%!test
%! A = cellfun (@(x) islogical (x), {true, 0.1, false, i*2});
%! assert (A, [true, false, true, false]);

## First input argument can be the special string "isreal",
## "isempty", "islogical", "isnumeric", "length", "ndims" or "prodofsize"
%!test
%! A = cellfun ("isreal", {true, 0.1, {}, i*2, [], "abc"});
%! assert (A, [true, true, false, false, true, true]);
%!test
%! A = cellfun ("isempty", {true, 0.1, false, i*2, [], "abc"});
%! assert (A, [false, false, false, false, true, false]);
%!test
%! A = cellfun ("islogical", {true, 0.1, false, i*2, [], "abc"});
%! assert (A, [true, false, true, false, false, false]);
%!test
%! A = cellfun ("isnumeric", {true, 0.1, false, i*2, [], "abc"});
%! assert (A, [false, true, false, true, true, false]);
%!test
%! A = cellfun ("length", {true, 0.1, false, i*2, [], "abc"});
%! assert (A, [1, 1, 1, 1, 0, 3]);
%!test
%! A = cellfun ("ndims", {[1, 2; 3, 4]; (cell (1,2,3,4))});
%! assert (A, [2; 4]);
%!test
%! A = cellfun ("prodofsize", {[1, 2; 3, 4], (cell (1,2,3,4))});
%! assert (A, [4, 24]);

## Number of input and output arguments may not be limited to one
%!test
%! A = cellfun (@(x,y,z) x + y + z, {1, 1, 1}, {2, 2, 2}, {3, 4, 5});
%! assert (A, [6, 7, 8]);
%!test
%! A = cellfun (@(x,y,z) x + y + z, {1, 1, 1}, {2, 2, 2}, {3, 4, 5}, ...
%!              "UniformOutput", false);
%! assert (A, {6, 7, 8});
%!test  # Two input arguments of different types
%! A = cellfun (@(x,y) islogical (x) && ischar (y), {false, true}, {"a", 3});
%! assert (A, [true, false]);
%!test  # Pass another variable to the anonymous function
%! y = true;
%! A = cellfun (@(x) islogical (x) && y, {false, 0.3});
%! assert (A, [true, false]);
%!test  # Three output arguments of different type
%! [A, B, C] = cellfun (@find, {10, 11; 0, 12}, "UniformOutput", false);
%! assert (isequal (A, {true, true; [], true}));
%! assert (isequal (B, {true, true; [], true}));
%! assert (isequal (C, {10, 11; [], 12}));

## Input arguments can be of type cell array of logical
%!test
%! A = cellfun (@(x,y) x == y, {false, true}, {true, true});
%! assert (A, [false, true]);
%!test
%! A = cellfun (@(x,y) x == y, {false; true}, {true; true}, ...
%!              "UniformOutput", true);
%! assert (A, [false; true]);
%!test
%! A = cellfun (@(x) x, {false, true; false, true}, "UniformOutput", false);
%! assert (A, {false, true; false, true});
%!test  # Three output arguments of same type
%! [A, B, C] = cellfun (@find, {true, false; false, true}, ...
%!                      "UniformOutput", false);
%! assert (isequal (A, {true, []; [], true}));
%! assert (isequal (B, {true, []; [], true}));
%! assert (isequal (C, {true, []; [], true}));
%!test
%! A = cellfun (@(x,y) cell2str (x,y), {true}, {true}, ...
%!              "ErrorHandler", @__cellfunerror);
%! assert (isfield (A, "identifier"), true);
%! assert (isfield (A, "message"), true);
%! assert (isfield (A, "index"), true);
%! assert (isempty (A.message), false);
%! assert (A.index, 1);
%!test  # Overwriting setting of "UniformOutput" true
%! A = cellfun (@(x,y) cell2str (x,y), {true}, {true}, ...
%!              "UniformOutput", true, "ErrorHandler", @__cellfunerror);
%! assert (isfield (A, "identifier"), true);
%! assert (isfield (A, "message"), true);
%! assert (isfield (A, "index"), true);
%! assert (isempty (A.message), false);
%! assert (A.index, 1);

## Input arguments can be of type cell array of numeric
%!test
%! A = cellfun (@(x,y) x>y, {1.1, 4.2}, {3.1, 2+3*i});
%! assert (A, [false, true]);
%!test
%! A = cellfun (@(x,y) x>y, {1.1, 4.2; 2, 4}, {3.1, 2; 2, 4+2*i}, ...
%!              "UniformOutput", true);
%! assert (A, [false, true; false, false]);
%!test
%! A = cellfun (@(x,y) x:y, {1.1, 4}, {3.1, 6}, "UniformOutput", false);
%! assert (isequal (A{1}, [1.1, 2.1, 3.1]));
%! assert (isequal (A{2}, [4, 5, 6]));
%!test  # Three output arguments of different type
%! [A, B, C] = cellfun (@find, {10, 11; 0, 12}, "UniformOutput", false);
%! assert (isequal (A, {true, true; [], true}));
%! assert (isequal (B, {true, true; [], true}));
%! assert (isequal (C, {10, 11; [], 12}));
%!test
%! A = cellfun (@(x,y) cell2str (x,y), {1.1, 4}, {3.1, 6}, ...
%!              "ErrorHandler", @__cellfunerror);
%! B = isfield (A(1), "message") && isfield (A(1), "index");
%! assert ([(isfield (A(1), "identifier")), (isfield (A(2), "identifier"))],
%!         [true, true]);
%! assert ([(isfield (A(1), "message")), (isfield (A(2), "message"))],
%!         [true, true]);
%! assert ([(isfield (A(1), "index")), (isfield (A(2), "index"))],
%!         [true, true]);
%! assert ([(isempty (A(1).message)), (isempty (A(2).message))],
%!         [false, false]);
%! assert ([A(1).index, A(2).index], [1, 2]);
%!test  # Overwriting setting of "UniformOutput" true
%! A = cellfun (@(x,y) cell2str (x,y), {1.1, 4}, {3.1, 6}, ...
%!              "UniformOutput", true, "ErrorHandler", @__cellfunerror);
%! B = isfield (A(1), "message") && isfield (A(1), "index");
%! assert ([(isfield (A(1), "identifier")), (isfield (A(2), "identifier"))],
%!         [true, true]);
%! assert ([(isfield (A(1), "message")), (isfield (A(2), "message"))],
%!         [true, true]);
%! assert ([(isfield (A(1), "index")), (isfield (A(2), "index"))],
%!         [true, true]);
%! assert ([(isempty (A(1).message)), (isempty (A(2).message))],
%!         [false, false]);
%! assert ([A(1).index, A(2).index], [1, 2]);

## Input arguments can be of type cell arrays of character or strings
%!error  # "UniformOutput" false should be used
%! A = cellfun (@(x,y) x>y, {"ad", "c", "ghi"}, {"cc", "d", "fgh"});
%!test
%! A = cellfun (@(x,y) x>y, {"a"; "f"}, {"c"; "d"}, "UniformOutput", true);
%! assert (A, [false; true]);
%!test
%! A = cellfun (@(x,y) x:y, {"a", "d"}, {"c", "f"}, "UniformOutput", false);
%! assert (A, {"abc", "def"});
%!test
%! A = cellfun (@(x,y) cell2str (x,y), {"a", "d"}, {"c", "f"}, ...
%!              "ErrorHandler", @__cellfunerror);
%! assert ([(isfield (A(1), "identifier")), (isfield (A(2), "identifier"))],
%!         [true, true]);
%! assert ([(isfield (A(1), "message")), (isfield (A(2), "message"))],
%!         [true, true]);
%! assert ([(isfield (A(1), "index")), (isfield (A(2), "index"))],
%!         [true, true]);
%! assert ([(isempty (A(1).message)), (isempty (A(2).message))],
%!         [false, false]);
%! assert ([A(1).index, A(2).index], [1, 2]);
%!test  # Overwriting setting of "UniformOutput" true
%! A = cellfun (@(x,y) cell2str (x,y), {"a", "d"}, {"c", "f"}, ...
%!              "UniformOutput", true, "ErrorHandler", @__cellfunerror);
%! assert ([(isfield (A(1), "identifier")), (isfield (A(2), "identifier"))],
%!         [true, true]);
%! assert ([(isfield (A(1), "message")), (isfield (A(2), "message"))],
%!         [true, true]);
%! assert ([(isfield (A(1), "index")), (isfield (A(2), "index"))],
%!         [true, true]);
%! assert ([(isempty (A(1).message)), (isempty (A(2).message))],
%!         [false, false]);
%! assert ([A(1).index, A(2).index], [1, 2]);

## Structures cannot be handled by cellfun
%!error
%! vst1.a = 1.1;  vst1.b = 4.2;  vst2.a = 3.1;  vst2.b = 2;
%! A = cellfun (@(x,y) (x.a < y.a) && (x.b > y.b), vst1, vst2);

## Input arguments can be of type cell array of cell arrays
%!test
%! A = cellfun (@(x,y) x{1} < y{1}, {{1.1}, {4.2}}, {{3.1}, {2}});
%! assert (A, [1, 0], 1e-16);
%!test
%! A = cellfun (@(x,y) x{1} < y{1}, {{1.1}; {4.2}}, {{3.1}; {2}}, ...
%!              "UniformOutput", true);
%! assert (A, [1; 0], 1e-16);
%!test
%! A = cellfun (@(x,y) x{1} < y{1}, {{1.1}, {4.2}}, {{3.1}, {2}}, ...
%!              "UniformOutput", false);
%! assert (A, {true, false});
%!test
%! A = cellfun (@(x,y) mat2str (x,y), {{1.1}, {4.2}}, {{3.1}, {2}}, ...
%!              "ErrorHandler", @__cellfunerror);
%! assert ([(isfield (A(1), "identifier")), (isfield (A(2), "identifier"))],
%!         [true, true]);
%! assert ([(isfield (A(1), "message")), (isfield (A(2), "message"))],
%!         [true, true]);
%! assert ([(isfield (A(1), "index")), (isfield (A(2), "index"))],
%!         [true, true]);
%! assert ([(isempty (A(1).message)), (isempty (A(2).message))],
%!         [false, false]);
%! assert ([A(1).index, A(2).index], [1, 2]);
%!test  # Overwriting setting of "UniformOutput" true
%! A = cellfun (@(x,y) mat2str (x,y), {{1.1}, {4.2}}, {{3.1}, {2}}, ...
%!              "UniformOutput", true, "ErrorHandler", @__cellfunerror);
%! assert ([(isfield (A(1), "identifier")), (isfield (A(2), "identifier"))],
%!         [true, true]);
%! assert ([(isfield (A(1), "message")), (isfield (A(2), "message"))],
%!         [true, true]);
%! assert ([(isfield (A(1), "index")), (isfield (A(2), "index"))],
%!         [true, true]);
%! assert ([(isempty (A(1).message)), (isempty (A(2).message))],
%!         [false, false]);
%! assert ([A(1).index, A(2).index], [1, 2]);

## Input arguments can be of type cell array of structure arrays
%!test
%! a = struct ("a", 1, "b", 2);  b = struct ("a", 1, "b", 3);
%! A = cellfun (@(x,y) (x.a == y.a) && (x.b < y.b), {a}, {b});
%! assert (A, true);
%!test
%! a = struct ("a", 1, "b", 2);  b = struct ("a", 1, "b", 3);
%! A = cellfun (@(x,y) (x.a == y.a) && (x.b < y.b) , {a}, {b}, ...
%!              "UniformOutput", true);
%! assert (A, true);
%!test
%! a = struct ("a", 1, "b", 2);  b = struct ("a", 1, "b", 3);
%! A = cellfun (@(x,y) (x.a == y.a) && (x.b < y.b) , {a}, {b}, ...
%!              "UniformOutput", false);
%! assert (A, {true});
%!test
%! a = struct ("a", 1, "b", 2);  b = struct ("a", 1, "b", 3);
%! A = cellfun (@(x,y) cell2str (x.a, y.a), {a}, {b}, ...
%!              "ErrorHandler", @__cellfunerror);
%! assert (isfield (A, "identifier"), true);
%! assert (isfield (A, "message"), true);
%! assert (isfield (A, "index"), true);
%! assert (isempty (A.message), false);
%! assert (A.index, 1);
%!test  # Overwriting setting of "UniformOutput" true
%! a = struct ("a", 1, "b", 2);  b = struct ("a", 1, "b", 3);
%! A = cellfun (@(x,y) cell2str (x.a, y.a), {a}, {b}, ...
%!              "UniformOutput", true, "ErrorHandler", @__cellfunerror);
%! assert (isfield (A, "identifier"), true);
%! assert (isfield (A, "message"), true);
%! assert (isfield (A, "index"), true);
%! assert (isempty (A.message), false);
%! assert (A.index, 1);

## A lot of other tests
%!assert (cellfun (@sin, {0,1}), sin ([0,1]))
%!assert (cellfun (inline ("sin (x)"), {0,1}), sin ([0,1]))
%!assert (cellfun ("sin", {0,1}), sin ([0,1]))
%!assert (cellfun ("isempty", {1,[]}), [false,true])
%!assert (cellfun ("islogical", {false,pi}), [true,false])
%!assert (cellfun ("isnumeric", {false,pi,struct()}), [false,true,false])
%!assert (cellfun ("isreal", {1i,1}), [false,true])
%!assert (cellfun ("length", {zeros(2,2),1}), [2,1])
%!assert (cellfun ("prodofsize", {zeros(2,2),1}), [4,1])
%!assert (cellfun ("ndims", {zeros([2,2,2]),1}), [3,2])
%!assert (cellfun ("isclass", {zeros([2,2,2]),"test"}, "double"), [true,false])
%!assert (cellfun ("size", {zeros([1,2,3]),1}, 1), [1,1])
%!assert (cellfun ("size", {zeros([1,2,3]),1}, 2), [2,1])
%!assert (cellfun ("size", {zeros([1,2,3]),1}, 3), [3,1])
%!assert (cellfun (@atan2, {1,1}, {1,2}), [atan2(1,1), atan2(1,2)])
%!assert (cellfun (@atan2, {1,1}, {1,2},"UniformOutput", false),
%!        {atan2(1,1), atan2(1,2)})
%!assert (cellfun (@sin, {1,2;3,4}), sin ([1,2;3,4]))
%!assert (cellfun (@atan2, {1,1;1,1}, {1,2;1,2}), atan2 ([1,1;1,1],[1,2;1,2]))
%!error cellfun (@factorial, {-1,3})
%!assert (cellfun (@factorial,{-1,3},"ErrorHandler",@(x,y) NaN), [NaN,6])
%!assert (cellfun (@(x) x(2),{[1],[1,2]},"ErrorHandler",@(x,y) NaN), [NaN,2])
%!test
%! [a,b,c] = cellfun (@fileparts, {fullfile("a","b","c.d"), fullfile("e","f","g.h")}, "UniformOutput", false);
%! assert (a, {fullfile("a","b"), fullfile("e","f")});
%! assert (b, {"c", "g"});
%! assert (c, {".d", ".h"});

%!assert <*40467> (cellfun (@isreal, {1 inf nan []}), [true, true, true, true])
%!assert <*40467> (cellfun (@isreal, {1 inf nan []}, "UniformOutput", false),
%!                 {true, true, true, true})
%!assert <*40467> (cellfun (@iscomplex, {1 inf nan []}),
%!                 [false, false, false, false])
%!assert <*40467> (cellfun (@iscomplex, {1 inf nan []}, "UniformOutput", false),
%!                 {false, false, false, false})

%!error cellfun (1)
%!error cellfun ("isclass", 1)
%!error cellfun ("size", 1)
%!error cellfun (@sin, {[]}, "BadParam", false)
%!error cellfun (@sin, {[]}, "UniformOuput")
%!error cellfun (@sin, {[]}, "ErrorHandler")

%!function retval = __errfcn (S, varargin)
%!  global __errmsg;
%!  __errmsg = S.message;
%!  retval = NaN;
%!endfunction
%!test <*58411>
%! global __errmsg;
%! assert (cellfun (@factorial, {1, 2, -3}, "ErrorHandler", @__errfcn),
%!         [1, 2, NaN]);
%! assert (! isempty (__errmsg));
%! clear -global __errmsg;
*/

// Arrayfun was originally a .m file written by Bill Denney and Jaroslav
// Hajek.  It was converted to C++ by jwe so that it could properly
// handle the nargout = 0 case.

DEFMETHOD (arrayfun, interp, args, nargout,
           doc: /* -*- texinfo -*-
@deftypefn  {} {@var{B} =} arrayfun (@var{fcn}, @var{A})
@deftypefnx {} {@var{B} =} arrayfun (@var{fcn}, @var{A1}, @var{A2}, @dots{})
@deftypefnx {} {[@var{B1}, @var{B2}, @dots{}] =} arrayfun (@var{fcn}, @var{A}, @dots{})
@deftypefnx {} {@var{B} =} arrayfun (@dots{}, "UniformOutput", @var{val})
@deftypefnx {} {@var{B} =} arrayfun (@dots{}, "ErrorHandler", @var{errfcn})

Execute a function on each element of an array.

This is useful for functions that do not accept array arguments.  If the
function does accept array arguments it is @emph{better} to call the function
directly.

The first input argument @var{fcn} can be a string, a function handle, an
inline function, or an anonymous function.  The input argument @var{A} can be a
logical array, a numeric array, a string array, a structure array, or a cell
array.  @code{arrayfun} passes all elements of @var{A} individually to the
function @var{fcn} and collects the results.  The equivalent pseudo-code is

@example
@group
cls = class (@var{fcn} (@var{A}(1));
@var{B} = zeros (size (@var{A}), cls);
for i = 1:numel (@var{A})
  @var{B}(i) = @var{fcn} (@var{A}(i))
endfor
@end group
@end example

The named function can also take more than two input arguments, with the input
arguments given as third input argument @var{A2}, fourth input argument
@var{A2}, @dots{}  If given more than one array input argument then all input
arguments must have the same sizes.  For example:

@example
@group
arrayfun (@@atan2, [1, 0], [0, 1])
     @result{} [ 1.57080   0.00000 ]
@end group
@end example

If the parameter @var{val} after a further string input argument
@qcode{"UniformOutput"} is set @code{true} (the default), then the named
function @var{fcn} must return a single element which then will be concatenated
into the return value and is of type matrix.  Otherwise, if that parameter is
set to @code{false}, then the outputs are concatenated in a cell array.  For
example:

@example
@group
arrayfun (@@(x,y) x:y, "abc", "def", "UniformOutput", false)
@result{}
   @{
     [1,1] = abcd
     [1,2] = bcde
     [1,3] = cdef
   @}
@end group
@end example

If more than one output arguments are given then the named function must return
the number of return values that also are expected, for example:

@example
@group
[A, B, C] = arrayfun (@@find, [10; 0], "UniformOutput", false)
@result{}
A =
@{
   [1,1] =  1
   [2,1] = [](0x0)
@}
B =
@{
   [1,1] =  1
   [2,1] = [](0x0)
@}
C =
@{
   [1,1] =  10
   [2,1] = [](0x0)
@}
@end group
@end example

If the parameter @var{errfcn} after a further string input argument
@qcode{"ErrorHandler"} is another string, a function handle, an inline
function, or an anonymous function, then @var{errfcn} defines a function to
call in the case that @var{fcn} generates an error.  The definition of the
function must be of the form

@example
function [@dots{}] = errfcn (@var{s}, @dots{})
@end example

@noindent
where there is an additional input argument to @var{errfcn} relative to
@var{fcn}, given by @var{s}.  This is a structure with the elements
@qcode{"identifier"}, @qcode{"message"}, and @qcode{"index"} giving,
respectively, the error identifier, the error message, and the index of the
array elements that caused the error.  The size of the output argument of
@var{errfcn} must have the same size as the output argument of @var{fcn},
otherwise a real error is thrown.  For example:

@example
@group
function y = ferr (s, x), y = "MyString"; endfunction
arrayfun (@@str2num, [1234],
          "UniformOutput", false, "ErrorHandler", @@ferr)
@result{}
   @{
     [1,1] = MyString
   @}
@end group
@end example

@seealso{spfun, cellfun, structfun}
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin < 2)
    print_usage ();

  octave_value_list retval;
  int nargout1 = (nargout < 1 ? 1 : nargout);
  bool symbol_table_lookup = false;
  octave_value fcn = args(0);

  symbol_table& symtab = interp.get_symbol_table ();

  if (fcn.is_string ())
    {
      // See if we can convert the string into a function.
      std::string name = args(0).string_value ();

      if (! valid_identifier (name))
        fcn = get_function_handle (interp, args(0), "x");
      else
        {
          fcn = symtab.find_function (name);

          if (fcn.is_undefined ())
            error_with_id ("Octave:invalid-input-arg",
                           "arrayfun: invalid function NAME: %s",
                           name.c_str ());

          symbol_table_lookup = true;
        }
    }

  if (fcn.is_function_handle () || fcn.is_inline_function ()
      || fcn.is_function ())
    {
      // The following is an optimization because the symbol table can give a
      // more specific function class, so this can result in fewer polymorphic
      // function calls as the function gets called for each value of the array.

      if (! symbol_table_lookup)
        {
          if (fcn.is_function_handle () || fcn.class_name () == "inline")
            {
              // FIXME: instead of checking for overloaded functions as
              // we did previously but is no longer possible, we could
              // check whether the types of all the cell array elements
              // are the same, then lookup the function for that type
              // just once.

              goto nevermind;
            }

          octave_value f
            = symtab.find_function (fcn.function_value () -> name ());

          if (f.is_defined ())
            fcn = f;
        }

    nevermind:

      bool uniform_output = true;
      octave_value error_handler;

      get_mapper_fun_options (symtab, args, nargin, uniform_output,
                              error_handler);

      octave_value_list inputlist (nargin, octave_value ());

      OCTAVE_LOCAL_BUFFER (octave_value, inputs, nargin);
      OCTAVE_LOCAL_BUFFER (bool, mask, nargin);

      octave_idx_type k = 1;

      dim_vector fdims (1, 1);

      // Collect arguments.  Pre-fill scalar elements of inputlist array.

      for (int j = 0; j < nargin; j++)
        {
          inputs[j] = args(j+1);
          mask[j] = inputs[j].numel () != 1;

          if (! mask[j])
            inputlist(j) = inputs[j];
        }

      for (int j = 0; j < nargin; j++)
        {
          if (mask[j])
            {
              fdims = inputs[j].dims ();
              k = inputs[j].numel ();

              for (int i = j+1; i < nargin; i++)
                {
                  if (mask[i] && inputs[i].dims () != fdims)
                    error_with_id ("Octave:invalid-input-arg",
                                   "arrayfun: dimensions mismatch");
                }
              break;
            }
        }

      error_system& es = interp.get_error_system ();

      // Apply functions.

      if (uniform_output)
        {
          std::list<octave_value_list> idx_list (1);
          idx_list.front ().resize (1);
          std::string idx_type = "(";

          OCTAVE_LOCAL_BUFFER (octave_value, retv, nargout1);

          for (octave_idx_type count = 0; count < k; count++)
            {
              idx_list.front ()(0) = count + 1.0;

              for (int j = 0; j < nargin; j++)
                {
                  if (mask[j])
                    inputlist.xelem (j) = inputs[j].index_op (idx_list);
                }

              const octave_value_list tmp
                = get_output_list (es, count, nargout, inputlist, fcn,
                                   error_handler);

              if (nargout > 0 && tmp.length () < nargout)
                error_with_id ("Octave:invalid-fun-call",
                               "arrayfun: function returned fewer than nargout values");

              if (nargout > 0
                  || (nargout == 0
                      && tmp.length () > 0 && tmp(0).is_defined ()))
                {
                  int num_to_copy = tmp.length ();

                  if (num_to_copy > nargout1)
                    num_to_copy = nargout1;

                  if (count == 0)
                    {
                      for (int j = 0; j < num_to_copy; j++)
                        {
                          if (tmp(j).is_defined ())
                            {
                              octave_value val = tmp(j);

                              if (val.numel () == 1)
                                retv[j] = val.resize (fdims);
                              else
                                error_with_id ("Octave:invalid-fun-call",
                                               "arrayfun: all values must be scalars when UniformOutput = true");
                            }
                        }
                    }
                  else
                    {
                      for (int j = 0; j < num_to_copy; j++)
                        {
                          if (tmp(j).is_defined ())
                            {
                              octave_value val = tmp(j);

                              if (! retv[j].fast_elem_insert (count, val))
                                {
                                  if (val.numel () == 1)
                                    {
                                      idx_list.front ()(0) = count + 1.0;
                                      retv[j].assign (octave_value::op_asn_eq,
                                                      idx_type, idx_list, val);
                                    }
                                  else
                                    error_with_id ("Octave:invalid-fun-call",
                                                   "arrayfun: all values must be scalars when UniformOutput = true");
                                }
                            }
                        }
                    }
                }
            }

          retval.resize (nargout1);

          for (int j = 0; j < nargout1; j++)
            {
              if (nargout > 0 && retv[j].is_undefined ())
                retval(j) = NDArray (fdims);
              else
                retval(j) = retv[j];
            }
        }
      else
        {
          std::list<octave_value_list> idx_list (1);
          idx_list.front ().resize (1);
          std::string idx_type = "(";

          OCTAVE_LOCAL_BUFFER (Cell, results, nargout1);

          for (int j = 0; j < nargout1; j++)
            results[j].resize (fdims, Matrix ());

          bool have_some_output = false;

          for (octave_idx_type count = 0; count < k; count++)
            {
              idx_list.front ()(0) = count + 1.0;

              for (int j = 0; j < nargin; j++)
                {
                  if (mask[j])
                    inputlist.xelem (j) = inputs[j].index_op (idx_list);
                }

              const octave_value_list tmp
                = get_output_list (es, count, nargout, inputlist, fcn,
                                   error_handler);

              if (nargout > 0 && tmp.length () < nargout)
                error_with_id ("Octave:invalid-fun-call",
                               "arrayfun: function returned fewer than nargout values");

              if (nargout > 0
                  || (nargout == 0
                      && tmp.length () > 0 && tmp(0).is_defined ()))
                {
                  int num_to_copy = tmp.length ();

                  if (num_to_copy > nargout1)
                    num_to_copy = nargout1;

                  if (num_to_copy > 0)
                    have_some_output = true;

                  for (int j = 0; j < num_to_copy; j++)
                    results[j](count) = tmp(j);
                }
            }

          if (have_some_output || fdims.any_zero ())
            {
              retval.resize (nargout1);

              for (int j = 0; j < nargout1; j++)
                retval(j) = results[j];
            }
        }
    }
  else
    error_with_id ("Octave:invalid-fun-call",
                   "arrayfun: argument NAME must be a string or function handle");

  return retval;
}

/*
%!function r = __f11 (x)
%!  r = x;
%!endfunction

%!function __f01 (x)
%!  ## Empty function
%!endfunction

%!test
%! __arrayfun_test_num_outputs__ = -1;
%!
%! function r = __subf11 (x)
%!   __arrayfun_test_num_outputs__ = nargout;
%!   r = x;
%! endfunction
%!
%! arrayfun ("__subf11", {1});
%! assert (__arrayfun_test_num_outputs__, 0);
%!
%! __arrayfun_test_num_outputs__ = -1;
%! x = arrayfun ("__subf11", {1});
%! assert (__arrayfun_test_num_outputs__, 1);

%!test
%! __arrayfun_test_num_outputs__ = -1;
%!
%! function __subf01 (x)
%!   __arrayfun_test_num_outputs__ = nargout;
%! endfunction
%!
%! arrayfun ("__subf01", {1});
%! assert (__arrayfun_test_num_outputs__, 0);

%!error x = arrayfun (@__f01, [1, 2])

%!test
%! assert (arrayfun (@__f11, [1, 2]), [1, 2]);
%! assert (arrayfun (@__f11, [1, 2], "uniformoutput", false), {1, 2});
%! assert (arrayfun (@__f11, {1, 2}), {1, 2});
%! assert (arrayfun (@__f11, {1, 2}, "uniformoutput", false), {{1}, {2}});

%!assert (arrayfun (@ones, 1, [2,3], "uniformoutput", false), {[1,1], [1,1,1]})

## Test function to check the "Errorhandler" option
%!function z = __arrayfunerror (S, varargin)
%!  z = S;
%!endfunction
## First input argument can be a string, an inline function, a
## function_handle or an anonymous function
%!test
%! arrayfun (@isequal, [false, true], [true, true]);  # No output argument
%!error
%! arrayfun (@isequal);  # One or less input arguments
%!test
%! A = arrayfun ("isequal", [false, true], [true, true]);
%! assert (A, [false, true]);
%!test
%! A = arrayfun (inline ("(x == y)", "x", "y"), [false, true], [true, true]);
%! assert (A, [false, true]);
%!test
%! A = arrayfun (@isequal, [false, true], [true, true]);
%! assert (A, [false, true]);
%!test
%! A = arrayfun (@(x,y) isequal (x,y), [false, true], [true, true]);
%! assert (A, [false, true]);

## Number of input and output arguments may be greater than one
%!test
%! A = arrayfun (@(x) islogical (x), false);
%! assert (A, true);
%!test
%! A = arrayfun (@(x,y,z) x + y + z, [1, 1, 1], [2, 2, 2], [3, 4, 5]);
%! assert (A, [6, 7, 8], 1e-16);
%!test  # Two input arguments of different types
%! A = arrayfun (@(x,y) islogical (x) && ischar (y), false, "a");
%! assert (A, true);
%!test  # Pass another variable to the anonymous function
%! y = true;
%! A = arrayfun (@(x) islogical (x && y), false);
%! assert (A, true);
%!test  # Three output arguments of different type
%! [A, B, C] = arrayfun (@find, [10, 11; 0, 12], "UniformOutput", false);
%! assert (isequal (A, {true, true; [], true}));
%! assert (isequal (B, {true, true; [], true}));
%! assert (isequal (C, {10, 11; [], 12}));

## Input arguments can be of type logical
%!test
%! A = arrayfun (@(x,y) x == y, [false, true], [true, true]);
%! assert (A, [false, true]);
%!test
%! A = arrayfun (@(x,y) x == y, [false; true], [true; true], "UniformOutput", true);
%! assert (A, [false; true]);
%!test
%! A = arrayfun (@(x) x, [false, true, false, true], "UniformOutput", false);
%! assert (A, {false, true, false, true});
%!test  # Three output arguments of same type
%! [A, B, C] = arrayfun (@find, [true, false; false, true], "UniformOutput", false);
%! assert (isequal (A, {true, []; [], true}));
%! assert (isequal (B, {true, []; [], true}));
%! assert (isequal (C, {true, []; [], true}));
%!test
%! A = arrayfun (@(x,y) array2str (x,y), true, true, ...
%!               "ErrorHandler", @__arrayfunerror);
%! assert (isfield (A, "identifier"), true);
%! assert (isfield (A, "message"), true);
%! assert (isfield (A, "index"), true);
%! assert (isempty (A.message), false);
%! assert (A.index, 1);
%!test  # Overwriting setting of "UniformOutput" true
%! A = arrayfun (@(x,y) array2str (x,y), true, true, "UniformOutput", true, ...
%!               "ErrorHandler", @__arrayfunerror);
%! assert (isfield (A, "identifier"), true);
%! assert (isfield (A, "message"), true);
%! assert (isfield (A, "index"), true);
%! assert (isempty (A.message), false);
%! assert (A.index, 1);

## Input arguments can be of type numeric
%!test
%! A = arrayfun (@(x,y) x>y, [1.1, 4.2], [3.1, 2+3*i]);
%! assert (A, [false, true]);
%!test
%! A = arrayfun (@(x,y) x>y, [1.1, 4.2; 2, 4], [3.1, 2; 2, 4+2*i], "UniformOutput", true);
%! assert (A, [false, true; false, false]);
%!test
%! A = arrayfun (@(x,y) x:y, [1.1, 4], [3.1, 6], "UniformOutput", false);
%! assert (isequal (A{1}, [1.1, 2.1, 3.1]));
%! assert (isequal (A{2}, [4, 5, 6]));
%!test  # Three output arguments of different type
%! [A, B, C] = arrayfun (@find, [10, 11; 0, 12], "UniformOutput", false);
%! assert (isequal (A, {true, true; [], true}));
%! assert (isequal (B, {true, true; [], true}));
%! assert (isequal (C, {10, 11; [], 12}));
%!test
%! A = arrayfun (@(x,y) array2str (x,y), {1.1, 4}, {3.1, 6}, ...
%!               "ErrorHandler", @__arrayfunerror);
%! B = isfield (A(1), "message") && isfield (A(1), "index");
%! assert ([(isfield (A(1), "identifier")), (isfield (A(2), "identifier"))],
%!         [true, true]);
%! assert ([(isfield (A(1), "message")), (isfield (A(2), "message"))],
%!         [true, true]);
%! assert ([(isfield (A(1), "index")), (isfield (A(2), "index"))],
%!         [true, true]);
%! assert ([(isempty (A(1).message)), (isempty (A(2).message))],
%!         [false, false]);
%! assert ([A(1).index, A(2).index], [1, 2]);
%!test  # Overwriting setting of "UniformOutput" true
%! A = arrayfun (@(x,y) array2str (x,y), {1.1, 4}, {3.1, 6}, ...
%!               "UniformOutput", true, "ErrorHandler", @__arrayfunerror);
%! B = isfield (A(1), "message") && isfield (A(1), "index");
%! assert ([(isfield (A(1), "identifier")), (isfield (A(2), "identifier"))],
%!         [true, true]);
%! assert ([(isfield (A(1), "message")), (isfield (A(2), "message"))],
%!         [true, true]);
%! assert ([(isfield (A(1), "index")), (isfield (A(2), "index"))],
%!         [true, true]);
%! assert ([(isempty (A(1).message)), (isempty (A(2).message))],
%!         [false, false]);
%! assert ([A(1).index, A(2).index], [1, 2]);

## Input arguments can be of type character or strings
%!test
%! A = arrayfun (@(x,y) x>y, ["ad", "c", "ghi"], ["cc", "d", "fgh"]);
%! assert (A, [false, true, false, true, true, true]);
%!test
%! A = arrayfun (@(x,y) x>y, ["a"; "f"], ["c"; "d"], "UniformOutput", true);
%! assert (A, [false; true]);
%!test
%! A = arrayfun (@(x,y) x:y, ["a", "d"], ["c", "f"], "UniformOutput", false);
%! assert (A, {"abc", "def"});
%!test
%! A = arrayfun (@(x,y) cell2str (x,y), ["a", "d"], ["c", "f"], ...
%!               "ErrorHandler", @__arrayfunerror);
%! B = isfield (A(1), "identifier") && isfield (A(1), "message") ...
%!     && isfield (A(1), "index");
%! assert (B, true);

## Input arguments can be of type structure
%!test
%! a = struct ("a", 1.1, "b", 4.2);  b = struct ("a", 3.1, "b", 2);
%! A = arrayfun (@(x,y) (x.a < y.a) && (x.b > y.b), a, b);
%! assert (A, true);
%!test
%! a = struct ("a", 1.1, "b", 4.2);  b = struct ("a", 3.1, "b", 2);
%! A = arrayfun (@(x,y) (x.a < y.a) && (x.b > y.b), a, b, "UniformOutput", true);
%! assert (A, true);
%!test
%! a = struct ("a", 1.1, "b", 4.2);  b = struct ("a", 3.1, "b", 2);
%! A = arrayfun (@(x,y) x.a:y.a, a, b, "UniformOutput", false);
%! assert (isequal (A, {[1.1, 2.1, 3.1]}));
%!test
%! A = arrayfun (@(x) mat2str (x), "a", "ErrorHandler", @__arrayfunerror);
%! assert (isfield (A, "identifier"), true);
%! assert (isfield (A, "message"), true);
%! assert (isfield (A, "index"), true);
%! assert (isempty (A.message), false);
%! assert (A.index, 1);
%!test  # Overwriting setting of "UniformOutput" true
%! A = arrayfun (@(x) mat2str (x), "a", "UniformOutput", true, ...
%!               "ErrorHandler", @__arrayfunerror);
%! assert (isfield (A, "identifier"), true);
%! assert (isfield (A, "message"), true);
%! assert (isfield (A, "index"), true);
%! assert (isempty (A.message), false);
%! assert (A.index, 1);

## Input arguments can be of type cell array
%!test
%! A = arrayfun (@(x,y) x{1} < y{1}, {1.1, 4.2}, {3.1, 2});
%! assert (A, [true, false]);
%!test
%! A = arrayfun (@(x,y) x{1} < y{1}, {1.1; 4.2}, {3.1; 2}, "UniformOutput", true);
%! assert (A, [true; false]);
%!test
%! A = arrayfun (@(x,y) x{1} < y{1}, {1.1, 4.2}, {3.1, 2}, "UniformOutput", false);
%! assert (A, {true, false});
%!test
%! A = arrayfun (@(x,y) num2str(x,y), {1.1, 4.2}, {3.1, 2}, "ErrorHandler", @__arrayfunerror);
%! assert ([(isfield (A(1), "identifier")), (isfield (A(2), "identifier"))],
%!         [true, true]);
%! assert ([(isfield (A(1), "message")), (isfield (A(2), "message"))],
%!         [true, true]);
%! assert ([(isfield (A(1), "index")), (isfield (A(2), "index"))],
%!         [true, true]);
%! assert ([(isempty (A(1).message)), (isempty (A(2).message))],
%!         [false, false]);
%! assert ([A(1).index, A(2).index], [1, 2]);
%!test
%! A = arrayfun (@(x,y) num2str (x,y), {1.1, 4.2}, {3.1, 2}, ...
%!               "UniformOutput", true, "ErrorHandler", @__arrayfunerror);
%! assert ([(isfield (A(1), "identifier")), (isfield (A(2), "identifier"))],
%!         [true, true]);
%! assert ([(isfield (A(1), "message")), (isfield (A(2), "message"))],
%!         [true, true]);
%! assert ([(isfield (A(1), "index")), (isfield (A(2), "index"))],
%!         [true, true]);
%! assert ([(isempty (A(1).message)), (isempty (A(2).message))],
%!         [false, false]);
%! assert ([A(1).index, A(2).index], [1, 2]);
*/

static void
do_num2cell_helper (const dim_vector& dv,
                    const Array<int>& dimv,
                    dim_vector& celldv, dim_vector& arraydv,
                    Array<int>& perm)
{
  int dvl = dimv.numel ();
  int maxd = dv.ndims ();
  celldv = dv;
  for (int i = 0; i < dvl; i++)
    maxd = std::max (maxd, dimv(i));
  if (maxd > dv.ndims ())
    celldv.resize (maxd, 1);
  arraydv = celldv;

  OCTAVE_LOCAL_BUFFER_INIT (bool, sing, maxd, false);

  perm.clear (maxd, 1);
  for (int i = 0; i < dvl; i++)
    {
      int k = dimv(i) - 1;
      if (k < 0)
        error ("num2cell: dimension indices must be positive");

      if (i > 0 && k < dimv(i-1) - 1)
        error ("num2cell: dimension indices must be strictly increasing");

      sing[k] = true;
      perm(i) = k;
    }

  for (int k = 0, i = dvl; k < maxd; k++)
    if (! sing[k])
      perm(i++) = k;

  for (int i = 0; i < maxd; i++)
    if (sing[i])
      celldv(i) = 1;
    else
      arraydv(i) = 1;
}

template <typename NDA>
static inline typename NDA::element_type
do_num2cell_elem (const NDA& array, octave_idx_type i)
{ return array(i); }

static inline Cell
do_num2cell_elem (const Cell& array, octave_idx_type i)
{ return Cell (array(i)); }

template <typename NDA>
static Cell
do_num2cell (const NDA& array, const Array<int>& dimv)
{
  if (dimv.isempty ())
    {
      Cell retval (array.dims ());
      octave_idx_type nel = array.numel ();
      for (octave_idx_type i = 0; i < nel; i++)
        retval.xelem (i) = do_num2cell_elem (array, i);

      return retval;
    }
  else
    {
      dim_vector celldv, arraydv;
      Array<int> perm;
      do_num2cell_helper (array.dims (), dimv, celldv, arraydv, perm);

      NDA parray = array.permute (perm);

      octave_idx_type nela = arraydv.numel ();
      octave_idx_type nelc = celldv.numel ();
      parray = parray.reshape (dim_vector (nela, nelc));

      Cell retval (celldv);
      for (octave_idx_type i = 0; i < nelc; i++)
        {
          retval.xelem (i) = NDA (parray.column (i).reshape (arraydv));
        }

      return retval;
    }
}

// FIXME: this is a mess, but if a size method for the object exists,
// we have to call it to get the size of the object instead of using the
// internal dims method.

static dim_vector
get_object_dims (octave_value& obj)
{
  dim_vector retval;

  Matrix m = obj.size ();

  int n = m.numel ();

  retval.resize (n);

  for (int i = 0; i < n; i++)
    retval(i) = m(i);

  return retval;
}

static Cell
do_object2cell (const octave_value& obj, const Array<int>& dimv)
{
  Cell retval;

  // FIXME: this copy is only needed because the octave_value::size
  // method is not const.
  octave_value array = obj;

  if (! dimv.isempty ())
    error ("num2cell (A, dim) not implemented for class objects");

  dim_vector dv = get_object_dims (array);

  retval.resize (dv);

  octave_value_list idx (1);

  for (octave_idx_type i = 0; i < dv.numel (); i++)
    {
      octave_quit ();

      idx(0) = double (i+1);

      retval.xelem (i) = array.single_subsref ("(", idx);
    }

  return retval;
}

DEFUN (num2cell, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{C} =} num2cell (@var{A})
@deftypefnx {} {@var{C} =} num2cell (@var{A}, @var{dim})
Convert the numeric matrix @var{A} to a cell array.

When no @var{dim} is specified, each element of @var{A} becomes a 1x1 element
in the output @var{C}.

If @var{dim} is defined then individual elements of @var{C} contain all of the
elements from @var{A} along the specified dimension.  @var{dim} may also be a
vector of dimensions with the same rule applied.

For example:

@example
x = [1,2;3,4]
@result{}
    1    2
    3    4

## each element of A becomes a 1x1 element of C
num2cell (x)
   @result{}
      @{
        [1,1] =  1
        [2,1] =  3
        [1,2] =  2
        [2,2] =  4
      @}
## all rows (dim 1) of A appear in each element of C
num2cell (x, 1)
   @result{}
      @{
        [1,1] =
           1
           3
        [1,2] =
           2
           4
      @}
## all columns (dim 2) of A appear in each element of C
num2cell (x, 2)
   @result{}
      @{
        [1,1] =
           1   2
        [2,1] =
           3   4
      @}
## all rows and cols appear in each element of C
## (hence, only 1 output)
num2cell (x, [1, 2])
   @result{}
      @{
        [1,1] =
           1   2
           3   4
      @}
@end example

@seealso{mat2cell}
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin < 1 || nargin > 2)
    print_usage ();

  octave_value retval;

  octave_value array = args(0);

  Array<int> dimv;
  if (nargin > 1)
    dimv = args(1).int_vector_value (true);

  if (array.islogical ())
    retval = do_num2cell (array.bool_array_value (), dimv);
  else if (array.is_char_matrix ())
    retval = do_num2cell (array.char_array_value (), dimv);
  else if (array.isnumeric ())
    {
      if (array.isinteger ())
        {
          if (array.is_int8_type ())
            retval = do_num2cell (array.int8_array_value (), dimv);
          else if (array.is_int16_type ())
            retval = do_num2cell (array.int16_array_value (), dimv);
          else if (array.is_int32_type ())
            retval = do_num2cell (array.int32_array_value (), dimv);
          else if (array.is_int64_type ())
            retval = do_num2cell (array.int64_array_value (), dimv);
          else if (array.is_uint8_type ())
            retval = do_num2cell (array.uint8_array_value (), dimv);
          else if (array.is_uint16_type ())
            retval = do_num2cell (array.uint16_array_value (), dimv);
          else if (array.is_uint32_type ())
            retval = do_num2cell (array.uint32_array_value (), dimv);
          else if (array.is_uint64_type ())
            retval = do_num2cell (array.uint64_array_value (), dimv);
        }
      else if (array.iscomplex ())
        {
          if (array.is_single_type ())
            retval = do_num2cell (array.float_complex_array_value (), dimv);
          else
            retval = do_num2cell (array.complex_array_value (), dimv);
        }
      else
        {
          if (array.is_single_type ())
            retval = do_num2cell (array.float_array_value (), dimv);
          else
            retval = do_num2cell (array.array_value (), dimv);
        }
    }
  else if (array.isobject ())
    retval = do_object2cell (array, dimv);
  else if (array.isstruct ())
    retval = do_num2cell (array.map_value (), dimv);
  else if (array.iscell ())
    retval = do_num2cell (array.cell_value (), dimv);
  else
    err_wrong_type_arg ("num2cell", array);

  return retval;
}

/*
%!assert (num2cell ([1,2;3,4]), {1,2;3,4})
%!assert (num2cell ([1,2;3,4], 1), {[1;3],[2;4]})
%!assert (num2cell ([1,2;3,4], 2), {[1,2];[3,4]})
*/

static bool
mat2cell_mismatch (const dim_vector& dv,
                   const Array<octave_idx_type> *d, int nd)
{
  for (int i = 0; i < nd; i++)
    {
      octave_idx_type s = 0;
      for (octave_idx_type j = 0; j < d[i].numel (); j++)
        s += d[i](j);

      octave_idx_type r = (i < dv.ndims () ? dv(i) : 1);

      if (s != r)
        error ("mat2cell: mismatch on dimension %d (%" OCTAVE_IDX_TYPE_FORMAT
               " != %" OCTAVE_IDX_TYPE_FORMAT ")", i+1, r, s);
    }

  return false;
}

template <typename container>
static void
prepare_idx (container *idx, int idim, int nd,
             const Array<octave_idx_type> *d)
{
  octave_idx_type nidx = (idim < nd ? d[idim].numel () : 1);
  if (nidx == 1)
    idx[0] = idx_vector::colon;
  else
    {
      octave_idx_type l = 0;
      for (octave_idx_type i = 0; i < nidx; i++)
        {
          octave_idx_type u = l + d[idim](i);
          idx[i] = idx_vector (l, u);
          l = u;
        }
    }
}

// 2D specialization, works for Array, Sparse and octave_map.
// Uses 1D or 2D indexing.

template <typename Array2D>
static Cell
do_mat2cell_2d (const Array2D& a, const Array<octave_idx_type> *d, int nd)
{
  Cell retval;
  error_unless (nd == 1 || nd == 2);
  error_unless (a.ndims () == 2);

  if (mat2cell_mismatch (a.dims (), d, nd))
    return retval;

  octave_idx_type nridx = d[0].numel ();
  octave_idx_type ncidx = (nd == 1 ? 1 : d[1].numel ());
  retval.clear (nridx, ncidx);

  int ivec = -1;
  if (a.rows () > 1 && a.cols () == 1 && ncidx == 1)
    ivec = 0;
  else if (a.rows () == 1 && nridx == 1 && nd == 2)
    ivec = 1;

  if (ivec >= 0)
    {
      // Vector split.  Use 1D indexing.
      octave_idx_type l = 0;
      octave_idx_type nidx = (ivec == 0 ? nridx : ncidx);
      for (octave_idx_type i = 0; i < nidx; i++)
        {
          octave_idx_type u = l + d[ivec](i);
          retval.xelem (i) = a.index (idx_vector (l, u));
          l = u;
        }
    }
  else
    {
      // General 2D case.  Use 2D indexing.
      OCTAVE_LOCAL_BUFFER (idx_vector, ridx, nridx);
      prepare_idx (ridx, 0, nd, d);

      OCTAVE_LOCAL_BUFFER (idx_vector, cidx, ncidx);
      prepare_idx (cidx, 1, nd, d);

      for (octave_idx_type j = 0; j < ncidx; j++)
        for (octave_idx_type i = 0; i < nridx; i++)
          {
            octave_quit ();

            retval.xelem (i, j) = a.index (ridx[i], cidx[j]);
          }
    }

  return retval;
}

// Nd case.  Works for Arrays and octave_map.
// Uses Nd indexing.

template <typename ArrayND>
Cell
do_mat2cell_nd (const ArrayND& a, const Array<octave_idx_type> *d, int nd)
{
  Cell retval;
  error_unless (nd >= 1);

  if (mat2cell_mismatch (a.dims (), d, nd))
    return retval;

  dim_vector rdv = dim_vector::alloc (nd);
  OCTAVE_LOCAL_BUFFER (octave_idx_type, nidx, nd);
  octave_idx_type idxtot = 0;
  for (int i = 0; i < nd; i++)
    {
      rdv(i) = nidx[i] = d[i].numel ();
      idxtot += nidx[i];
    }

  retval.clear (rdv);

  OCTAVE_LOCAL_BUFFER (idx_vector, xidx, idxtot);
  OCTAVE_LOCAL_BUFFER (idx_vector *, idx, nd);

  idxtot = 0;
  for (int i = 0; i < nd; i++)
    {
      idx[i] = xidx + idxtot;
      prepare_idx (idx[i], i, nd, d);
      idxtot += nidx[i];
    }

  OCTAVE_LOCAL_BUFFER_INIT (octave_idx_type, ridx, nd, 0);
  Array<idx_vector> ra_idx
  (dim_vector (1, std::max (nd, a.ndims ())), idx_vector::colon);

  for (octave_idx_type j = 0; j < retval.numel (); j++)
    {
      octave_quit ();

      for (int i = 0; i < nd; i++)
        ra_idx.xelem (i) = idx[i][ridx[i]];

      retval.xelem (j) = a.index (ra_idx);

      rdv.increment_index (ridx);
    }

  return retval;
}

// Dispatcher.
template <typename ArrayND>
Cell
do_mat2cell (const ArrayND& a, const Array<octave_idx_type> *d, int nd)
{
  if (a.ndims () == 2 && nd <= 2)
    return do_mat2cell_2d (a, d, nd);
  else
    return do_mat2cell_nd (a, d, nd);
}

// General case.  Works for any class supporting do_index_op.
// Uses Nd indexing.

Cell
do_mat2cell (octave_value& a, const Array<octave_idx_type> *d, int nd)
{
  Cell retval;
  error_unless (nd >= 1);

  if (mat2cell_mismatch (a.dims (), d, nd))
    return retval;

  dim_vector rdv = dim_vector::alloc (nd);
  OCTAVE_LOCAL_BUFFER (octave_idx_type, nidx, nd);
  octave_idx_type idxtot = 0;
  for (int i = 0; i < nd; i++)
    {
      rdv(i) = nidx[i] = d[i].numel ();
      idxtot += nidx[i];
    }

  retval.clear (rdv);

  OCTAVE_LOCAL_BUFFER (octave_value, xidx, idxtot);
  OCTAVE_LOCAL_BUFFER (octave_value *, idx, nd);

  idxtot = 0;
  for (int i = 0; i < nd; i++)
    {
      idx[i] = xidx + idxtot;
      prepare_idx (idx[i], i, nd, d);
      idxtot += nidx[i];
    }

  OCTAVE_LOCAL_BUFFER_INIT (octave_idx_type, ridx, nd, 0);
  octave_value_list ra_idx (std::max (nd, a.ndims ()),
                            octave_value::magic_colon_t);

  for (octave_idx_type j = 0; j < retval.numel (); j++)
    {
      octave_quit ();

      for (int i = 0; i < nd; i++)
        ra_idx(i) = idx[i][ridx[i]];

      retval.xelem (j) = a.index_op (ra_idx);

      rdv.increment_index (ridx);
    }

  return retval;
}

DEFUN (mat2cell, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{C} =} mat2cell (@var{A}, @var{dim1}, @var{dim2}, @dots{}, @var{dimi}, @dots{}, @var{dimn})
@deftypefnx {} {@var{C} =} mat2cell (@var{A}, @var{rowdim})
Convert the matrix @var{A} to a cell array.

Each dimension argument (@var{dim1}, @var{dim2}, etc.@:) is a vector of
integers which specifies how to divide that dimension's elements amongst the
new elements in the output @var{C}.  The number of elements in the @var{i}-th
dimension is @code{size (@var{A}, @var{i})}.  Because all elements in @var{A}
must be partitioned, there is a requirement that @code{sum (@var{dimi}) == size
(@var{A}, i)}.  The size of the output cell @var{C} is numel (@var{dim1}) x
numel (@var{dim2}) x @dots{} x numel (@var{dimn}).

Given a single dimensional argument, @var{rowdim}, the output is divided into
rows as specified.  All other dimensions are not divided and thus all
columns (dim 2), pages (dim 3), etc.@: appear in each output element.

Examples

@example
x = reshape (1:12, [3, 4])'
@result{}
    1    2    3
    4    5    6
    7    8    9
   10   11   12

@group
## The 4 rows (dim1) are divided in to two cell elements
## with 2 rows each.
## The 3 cols (dim2) are divided in to three cell elements
## with 1 col each.
mat2cell (x, [2,2], [1,1,1])
@result{}
@{
  [1,1] =

     1
     4

  [2,1] =

      7
     10

  [1,2] =

     2
     5

  [2,2] =
      8
     11

  [1,3] =

     3
     6

  [2,3] =
      9
     12
@}
@end group

@group
## The 4 rows (dim1) are divided in to two cell elements
## with a 3/1 split.
## All columns appear in each output element.
mat2cell (x, [3,1])
@result{}
@{
  [1,1] =

     1   2   3
     4   5   6
     7   8   9

  [2,1] =

     10   11   12
@}
@end group
@end example

@seealso{num2cell, cell2mat}
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin < 2)
    print_usage ();

  octave_value retval;

  // Prepare indices.
  OCTAVE_LOCAL_BUFFER (Array<octave_idx_type>, d, nargin-1);

  for (int i = 1; i < nargin; i++)
    d[i-1] = args(i).octave_idx_type_vector_value (true);

  octave_value a = args(0);
  bool sparse = a.issparse ();
  if (sparse && nargin > 3)
    error ("mat2cell: sparse arguments only support 2-D indexing");

  switch (a.builtin_type ())
    {
    case btyp_double:
      {
        if (sparse)
          retval = do_mat2cell_2d (a.sparse_matrix_value (), d, nargin-1);
        else
          retval = do_mat2cell (a.array_value (), d, nargin - 1);
      }
      break;

    case btyp_complex:
      {
        if (sparse)
          retval = do_mat2cell_2d (a.sparse_complex_matrix_value (), d,
                                   nargin-1);
        else
          retval = do_mat2cell (a.complex_array_value (), d, nargin - 1);
      }
      break;

#define BTYP_BRANCH(X, Y)                                       \
      case btyp_ ## X:                                          \
        retval = do_mat2cell (a.Y ## _value (), d, nargin - 1); \
        break

      BTYP_BRANCH (float, float_array);
      BTYP_BRANCH (float_complex, float_complex_array);
      BTYP_BRANCH (bool, bool_array);
      BTYP_BRANCH (char, char_array);

      BTYP_BRANCH (int8,  int8_array);
      BTYP_BRANCH (int16, int16_array);
      BTYP_BRANCH (int32, int32_array);
      BTYP_BRANCH (int64, int64_array);
      BTYP_BRANCH (uint8,  uint8_array);
      BTYP_BRANCH (uint16, uint16_array);
      BTYP_BRANCH (uint32, uint32_array);
      BTYP_BRANCH (uint64, uint64_array);

      BTYP_BRANCH (cell, cell);
      BTYP_BRANCH (struct, map);

#undef BTYP_BRANCH

    case btyp_func_handle:
      err_wrong_type_arg ("mat2cell", a);
      break;

    default:
      retval = do_mat2cell (a, d, nargin-1);
      break;
    }

  return retval;
}

/*
%!test
%! x = reshape (1:20, 5, 4);
%! c = mat2cell (x, [3,2], [3,1]);
%! assert (c, {[1,6,11;2,7,12;3,8,13],[16;17;18];[4,9,14;5,10,15],[19;20]});

%!test
%! x = "abcdefghij";
%! c = mat2cell (x, 1, [0,4,2,0,4,0]);
%! empty1by0str = resize ("", 1, 0);
%! assert (c, {empty1by0str,"abcd","ef",empty1by0str,"ghij",empty1by0str});
*/

// FIXME: it would be nice to allow ranges being handled without a conversion.
template <typename NDA>
static Cell
do_cellslices_nda (const NDA& array,
                   const Array<octave_idx_type>& lb,
                   const Array<octave_idx_type>& ub,
                   int dim = -1)
{
  octave_idx_type n = lb.numel ();
  Cell retval (1, n);
  if (array.isvector () && (dim == -1
                            || (dim == 0 && array.columns () == 1)
                            || (dim == 1 && array.rows () == 1)))
    {
      for (octave_idx_type i = 0; i < n; i++)
        retval.xelem (i) = array.index (idx_vector (lb(i) - 1, ub(i)));
    }
  else
    {
      const dim_vector dv = array.dims ();
      int ndims = dv.ndims ();
      if (dim < 0)
        dim = dv.first_non_singleton ();
      ndims = std::max (ndims, dim + 1);

      Array<idx_vector> idx (dim_vector (ndims, 1), idx_vector::colon);

      for (octave_idx_type i = 0; i < n; i++)
        {
          idx(dim) = idx_vector (lb(i) - 1, ub(i));
          retval.xelem (i) = array.index (idx);
        }
    }

  return retval;
}

DEFUN (cellslices, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{sl} =} cellslices (@var{x}, @var{lb}, @var{ub}, @var{dim})
Given an array @var{x}, this function produces a cell array of slices from
the array determined by the index vectors @var{lb}, @var{ub}, for lower and
upper bounds, respectively.

In other words, it is equivalent to the following code:

@example
@group
n = length (lb);
sl = cell (1, n);
for i = 1:length (lb)
  sl@{i@} = x(:,@dots{},lb(i):ub(i),@dots{},:);
endfor
@end group
@end example

The position of the index is determined by @var{dim}.  If not specified,
slicing is done along the first non-singleton dimension.
@seealso{cell2mat, cellindexmat, cellfun}
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin < 3 || nargin > 4)
    print_usage ();

  octave_value x = args(0);
  Array<octave_idx_type> lb = args(1).octave_idx_type_vector_value ();
  Array<octave_idx_type> ub = args(2).octave_idx_type_vector_value ();
  int dim = -1;
  if (nargin == 4)
    {
      dim = args(3).int_value () - 1;
      if (dim < 0)
        error ("cellslices: DIM must be a valid dimension");
    }

  if (lb.numel () != ub.numel ())
    error ("cellslices: the lengths of LB and UB must match");

  Cell retcell;
  if (! x.issparse () && x.is_matrix_type ())
    {
      // specialize for some dense arrays.
      if (x.islogical ())
        retcell = do_cellslices_nda (x.bool_array_value (),
                                     lb, ub, dim);
      else if (x.is_char_matrix ())
        retcell = do_cellslices_nda (x.char_array_value (),
                                     lb, ub, dim);
      else if (x.isinteger ())
        {
          if (x.is_int8_type ())
            retcell = do_cellslices_nda (x.int8_array_value (),
                                         lb, ub, dim);
          else if (x.is_int16_type ())
            retcell = do_cellslices_nda (x.int16_array_value (),
                                         lb, ub, dim);
          else if (x.is_int32_type ())
            retcell = do_cellslices_nda (x.int32_array_value (),
                                         lb, ub, dim);
          else if (x.is_int64_type ())
            retcell = do_cellslices_nda (x.int64_array_value (),
                                         lb, ub, dim);
          else if (x.is_uint8_type ())
            retcell = do_cellslices_nda (x.uint8_array_value (),
                                         lb, ub, dim);
          else if (x.is_uint16_type ())
            retcell = do_cellslices_nda (x.uint16_array_value (),
                                         lb, ub, dim);
          else if (x.is_uint32_type ())
            retcell = do_cellslices_nda (x.uint32_array_value (),
                                         lb, ub, dim);
          else if (x.is_uint64_type ())
            retcell = do_cellslices_nda (x.uint64_array_value (),
                                         lb, ub, dim);
        }
      else if (x.iscomplex ())
        {
          if (x.is_single_type ())
            retcell = do_cellslices_nda (x.float_complex_array_value (),
                                         lb, ub, dim);
          else
            retcell = do_cellslices_nda (x.complex_array_value (),
                                         lb, ub, dim);
        }
      else
        {
          if (x.is_single_type ())
            retcell = do_cellslices_nda (x.float_array_value (),
                                         lb, ub, dim);
          else
            retcell = do_cellslices_nda (x.array_value (),
                                         lb, ub, dim);
        }
    }
  else
    {
      // generic code.
      octave_idx_type n = lb.numel ();
      retcell = Cell (1, n);
      const dim_vector dv = x.dims ();
      int ndims = dv.ndims ();
      if (dim < 0)
        dim = dv.first_non_singleton ();
      ndims = std::max (ndims, dim + 1);
      octave_value_list idx (ndims, octave_value::magic_colon_t);
      for (octave_idx_type i = 0; i < n; i++)
        {
          idx(dim) = range<double> (lb(i), ub(i));
          retcell.xelem (i) = x.index_op (idx);
        }
    }

  return ovl (retcell);
}

/*
%!test
%! m = [1, 2, 3, 4; 5, 6, 7, 8; 9, 10, 11, 12];
%! c = cellslices (m, [1, 2], [2, 3], 2);
%! assert (c, {[1, 2; 5, 6; 9, 10], [2, 3; 6, 7; 10, 11]});
*/

DEFUN (cellindexmat, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{y} =} cellindexmat (@var{x}, @var{varargin})
Perform indexing of matrices in a cell array.

Given a cell array of matrices @var{x}, this function computes

@example
@group
Y = cell (size (X));
for i = 1:numel (X)
  Y@{i@} = X@{i@}(varargin@{1@}, varargin@{2@}, @dots{}, varargin@{N@});
endfor
@end group
@end example

The indexing arguments may be scalar (@code{2}), arrays (@code{[1, 3]}),
ranges (@code{1:3}), or the colon operator (@qcode{":"}).  However, the
indexing keyword @code{end} is not available.
@seealso{cellslices, cellfun}
@end deftypefn */)
{
  if (args.length () == 0)
    print_usage ();

  const Cell x = args(0).xcell_value ("cellindexmat: X must be a cell");

  Cell y (x.dims ());
  octave_idx_type nel = x.numel ();
  octave_value_list idx = args.slice (1, args.length () - 1);

  for (octave_idx_type i = 0; i < nel; i++)
    {
      octave_quit ();

      octave_value tmp = x(i);

      y.xelem (i) = tmp.index_op (idx);
    }

  return octave_value (y);
}

OCTAVE_END_NAMESPACE(octave)
