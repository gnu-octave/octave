## Copyright (C) 2006-2011 Bill Denney
## Copyright (C) 2009 Jaroslav Hajek
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or (at
## your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn  {Function File} {} arrayfun (@var{func}, @var{A})
## @deftypefnx {Function File} {@var{x} =} arrayfun (@var{func}, @var{A})
## @deftypefnx {Function File} {@var{x} =} arrayfun (@var{func}, @var{A}, @var{b}, @dots{})
## @deftypefnx {Function File} {[@var{x}, @var{y}, @dots{}] =} arrayfun (@var{func}, @var{A}, @dots{})
## @deftypefnx {Function File} {} arrayfun (@dots{}, "UniformOutput", @var{val})
## @deftypefnx {Function File} {} arrayfun (@dots{}, "ErrorHandler", @var{errfunc})
##
## Execute a function on each element of an array.  This is useful for
## functions that do not accept array arguments.  If the function does
## accept array arguments it is better to call the function directly.
##
## The first input argument @var{func} can be a string, a function
## handle, an inline function or an anonymous function.  The input
## argument @var{A} can be a logic array, a numeric array, a string
## array, a structure array or a cell array.  By a call of the function
## @command{arrayfun} all elements of @var{A} are passed on to the named
## function @var{func} individually.
##
## The named function can also take more than two input arguments, with
## the input arguments given as third input argument @var{b}, fourth
## input argument @var{c}, @dots{}  If given more than one array input
## argument then all input arguments must have the same sizes, for
## example:
##
## @example
## @group
## arrayfun (@@atan2, [1, 0], [0, 1])
## @result{} ans = [1.5708   0.0000]
## @end group
## @end example
##
## If the parameter @var{val} after a further string input argument
## "UniformOutput" is set @code{true} (the default), then the named
## function @var{func} must return a single element which then will be
## concatenated into the return value and is of type matrix.  Otherwise,
## if that parameter is set to @code{false}, then the outputs are
## concatenated in a cell array.  For example:
##
## @example
## @group
## arrayfun (@@(x,y) x:y, "abc", "def", "UniformOutput", false)
## @result{} ans =
## @{
##   [1,1] = abcd
##   [1,2] = bcde
##   [1,3] = cdef
## @}
## @end group
## @end example
##
## If more than one output arguments are given then the named function
## must return the number of return values that also are expected, for
## example:
##
## @example
## @group
## [A, B, C] = arrayfun (@@find, [10; 0], "UniformOutput", false)
## @result{}
## A =
## @{
##   [1,1] =  1
##   [2,1] = [](0x0)
## @}
## B =
## @{
##   [1,1] =  1
##   [2,1] = [](0x0)
## @}
## C =
## @{
##   [1,1] =  10
##   [2,1] = [](0x0)
## @}
## @end group
## @end example
##
## If the parameter @var{errfunc} after a further string input argument
## "ErrorHandler" is another string, a function handle, an inline
## function or an anonymous function, then @var{errfunc} defines a
## function to call in the case that @var{func} generates an error.
## The definition of the function must be of the form
##
## @example
## function [@dots{}] = errfunc (@var{s}, @dots{})
## @end example
##
## @noindent
## where there is an additional input argument to @var{errfunc}
## relative to @var{func}, given by @var{s}.  This is a structure with
## the elements "identifier", "message" and "index", giving
## respectively the error identifier, the error message and the index of
## the array elements that caused the error.  The size of the output
## argument of @var{errfunc} must have the same size as the output
## argument of @var{func}, otherwise a real error is thrown.  For
## example:
##
## @example
## @group
## function y = ferr (s, x), y = "MyString"; endfunction
## arrayfun (@@str2num, [1234], \
##           "UniformOutput", false, "ErrorHandler", @@ferr)
## @result{} ans =
## @{
##  [1,1] = MyString
## @}
## @end group
## @end example
##
## @seealso{spfun, cellfun, structfun}
## @end deftypefn

## Author: Bill Denney <denney@seas.upenn.edu>
## Rewritten: Jaroslav Hajek <highegg@gmail.com>

function varargout = arrayfun (func, varargin)

  if (nargin < 2)
    print_usage ();
  endif

  nargs = length (varargin);

  recognized_opts = {"UniformOutput", "ErrorHandler"};

  while (nargs >= 2)
    maybeopt = varargin{nargs-1};
    if (ischar (maybeopt) && any (strcmpi (maybeopt, recognized_opts)))
      nargs -= 2;
    else
      break;
    endif
  endwhile

  args = varargin(1:nargs);
  opts = varargin(nargs+1:end);

  args = cellfun (@num2cell, args, "UniformOutput", false,
  "ErrorHandler",  @arg_class_error);

  [varargout{1:max(1, nargout)}] = cellfun (func, args{:}, opts{:});

endfunction

function arg_class_error (S, X)
  error ("arrayfun: invalid argument of class %s", class (X));
endfunction

%% Test function to check the "Errorhandler" option
%!function [z] = arrayfunerror (S, varargin)
%!      z = S;
%!    endfunction
%% First input argument can be a string, an inline function, a
%% function_handle or an anonymous function
%!test
%!  arrayfun (@isequal, [false, true], [true, true]); %% No output argument
%!error
%!  arrayfun (@isequal); %% One or less input arguments
%!test
%!  A = arrayfun ("isequal", [false, true], [true, true]);
%!  assert (A, [false, true]);
%!test
%!  A = arrayfun (inline ("(x == y)", "x", "y"), [false, true], [true, true]);
%!  assert (A, [false, true]);
%!test
%!  A = arrayfun (@isequal, [false, true], [true, true]);
%!  assert (A, [false, true]);
%!test
%!  A = arrayfun (@(x,y) isequal(x,y), [false, true], [true, true]);
%!  assert (A, [false, true]);

%% Number of input and output arguments may be greater than one
%#!test
%!  A = arrayfun (@(x) islogical (x), false);
%!  assert (A, true);
%!test
%!  A = arrayfun (@(x,y,z) x + y + z, [1, 1, 1], [2, 2, 2], [3, 4, 5]);
%!  assert (A, [6, 7, 8], 1e-16);
%!test %% Two input arguments of different types
%!  A = arrayfun (@(x,y) islogical (x) && ischar (y), false, "a");
%!  assert (A, true);
%!test %% Pass another variable to the anonymous function
%!  y = true; A = arrayfun (@(x) islogical (x && y), false);
%!  assert (A, true);
%!test %% Three ouptut arguments of different type
%!  [A, B, C] = arrayfun (@find, [10, 11; 0, 12], "UniformOutput", false);
%!  assert (isequal (A, {true, true; [], true}));
%!  assert (isequal (B, {true, true; [], true}));
%!  assert (isequal (C, {10, 11; [], 12}));

%% Input arguments can be of type logical
%!test
%!  A = arrayfun (@(x,y) x == y, [false, true], [true, true]);
%!  assert (A, [false, true]);
%!test
%!  A = arrayfun (@(x,y) x == y, [false; true], [true; true], "UniformOutput", true);
%!  assert (A, [false; true]);
%!test
%!  A = arrayfun (@(x) x, [false, true, false, true], "UniformOutput", false);
%!  assert (A, {false, true, false, true});
%!test %% Three ouptut arguments of same type
%!  [A, B, C] = arrayfun (@find, [true, false; false, true], "UniformOutput", false);
%!  assert (isequal (A, {true, []; [], true}));
%!  assert (isequal (B, {true, []; [], true}));
%!  assert (isequal (C, {true, []; [], true}));
%!test
%!  A = arrayfun (@(x,y) array2str (x,y), true, true, "ErrorHandler", @arrayfunerror);
%!  assert (isfield (A, "identifier"), true);
%!  assert (isfield (A, "message"), true);
%!  assert (isfield (A, "index"), true);
%!  assert (isempty (A.message), false);
%!  assert (A.index, 1);
%!test %% Overwriting setting of "UniformOutput" true
%!  A = arrayfun (@(x,y) array2str (x,y), true, true, \
%!                "UniformOutput", true, "ErrorHandler", @arrayfunerror);
%!  assert (isfield (A, "identifier"), true);
%!  assert (isfield (A, "message"), true);
%!  assert (isfield (A, "index"), true);
%!  assert (isempty (A.message), false);
%!  assert (A.index, 1);

%% Input arguments can be of type numeric
%!test
%!  A = arrayfun (@(x,y) x>y, [1.1, 4.2], [3.1, 2+3*i]);
%!  assert (A, [false, true]);
%!test
%!  A = arrayfun (@(x,y) x>y, [1.1, 4.2; 2, 4], [3.1, 2; 2, 4+2*i], "UniformOutput", true);
%!  assert (A, [false, true; false, false]);
%!test
%!  A = arrayfun (@(x,y) x:y, [1.1, 4], [3.1, 6], "UniformOutput", false);
%!  assert (isequal (A{1}, [1.1, 2.1, 3.1]));
%!  assert (isequal (A{2}, [4, 5, 6]));
%!test %% Three ouptut arguments of different type
%!  [A, B, C] = arrayfun (@find, [10, 11; 0, 12], "UniformOutput", false);
%!  assert (isequal (A, {true, true; [], true}));
%!  assert (isequal (B, {true, true; [], true}));
%!  assert (isequal (C, {10, 11; [], 12}));
%!test
%!  A = arrayfun (@(x,y) array2str(x,y), {1.1, 4}, {3.1, 6}, "ErrorHandler", @arrayfunerror);
%!  B = isfield (A(1), "message") && isfield (A(1), "index");
%!  assert ([(isfield (A(1), "identifier")), (isfield (A(2), "identifier"))], [true, true]);
%!  assert ([(isfield (A(1), "message")), (isfield (A(2), "message"))], [true, true]);
%!  assert ([(isfield (A(1), "index")), (isfield (A(2), "index"))], [true, true]);
%!  assert ([(isempty (A(1).message)), (isempty (A(2).message))], [false, false]);
%!  assert ([A(1).index, A(2).index], [1, 2]);
%!test %% Overwriting setting of "UniformOutput" true
%!  A = arrayfun (@(x,y) array2str(x,y), {1.1, 4}, {3.1, 6}, \
%!                "UniformOutput", true, "ErrorHandler", @arrayfunerror);
%!  B = isfield (A(1), "message") && isfield (A(1), "index");
%!  assert ([(isfield (A(1), "identifier")), (isfield (A(2), "identifier"))], [true, true]);
%!  assert ([(isfield (A(1), "message")), (isfield (A(2), "message"))], [true, true]);
%!  assert ([(isfield (A(1), "index")), (isfield (A(2), "index"))], [true, true]);
%!  assert ([(isempty (A(1).message)), (isempty (A(2).message))], [false, false]);
%!  assert ([A(1).index, A(2).index], [1, 2]);

%% Input arguments can be of type character or strings
%!test
%!  A = arrayfun (@(x,y) x>y, ["ad", "c", "ghi"], ["cc", "d", "fgh"]);
%!  assert (A, [false, true, false, true, true, true]);
%!test
%!  A = arrayfun (@(x,y) x>y, ["a"; "f"], ["c"; "d"], "UniformOutput", true);
%!  assert (A, [false; true]);
%!test
%!  A = arrayfun (@(x,y) x:y, ["a", "d"], ["c", "f"], "UniformOutput", false);
%!  assert (A, {"abc", "def"});
%! %#!test
%!   A = arrayfun (@(x,y) cell2str(x,y), ["a", "d"], ["c", "f"], "ErrorHandler", @arrayfunerror);
%!   B = isfield (A(1), "identifier") && isfield (A(1), "message") && isfield (A(1), "index");
%!   assert (B, true);

%% Input arguments can be of type structure
%!test
%!  a = struct ("a", 1.1, "b", 4.2); b = struct ("a", 3.1, "b", 2);
%!  A = arrayfun (@(x,y) (x.a < y.a) && (x.b > y.b), a, b);
%!  assert (A, true);
%!test
%!  a = struct ("a", 1.1, "b", 4.2); b = struct ("a", 3.1, "b", 2);
%!  A = arrayfun (@(x,y) (x.a < y.a) && (x.b > y.b), a, b, "UniformOutput", true);
%!  assert (A, true);
%!test
%!  a = struct ("a", 1.1, "b", 4.2); b = struct ("a", 3.1, "b", 2);
%!  A = arrayfun (@(x,y) x.a:y.a, a, b, "UniformOutput", false);
%!  assert (isequal (A, {[1.1, 2.1, 3.1]}));
%!test
%!  A = arrayfun (@(x) mat2str(x), "a", "ErrorHandler", @arrayfunerror);
%!  assert (isfield (A, "identifier"), true);
%!  assert (isfield (A, "message"), true);
%!  assert (isfield (A, "index"), true);
%!  assert (isempty (A.message), false);
%!  assert (A.index, 1);
%!test %% Overwriting setting of "UniformOutput" true
%!  A = arrayfun (@(x) mat2str(x), "a", "UniformOutput", true, \
%!                "ErrorHandler", @arrayfunerror);
%!  assert (isfield (A, "identifier"), true);
%!  assert (isfield (A, "message"), true);
%!  assert (isfield (A, "index"), true);
%!  assert (isempty (A.message), false);
%!  assert (A.index, 1);

%% Input arguments can be of type cell array
%!test
%!  A = arrayfun (@(x,y) x{1} < y{1}, {1.1, 4.2}, {3.1, 2});
%!  assert (A, [true, false]);
%!test
%!  A = arrayfun (@(x,y) x{1} < y{1}, {1.1; 4.2}, {3.1; 2}, "UniformOutput", true);
%!  assert (A, [true; false]);
%!test
%!  A = arrayfun (@(x,y) x{1} < y{1}, {1.1, 4.2}, {3.1, 2}, "UniformOutput", false);
%!  assert (A, {true, false});
%!test
%!  A = arrayfun (@(x,y) num2str(x,y), {1.1, 4.2}, {3.1, 2}, "ErrorHandler", @arrayfunerror);
%!  assert ([(isfield (A(1), "identifier")), (isfield (A(2), "identifier"))], [true, true]);
%!  assert ([(isfield (A(1), "message")), (isfield (A(2), "message"))], [true, true]);
%!  assert ([(isfield (A(1), "index")), (isfield (A(2), "index"))], [true, true]);
%!  assert ([(isempty (A(1).message)), (isempty (A(2).message))], [false, false]);
%!  assert ([A(1).index, A(2).index], [1, 2]);
%!test
%!  A = arrayfun (@(x,y) num2str(x,y), {1.1, 4.2}, {3.1, 2}, \
%!                "UniformOutput", true, "ErrorHandler", @arrayfunerror);
%!  assert ([(isfield (A(1), "identifier")), (isfield (A(2), "identifier"))], [true, true]);
%!  assert ([(isfield (A(1), "message")), (isfield (A(2), "message"))], [true, true]);
%!  assert ([(isfield (A(1), "index")), (isfield (A(2), "index"))], [true, true]);
%!  assert ([(isempty (A(1).message)), (isempty (A(2).message))], [false, false]);
%!  assert ([A(1).index, A(2).index], [1, 2]);

## Local Variables: ***
## mode: octave ***
## End: ***
