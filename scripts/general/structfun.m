########################################################################
##
## Copyright (C) 2007-2023 The Octave Project Developers
##
## See the file COPYRIGHT.md in the top-level directory of this
## distribution or <https://octave.org/copyright/>.
##
## This file is part of Octave.
##
## Octave is free software: you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <https://www.gnu.org/licenses/>.
##
########################################################################

## -*- texinfo -*-
## @deftypefn  {} {@var{A} =} structfun (@var{fcn}, @var{S})
## @deftypefnx {} {@var{A} =} structfun (@dots{}, "ErrorHandler", @var{errfcn})
## @deftypefnx {} {@var{A} =} structfun (@dots{}, "UniformOutput", @var{val})
## @deftypefnx {} {[@var{A}, @var{B}, @dots{}] =} structfun (@dots{})
##
## Evaluate the function named @var{name} on the fields of the structure
## @var{S}.  The fields of @var{S} are passed to the function @var{fcn}
## individually.
##
## @code{structfun} accepts an arbitrary function @var{fcn} in the form of an
## inline function, function handle, or the name of a function (in a character
## string).  In the case of a character string argument, the function must
## accept a single argument named @var{x}, and it must return a string value.
## If the function returns more than one argument, they are returned as
## separate output variables.
##
## If the parameter @qcode{"UniformOutput"} is set to true (the default), then
## the function must return a single element which will be concatenated into
## the return value.  If @qcode{"UniformOutput"} is false, the outputs are
## placed into a structure with the same fieldnames as the input structure.
##
## @example
## @group
## s.name1 = "John Smith";
## s.name2 = "Jill Jones";
## structfun (@@(x) regexp (x, '(\w+)$', "matches")@{1@}, s,
##            "UniformOutput", false)
##   @result{} scalar structure containing the fields:
##        name1 = Smith
##        name2 = Jones
## @end group
## @end example
##
## Given the parameter @qcode{"ErrorHandler"}, @var{errfcn} defines a function
## to call in case @var{fcn} generates an error.  The form of the function is
##
## @example
## function [@dots{}] = errfcn (@var{se}, @dots{})
## @end example
##
## @noindent
## where there is an additional input argument to @var{errfcn} relative to
## @var{fcn}, given by @nospell{@var{se}}.  This is a structure with the
## elements @qcode{"identifier"}, @qcode{"message"} and @qcode{"index"},
## giving respectively the error identifier, the error message, and the index
## into the input arguments of the element that caused the error.  For an
## example on how to use an error handler, @pxref{XREFcellfun,,@code{cellfun}}.
##
## @seealso{cellfun, arrayfun, spfun}
## @end deftypefn

function varargout = structfun (fcn, S, varargin)

  if (nargin < 2)
    print_usage ();
  endif

  nargs = length (varargin);

  recognized_opts = {"UniformOutput", "ErrorHandler"};
  uo_str = recognized_opts{1};

  uniform_output = true;

  while (nargs >= 2)
    opt_match = strcmpi (varargin{nargs-1}, recognized_opts);
    if (opt_match(1))
      uniform_output = varargin{nargs};
    endif
    if (any (opt_match))
      nargs -= 2;
    else
      break;
    endif
  endwhile

  if (nargs > 0)
    error ("structfun: invalid options");
  endif

  varargout = cell (max ([nargout, 1]), 1);
  [varargout{:}] = cellfun (fcn, struct2cell (S), varargin{:});

  if (! uniform_output)
    varargout = cellfun ("cell2struct", varargout, {fieldnames(S)}, {1}, ...
                         uo_str, false);
  endif

endfunction


%!test
%! s.name1 = "John Smith";
%! s.name2 = "Jill Jones";
%! l.name1 = "Smith";
%! l.name2 = "Jones";
%! o = structfun (@(x) regexp (x, '(\w+)$', "matches"){1}, s,
%!                "UniformOutput", false);
%! assert (o, l);

%!function [a, b] = __twoouts (x)
%!  a = x + x;
%!  b = x * x;
%!endfunction

%!test
%! s = struct ("a", {1, 2, 3}, "b", {4, 5, 6});
%! c(1:2, 1, 1) = [2; 8];
%! c(1:2, 1, 2) = [4; 10];
%! c(1:2, 1, 3) = [6; 12];
%! d(1:2, 1, 1) = [1; 16];
%! d(1:2, 1, 2) = [4; 25];
%! d(1:2, 1, 3) = [9; 36];
%! [aa, bb] = structfun (@__twoouts, s);
%! assert (aa, c);
%! assert (bb, d);

%!test
%! s = struct ("a", {1, 2, 3}, "b", {4, 5, 6});
%! c = struct ("a", {2, 4, 6}, "b", {8, 10, 12});
%! d = struct ("a", {1, 4, 9}, "b", {16, 25, 36});
%! [aa, bb] = structfun (@__twoouts, s, "UniformOutput", false);
%! assert (aa, c);
%! assert (bb, d);
