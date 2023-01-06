########################################################################
##
## Copyright (C) 2004-2023 The Octave Project Developers
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
##
## Original version by Paul Kienzle distributed as free software in the
## public domain.

## -*- texinfo -*-
## @deftypefn  {} {@var{namestr} =} inputname (@var{n})
## @deftypefnx {} {@var{namestr} =} inputname (@var{n}, @var{ids_only})
## Return the name of the @var{n}-th argument to the calling function.
##
## If the argument is not a simple variable name, return an empty string.
## Examples which will return @qcode{""} are numbers (@code{5.1}),
## expressions (@code{@var{y}/2}), and cell or structure indexing
## (@code{@var{c}@{1@}} or @code{@var{s}.@var{field}}).
##
## @code{inputname} is only useful within a function.  When used at the command
## line or within a script it always returns an empty string.
##
## By default, return an empty string if the @var{n}-th argument is not a valid
## variable name.  If the optional argument @var{ids_only} is false, return the
## text of the argument even if it is not a valid variable name.  This is an
## Octave extension that allows the programmer to view exactly how the function
## was invoked even when the inputs are complex expressions.
## @seealso{nargin, narginchk}
## @end deftypefn

## FIXME: Actually, it probably *isn't* worth fixing, but there are small
## differences between Octave and Matlab.
##
## 1) When called from the top-level or a script, Matlab throws an error
##
##   inputname (1)   % at command prompt
##   % Octave returns "", Matlab throws an error
##
## 2) cell or struct indexing causes all further names to be returned as ""
##
##   c = {'a', 'b'}
##   y = 1; z = 2;
##   fcn (c, y, z)
##   % inputname() would return 'c', 'y', 'z' for the inputs.
##   fcn (c{1}, y, z)
##   % inputname() would return '', '', '' for the inputs.
##
## 3) If inputname is not called from a function, Matlab walks up the stack
##    until it finds some valid code and then works from there.  This could
##    be relevant for mex files or anonymous functions.
##
##   f = @(x) inputname (x);
##   a = 1:4;
##   arrayfun (fn, a, 'uniformoutput', false)
##   % output is {'fn', 'a', '', ''}

function namestr = inputname (n, ids_only = true)

  if (nargin < 1)
    print_usage ();
  endif

  if (! isscalar (n) || ! isindex (n))
    error ("inputname: N must be a scalar index");
  endif

  try
    namestr = evalin ("caller", sprintf ("__varval__ ('.argn.'){%d}", n));
  catch
    namestr = "";
    return;
  end_try_catch

  ## For compatibility with Matlab, return empty string if argument name is
  ## not a valid identifier.
  if (ids_only && ! isvarname (namestr))
    namestr = "";
  elseif (ids_only)
    ## More complicated checking is required to verify name (bug #59103).
    ## NAME may be text, like "Inf", which is an acceptable variable name
    ## that passes isvarname(), but that does not mean it is an actual
    ## variable name, rather than a function or IEEE number.
    try
      v = evalin ("caller",
                  sprintf ("evalin ('caller', '__varval__ (\"%s\")')", namestr));
    catch
      namestr = "";
    end_try_catch
  endif

endfunction


%!function name = __iname1__ (arg1, arg2, arg3)
%!  name = inputname (1);
%!endfunction

%!function name = __iname1_ID__ (arg1, arg2, arg3)
%!  name = inputname (1, false);
%!endfunction

%!function name = __iname2__ (arg1, arg2, arg3)
%!  name = inputname (2);
%!endfunction

%!function names = __iname3__ (arg1, arg2, arg3)
%!  names = cell (1, 3);
%!  for i = 1:3
%!    names{i} = inputname (i);
%!  endfor
%!endfunction

%!test
%! assert (__iname1__ ('xvar'), "");
%! xvar = 1;
%! assert (__iname1__ (xvar), "xvar");

%!test
%! xvar = 1;  yvar = 2;
%! assert (__iname2__ (xvar), "");
%! assert (__iname2__ (xvar, yvar), "yvar");

%!test
%! xvar = 1;  yvar = 2;
%! assert (__iname3__ (xvar), {"xvar", "", ""});
%! assert (__iname3__ (xvar, yvar), {"xvar", "yvar", ""});
%! assert (__iname3__ (xvar, 3, yvar), {"xvar", "", "yvar"});

## Test numbers, expressions, indexing operations
%!test
%! assert (__iname1__ (1.0), "");
%! x = 1;
%! assert (__iname1__ (x / 2), "");
%! assert (__iname1__ (Inf), "");

%!test
%! assert (__iname1_ID__ (1.0), "1.0");
%! x = 1;
%! assert (__iname1_ID__ (x / 2), "x / 2");
%! assert (__iname1_ID__ (Inf), "Inf");

%!error <Invalid call> inputname ()
%!error <N must be a scalar> inputname (ones (2,2))
%!error <N must be a scalar index> inputname (-1)
