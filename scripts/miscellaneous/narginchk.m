########################################################################
##
## Copyright (C) 2012-2023 The Octave Project Developers
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
## @deftypefn {} {} narginchk (@var{minargs}, @var{maxargs})
## Check for correct number of input arguments.
##
## Generate an error message if the number of arguments in the calling function
## is outside the range @var{minargs} and @var{maxargs}.  Otherwise, do
## nothing.
##
## Both @var{minargs} and @var{maxargs} must be scalar numeric values.  Zero,
## Inf, and negative values are all allowed, and @var{minargs} and
## @var{maxargs} may be equal.
##
## Note that this function evaluates @code{nargin} on the caller.
##
## @seealso{nargoutchk, error, nargout, nargin}
## @end deftypefn

function narginchk (minargs, maxargs)

  if (nargin != 2)
    print_usage ();
  elseif (! isnumeric (minargs) || ! isscalar (minargs))
    error ("narginchk: MINARGS must be a numeric scalar");
  elseif (! isnumeric (maxargs) || ! isscalar (maxargs))
    error ("narginchk: MAXARGS must be a numeric scalar");
  elseif (minargs > maxargs)
    error ("narginchk: MINARGS cannot be larger than MAXARGS");
  endif

  args = evalin ("caller", "nargin;");

  if (args < minargs)
    error ("narginchk: not enough input arguments");
  elseif (args > maxargs)
    error ("narginchk: too many input arguments");
  endif

endfunction


%!function f (nargs, varargin)
%! narginchk (nargs(1), nargs(2));
%!endfunction

%!error <too many input arguments> f([0,0])
%!error <not enough input arguments> f([3, 3], 1)

%!test
%! f([1,1]);
%!test
%! f([1,5], 2, 3, 4, 5);
