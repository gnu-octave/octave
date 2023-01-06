########################################################################
##
## Copyright (C) 1994-2023 The Octave Project Developers
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
## @deftypefn  {} {@var{l} =} lcm (@var{x}, @var{y})
## @deftypefnx {} {@var{l} =} lcm (@var{x}, @var{y}, @dots{})
## Compute the least common multiple of @var{x} and @var{y}, or of the list of
## all arguments.
##
## All elements must be numeric and of the same size or scalar.
## @seealso{factor, gcd, isprime}
## @end deftypefn

function l = lcm (varargin)

  if (nargin < 2)
    print_usage ();
  endif

  if (common_size (varargin{:}) != 0)
    error ("lcm: all args must be the same size or scalar");
  elseif (! all (cellfun ("isnumeric", varargin)))
    error ("lcm: all arguments must be numeric");
  endif

  l = varargin{1};
  for i = 2:nargin
    x = varargin{i};
    msk = (l == 0 & x == 0);
    l .*= x ./ gcd (l, x);
    l(msk) = 0;
  endfor

  if (isfloat (l) && l > flintmax (l))
    warning ("Octave:lcm:large-output-float", ...
             "lcm: possible loss of precision");
  elseif (isinteger (l) && l == intmax (l))
    warning ("Octave:lcm:large-output-integer", ...
             "lcm: result may have saturated at intmax");
  endif

endfunction


%!assert (lcm (3, 5, 7, 15), 105)

## Test input validation
%!error <Invalid call> lcm ()
%!error <Invalid call> lcm (1)
%!error <same size or scalar> lcm ([1 2], [1 2 3])
%!error <arguments must be numeric> lcm ([1 2], {1 2})
%!warning <loss of precision>   lcm (num2cell (double (1:47)){:});
%!warning <loss of precision>   lcm (num2cell (single (1:47)){:});
%!warning <result .* saturated> lcm (num2cell (uint64 (1:47)){:});
%!warning <result .* saturated> lcm (num2cell (uint32 (1:47)){:});
%!warning <result .* saturated> lcm (num2cell (uint16 (1:47)){:});
%!warning <result .* saturated> lcm (num2cell ( uint8 (1:47)){:});
%!warning <result .* saturated> lcm (num2cell ( int64 (1:47)){:});
%!warning <result .* saturated> lcm (num2cell ( int32 (1:47)){:});
%!warning <result .* saturated> lcm (num2cell ( int16 (1:47)){:});
%!warning <result .* saturated> lcm (num2cell (  int8 (1:47)){:});
