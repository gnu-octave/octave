## Copyright (C) 1994-2012 John W. Eaton
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
## @deftypefn  {Mapping Function} {} lcm (@var{x}, @var{y})
## @deftypefnx {Mapping Function} {} lcm (@var{x}, @var{y}, @dots{})
## Compute the least common multiple of @var{x} and @var{y},
## or of the list of all arguments.  All elements must be the same size or
## scalar.
## @seealso{factor, gcd}
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Created: 16 September 1994
## Adapted-By: jwe

function l = lcm (varargin)

  if (nargin > 1)
    if (common_size (varargin{:}) != 0)
      error ("lcm: all args must be of the same size or scalar");
    elseif (! all (cellfun ("isnumeric", varargin)))
      error ("lcm: all arguments must be numeric");
    endif

    l = varargin{1};
    for i = 2:nargin
      x = varargin{i};
      msk = l == 0 & x == 0;
      l .*= x ./ gcd (l, x);
      l(msk) = 0;
    endfor
  else
    print_usage ();
  endif

endfunction

%!assert(lcm (3, 5, 7, 15) == 105);

%!error lcm ();

%!test
%! s.a = 1;
%! fail("lcm (s)");

