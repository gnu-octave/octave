## Copyright (C) 1994, 1995, 1996, 1997, 1999, 2000, 2002, 2004, 2005,
##               2006, 2007, 2008, 2009 John W. Eaton
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
## @deftypefn  {Mapping Function} {} lcm (@var{x})
## @deftypefnx {Mapping Function} {} lcm (@var{x}, @dots{})
## Compute the least common multiple of the elements of @var{x}, or
## of the list of all arguments.  For example,
##
## @example
## lcm (a1, @dots{}, ak)
## @end example
##
## @noindent
## is the same as
##
## @example
## lcm ([a1, @dots{}, ak]).
## @end example
##
## All elements must be the same size or scalar.
## @seealso{factor, gcd}
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Created: 16 September 1994
## Adapted-By: jwe

function l = lcm (varargin)

  if (nargin == 0)
    print_usage ();
  endif

  if (nargin == 1)
    a = varargin{1};

    if (round (a) != a)
      error ("lcm: all arguments must be integer");
    endif

    if (any (a) == 0)
      l = 0;
    else
      a = abs (a);
      l = a (1);
      for k = 1:(length (a) - 1)
        l = l * a(k+1) / gcd (l, a(k+1));
      endfor
    endif
  else
    
    l = varargin{1};
    sz = size (l);
    nel = numel (l);

    for i = 2:nargin
      a = varargin{i};

      if (size (a) != sz)
        if (nel == 1)
          sz = size (a);
          nel = numel (a);
        elseif (numel (a) != 1)
          error ("lcm: all arguments must be the same size or scalar");
        endif
      endif

      if (round (a) != a)
        error ("lcm: all arguments must be integer");
      endif

      idx = find (l == 0 || a == 0);
      a = abs (a);
      l = l .* a ./ gcd (l, a);
      l(idx) = 0;
    endfor
  endif

endfunction

%!assert(lcm (3, 5, 7, 15) == lcm ([3, 5, 7, 15]) && lcm ([3, 5, 7,15]) == 105);

%!error lcm ();

%!test
%! s.a = 1;
%! fail("lcm (s)");

