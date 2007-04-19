## Copyright (C) 1996, 1997 John W. Eaton
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2, or (at your option)
## any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, write to the Free
## Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301, USA.

## -*- texinfo -*-
## @deftypefn {Mapping Function} {} lcm (@var{x}, @dots{})
## Compute the least common multiple of the elements elements of @var{x}, or
## the list of all the arguments.  For example,
##
## @example
## lcm (a1, ..., ak)
## @end example
##
## @noindent
## is the same as
##
## @example
## lcm ([a1, ..., ak]).
## @end example
##
## All elements must be the same size or scalar.
## @seealso{gcd, min, max, ceil, floor}
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

    for i=2:nargin
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
