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
## Software Foundation, 59 Temple Place - Suite 330, Boston, MA
## 02111-1307, USA.

## -*- texinfo -*-
## @deftypefn {Mapping Function} {} lcm (@var{x}, @code{...})
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
## @end deftypefn

## See also: gcd, min, max, ceil, floor.  

## Author: KH <Kurt.Hornik@ci.tuwien.ac.at>
## Created: 16 September 1994
## Adapted-By: jwe

function l = lcm (a, ...)

  if (nargin == 0)
    usage ("lcm (a, ...)");
  endif

  if (nargin > 1)
    va_start;
    for k = 2:nargin;
      a = [a, (va_arg ())];
    endfor
  endif

  if (round (a) != a)
    error ("lcm:  all arguments must be integer");
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

endfunction
