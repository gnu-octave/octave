## Copyright (C) 2004 Paul Kienzle
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
##
## Original version by Paul Kienzle distributed as free software in the
## public domain.

## -*- texinfo -*-
## @deftypefn {Function File} {} nthroot (@var{x}, @var{n})
## 
## Compute the nth root of @var{x}, returning real results for real 
## components of @var{x}. For example
##
## @example
## @group
## nthroot (-1, 3)
## @result{} -1
## (-1) ^ (1 / 3)
## @result{} 0.50000 - 0.86603i
## @end group
## @end example
##
## @end deftypefn

function y = nthroot (x, m)

  if (nargin != 2)
    print_usage ();
  endif
  
  y = x.^(1./m);

  if (isscalar (x))
    x *= ones (size (m)); 
  endif

  if (isscalar (m))
    m *= ones (size (x)); 
  endif

  idx = (mod (m, 2) == 1 & imag (x) == 0 & x < 0);

  if (any (idx(:)))
    y(idx) = -(-x(idx)).^(1./m(idx)); 
  endif

  ## If result is all real, make sure it looks real
  if (all (imag (y) == 0))
    y = real (y); 
  endif

endfunction

%!assert(nthroot(-1,[3,-3]), [-1,-1],eps);
%!assert(nthroot([-1,1],[3.1,-3]), [-1,1].^(1./[3.1,-3]));
%!assert(nthroot([-1+1i,-1-1i],3), [-1+1i,-1-1i].^(1/3));
