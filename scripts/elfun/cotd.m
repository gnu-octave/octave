## Copyright (C) 2006 David Bateman <dbateman@free.fr>
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
## @deftypefn {Function File} {} cotd (@var{x})
## Compute the cotangent of an angle in degrees.
## @seealso{cot, tand}
## @end deftypefn

function y = cotd (x)
  if (nargin != 1)
    print_usage ();
  endif
  y = 1 ./ tand (x);
endfunction

%!error(cotd())
%!error(cotd(1,2))
%!assert(cotd(10:10:80),cot(pi*[10:10:80]/180),-10*eps)
%!assert(cotd([0,180,360]) == Inf)
%!assert(cotd([90,270]) == 0)
