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
## @deftypefn {Function File} {} acosd (@var{x})
## Compute the inverse cosine of an angle in degrees.
## @seealso{acos, cosd, asecd}
## @end deftypefn

function y = acosd (x)
  if (nargin != 1)
    print_usage ();
  endif
  y = acos(x) .* 180 ./ pi;
endfunction

%!error(acosd())
%!error(acosd(1,2))
%!assert(acosd(0:0.1:1),180/pi*acos(0:0.1:1),-10*eps)
