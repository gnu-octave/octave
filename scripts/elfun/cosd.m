## Copyright (C) 2006 David Bateman <dbateman@free.fr>
##
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2 of the License, or
## (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program; if not, write to the Free Software
## Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

## -*- texinfo -*-
## @deftypefn {Function File} {} cosd (@var{x})
## Compute the cosine of an angle in degrees.  Returns zero in elements
## for which @code{(@var{x}-90)/180} is an integer.
## @seealso{cos, sind, tand, acosd, asind, atand}
## @end deftypefn

function y = cosd (x)
  if (nargin != 1)
    print_usage ();
  endif
  I = x / 180;
  y = cos (I .* pi);
  I = I + 0.5;
  y(I == round (I) & finite (I)) = 0;
endfunction

%!error(cosd())
%!error(cosd(1,2))
%!assert(cosd(0:10:80),cos(pi*[0:10:80]/180),-10*eps)
%!assert(cosd([0,180,360]) != 0)
%!assert(cosd([90,270]) == 0)
