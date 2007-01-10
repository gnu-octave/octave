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
## @deftypefn {Function File} {} secd (@var{x})
## Compute the secant of an angle in degrees.
## @seealso{sec, cscd, sind, cosd}
## @end deftypefn

function y = secd (x)
  if (nargin != 1)
    print_usage ();
  endif
  y = 1 ./ cosd (x);
endfunction

%!error(secd())
%!error(secd(1,2))
%!assert(secd(0:10:80),sec(pi*[0:10:80]/180),-10*eps)
%!assert(secd([0,180,360]) != Inf)
%!assert(secd([90,270]) == Inf)
