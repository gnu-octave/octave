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
## @deftypefn {Function File} {} asecd (@var{x})
## Compute inverse secant in degrees.
## @seealso{asec, secd, acscd}
## @end deftypefn

function y = asecd (x)
  if (nargin != 1)
    print_usage ();
  endif
  y = asec (x) .* 180 ./ pi;
endfunction;

%!error(asecd())
%!error(asecd(1,2))
%!assert(asecd(0:10:90),180./pi.*asec(0:10:90),-10*eps)
