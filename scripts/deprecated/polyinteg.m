## Copyright (C) 1996, 1997, 2007 John W. Eaton
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
## @deftypefn {Function File} {} polyinteg (@var{c})
## Return the coefficients of the integral of the polynomial whose
## coefficients are represented by the vector @var{c}.
##
## The constant of integration is set to zero.
## @seealso{poly, polyderiv, polyreduce, roots, conv, deconv, residue,
## filter, polyval, and polyvalm}
## @end deftypefn

## Author: Tony Richardson <arichard@stark.cc.oh.us>
## Created: June 1994
## Adapted-By: jwe

## Deprecated in version 3.0

function y = polyinteg (p)

  y = polyint (p);

endfunction

%!assert(all (all (polyinteg ([2, 2]) == [1, 2, 0])));

%!assert(isempty (polyinteg ([])));

%!assert(all (all (polyinteg (3) == [3, 0])));

%!error polyinteg ([1, 2; 3, 4]);

