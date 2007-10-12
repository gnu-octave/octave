## Copyright (C) 1998, 2000, 2002, 2005, 2007
##               Auburn University.  All rights reserved.
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
## @deftypefn {Function File} {} qtrans (@var{v}, @var{q})
## Transform the unit quaternion @var{v} by the unit quaternion @var{q}.
## Returns @code{@var{v} = @var{q}*@var{v}/@var{q}}.
## @end deftypefn

## Author: A. S. Hodel <a.s.hodel@eng.auburn.edu>
## Adapted-By: jwe

function v = qtrans (v, q)

  if (! isvector (v) || length (v) != 4)
    error ("qtrans: v(%d,%d) must be a quaternion", rows (v), columns (v));
  elseif (! isvector (q) || length (q) != 4)
    error ("qtrans: q(%d,%d) must be a quaternion", rows (q), columns (q));
  endif

  v = qmult (q, qmult (v, qinv (q)));

endfunction
