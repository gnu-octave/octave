## Copyright (C) 1996, 1997, 2006, 2007 John W. Eaton
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
## @deftypefn {Function File} {} intersection (@var{x}, @var{y})
## This function has been deprecated.  Use intersect instead.
## @end deftypefn

## Author: jwe

## Deprecated in version 3.0

function y = intersection (varargin)

  y = intersect (varargin{:});

endfunction

%!assert(all (all (intersection ([1, 2, 3], [2, 3, 5]) == [2, 3])));

%!assert(all (all (intersection ([1; 2; 3], [2, 3, 5]) == [2, 3])));

%!assert(isempty (intersection ([1, 2, 3], [4; 5; 6])));

%!error intersection (1);

%!error intersection (1, 2, 5);

