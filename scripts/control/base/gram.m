## Copyright (C) 1996, 2000, 2003, 2004, 2005, 2007
##               Auburn University. All rights reserved.
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
## @deftypefn {Function File} {} gram (@var{a}, @var{b})
## Return controllability gramian @var{m} of the continuous time system
## @math{dx/dt = a x + b u}.
##
## @var{m} satisfies @math{a m + m a' + b b' = 0}.
## @end deftypefn

## Author: A. S. Hodel <a.s.hodel@eng.auburn.edu>

function m = gram (a, b)

  ## Let lyap do the error checking...

  m = lyap (a, b*b');

endfunction
