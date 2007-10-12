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
## @deftypefn {Function File} {} dgram (@var{a}, @var{b})
## Return controllability gramian of discrete time system
## @iftex
## @tex
## $$ x_{k+1} = ax_k + bu_k $$
## @end tex
## @end iftex
## @ifinfo
## @example
##   x(k+1) = a x(k) + b u(k)
## @end example
## @end ifinfo
## 
## @strong{Inputs}
## @table @var
## @item a
## @var{n} by @var{n} matrix
## @item b
## @var{n} by @var{m} matrix
## @end table
##
## @strong{Output}
## @table @var
## @item m 
## @var{n} by @var{n} matrix, satisfies
## @iftex
## @tex
## $$ ama^T - m + bb^T = 0 $$
## @end tex
## @end iftex
## @ifinfo
## @example
##  a m a' - m + b*b' = 0
## @end example
## @end ifinfo
## @end table
## @end deftypefn

## Author: A. S. Hodel <a.s.hodel@eng.auburn.edu>
## Created: July 1995

function m = dgram (a, b)

  ## let dlyap do the error checking...

  m = dlyap (a, b*b');

endfunction
