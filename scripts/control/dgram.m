## Copyright (C) 1996 Auburn University.  All rights reserved.
##
## This file is part of Octave. 
##
## Octave is free software; you can redistribute it and/or modify it 
## under the terms of the GNU General Public License as published by the 
## Free Software Foundation; either version 2, or (at your option) any 
## later version. 
## 
## Octave is distributed in the hope that it will be useful, but WITHOUT 
## ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or 
## FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License 
## for more details.
## 
## You should have received a copy of the GNU General Public License 
## along with Octave; see the file COPYING.  If not, write to the Free 
## Software Foundation, 59 Temple Place, Suite 330, Boston, MA 02111 USA. 
 
## -*- texinfo -*-
## @deftypefn {Function File } { @var{m} =} dgram ( @var{a}, @var{b})
##  Return controllability grammian of discrete time system
## @example
##   x(k+1) = a x(k) + b u(k)
## @end example
## 
## @strong{Inputs}
## @table @var
## @item a
## @var{n} by @var{n} matrix
## @item b
## @var{n} by @var{m} matrix
## @end table
## 
## @strong{Outputs}
## @var{m} (@var{n} by @var{n}) satisfies
## @example
##  a m a' - m + b*b' = 0 
## @end example
## 
## @end deftypefn

## Author: A. S. Hodel <a.s.hodel@eng.auburn.edu>
## Created: July 1995

function m = dgram (a, b)

  ## let dlyap do the error checking...
  m = dlyap(a,b*b');

endfunction
