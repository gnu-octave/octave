## Copyright (C) 1997 Kai P. Mueller
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
## @deftypefn {Function File } {@var{Qs} =} ctrb(@var{sys} @{, @var{b}@})
## @deftypefnx {Function File } {@var{Qs} =} ctrb(@var{A}, @var{B})
## Build controllability matrix
## @example
##              2       n-1
## Qs = [ B AB A B ... A   B ]
## @end example
## 
##  of a system data structure or the pair (@var{A}, @var{B}).
## 
## @strong{Note} @code{ctrb} forms the controllability matrix.
##        The numerical properties of @code{is_controllable}
##        are much better for controllability tests.
## @end deftypefn

function Qs = ctrb (sys, b)

  ## Written by Kai P. Mueller November 4, 1997
  ## based on is_controllable.m of Scottedward Hodel
  ## modified by

  if (nargin == 2)
    a = sys;
  elseif (nargin == 1 && is_struct(sys))
    sysupdate(sys,"ss");
    [a,b] = sys2ss(sys);
  else
    usage("ctrb(sys [, b])")
  endif

  if (!is_abcd(a,b))
    Qs = [];
  else
    ## no need to check dimensions, we trust is_abcd().
    [na, ma] = size(a);
    ## using imb avoids name conflict with the "mb" function
    [inb, imb] = size(b);
    Qs = zeros(na, ma*imb);
    for i = 1:na
      Qs(:, (i-1)*imb+1:i*imb) = b;
      b = a * b;
    endfor
  endif
endfunction
