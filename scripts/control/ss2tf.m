## Copyright (C) 1996 Auburn University.  All Rights Reserved.
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
## @deftypefn {Function File } { outputs =} ss2tf ( inputs ) 
## @format
##  [num,den] = ss2tf(a,b,c,d)
##  Conversion from tranfer function to state-space.
##  The state space system
##       . 
##       x = Ax + Bu
##       y = Cx + Du
## 
##  is converted to a transfer function
## 
##                 num(s)
##           G(s)=-------
##                 den(s)
## 
##  used internally in system data structure format manipulations
## 
## 
## @end format
## @end deftypefn
 
function [num, den] = ss2tf (a, b, c, d)

  ## Written by R. Bruce Tenison (June 24, 1994) btenison@eng.auburn.edu
  ## a s hodel: modified to allow for pure gain blocks Aug 1996

  ## Check args
  [n,m,p] = abcddim(a,b,c,d);
  if (n == -1)
    num = [];
    den = [];
    error("ss2tf: Non compatible matrix arguments");
  elseif ( (m != 1) | (p != 1))
    num = [];
    den = [];
    error(["ss2tf: not SISO system: m=",num2str(m)," p=",num2str(p)]);
  endif
  
  if(n == 0)
    ## gain block only
    num = d;
    den = 1;
  else
    ## First, get the denominator coefficients
    den = poly(a);
  
    ## Get the zeros of the system
    [zz,g] = tzero(a,b,c,d);

    ## Form the Numerator (and include the gain)
    if (!isempty(zz))
      num = g * poly(zz);
    else
      num = g;
    endif
  
    ## the coefficients must be real
    den = real(den);
    num = real(num);
  endif
endfunction

