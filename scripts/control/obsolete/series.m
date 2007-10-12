## Copyright (C) 1996, 2000, 2004, 2005, 2007
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

## Forms the series connection of two systems.
##
## Superseded by sysmult.  Do not use this routine!
## used internally in zp2ss
##
## Type of input: Transfer functions
## Command:       [num,den]=series(num1,den1,num2,den2)
## Forms the series representation of the two transfer functions.
##
## Type of input: State space systems
## Command:       [a,b,c,d]=series(a1,b1,c1,d1,a2,b2,c2,d2)
## Forms the series representation of the two state space system arguments.
## The series connected system will have the inputs of system 1 and the
## outputs of system 2.
##
## Type of input: system data structure
## Command:       syst=series(syst1,syst2)
## Forms the series representation of the two mu system arguments.

## Author: David Clem
## Created: August 15, 1994

function [a, b, c, d] = series (a1, b1, c1, d1, a2, b2, c2, d2)

  ## If two arguments input, take care of mu system case

  warning("series is superseded by sysmult; use sysmult instead.")

  muflag = 0;
  if(nargin == 2)
    temp=b1;
    [a1,b1,c1,d1]=sys2ss(a1);
    [a2,b2,c2,d2]=sys2ss(temp);
    muflag = 1;
  endif

  ## If four arguments input, put two transfer functions in series

  if(nargin == 4)
    a = conv(a1,c1);    % was conv1
    b = conv(b1,d1);    % was conv1
    c = 0;
    d = 0;

    ## Find series combination of 2 state space systems

  elseif((nargin == 8)||(muflag == 1))

    ## check matrix dimensions

    [n1,m1,p1] = abcddim(a1,b1,c1,d1);
    [n2,m2,p2] = abcddim(a2,b2,c2,d2);

    if((n1 == -1) || (n2 == -1))
      error("Incorrect matrix dimensions");
    endif

    ## check to make sure the number of outputs of system1 equals the number
    ## of inputs of system2

   if(p1 ~= m2)
     error("System 1 output / System 2 input connection sizes do not match");
   endif

   ## put the two state space systems in series

    a = [a1, zeros(rows(a1),columns(a2));b2*c1, a2];
    b = [b1;b2*d1];
    c = [d2*c1, c2];
    d = [d2*d1];

    ## take care of mu output

    if(muflag == 1)
      a=ss(a,b,c,d);
      b=c=d=0;
    endif
  endif

endfunction

