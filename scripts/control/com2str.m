## Copyright (C) 1998 Auburn University.  All Rights Reserved
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
 
## usage retval = com2str(zz{,flg})
##  
## convert complex number to a string
## zz: complex number
## flg: format flag
##      0 (default):            -1, 0, 1,   1i,   1 + 0.5i
##      1 (for use with zpout): -1, 0, + 1, + 1i, + 1 + 0.5i

function retval = com2str (zz, flg)

  if (nargin < 1 | nargin > 2)
    usage("com2str(zz{,flg})");
  endif
  if(nargin == 1)
    flg = 0;
  endif
 
  if( !(is_scalar(zz) & is_scalar(flg) ) )
    error("com2str: arguments must be a scalar.");
  endif

  if(flg != 0 & flg != 1)
    error(["Illegal flg value: ",num2str(flg)]);
  endif

  sgns = "+-";
  rz = real(zz);
  iz = imag(zz);
  az = abs(zz);
  if(iz == 0)
    ## strictly a real number
    switch(flg)
    case(0)
      retval = num2str(rz);
    case(1)
      retval = [ sgns(1+(rz< 0))," ", num2str(abs(rz))];
    endswitch
  elseif(rz == 0)
    ## strictly an imaginary number
    switch(flg)
    case(0)
      retval = num2str(iz);
    case(1)
      retval = [ sgns(1+(iz< 0))," ", num2str(abs(iz)),"i"];
    endswitch
  else
    ## complex number
    ## strictly an imaginary number
    switch(flg)
    case(0)
      retval = [num2str(rz)," ",com2str(i*iz,1)];
    case(1)
      retval = [ sgns(1+(rz< 0))," ", num2str(abs(rz))," ",com2str(i*iz,1)];
    endswitch
  endif
  
endfunction
