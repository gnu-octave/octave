## Copyright (C) 1995  Friedrich Leisch
##
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2, or (at your option)
## any later version.
##
## This program is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this file.  If not, write to the Free Software Foundation,
## 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

## usage:  durbinlevinson (c, [oldphi, oldv])
##
## Performs one step of the Durbin-Levinson algorithm.
##
## The vector c_t = [gamma_0, ..., gamma_t] contains the autocovariances
## from lag 0 to t, oldphi the coefficients based on c_(t-1) and oldv
## the corresponding error.
##
## If oldphi is omitted, all steps from 1 to t of the algorithm are
## performed.

## Author:  FL <Friedrich.Leisch@ci.tuwien.ac.at>
## Description:  Perform one step of the Durbin-Levinson algorithm

function [newphi, newv] = durbinlevinson (c, oldphi, oldv)

  if( !((nargin == 1) || (nargin == 3)) )
    usage ("durbinlevinson (c, [oldphi, oldv])");
  endif

  if( columns (c) > 1 )
    c=c';
  endif

  newphi = 0;
  newv = 0;

  if (nargin == 3)

    t = length (oldphi) + 1;

    if (length (c) < t+1)
      error ("durbilevinson:  c too small");
    endif

    if (oldv == 0)
      error ("durbinlevinson: oldv = 0");
    endif

    if (rows (oldphi) > 1 )
      oldphi = oldphi';
    endif

    newphi = zeros (1, t);
    newphi(1) = ( c(t+1) - oldphi * c(2:t) ) / oldv;
    for i = 2 : t
      newphi(i) = oldphi(i-1) - newphi(1) * oldphi(t-i+1);
    endfor
    newv = ( 1 - newphi(1)^2 ) * oldv;

  elseif(nargin == 1)

    tt = length (c)-1;
    oldphi = c(2) / c(1);
    oldv = ( 1 - oldphi^2 ) * c(1);

    for t = 2 : tt

      newphi = zeros (1, t);
      newphi(1) = ( c(t+1) - oldphi * c(2:t) ) / oldv;
      for i = 2 : t
        newphi(i) = oldphi(i-1) - newphi(1) * oldphi(t-i+1);
      endfor
      newv = ( 1 - newphi(1)^2 ) * oldv;

      oldv = newv;
      oldphi = newphi;

    endfor

  endif

endfunction
