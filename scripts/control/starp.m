# Copyright (C) 1996 Auburn University.  All Rights Reserved.
#
# This file is part of Octave. 
#
# Octave is free software; you can redistribute it and/or modify it 
# under the terms of the GNU General Public License as published by the 
# Free Software Foundation; either version 2, or (at your option) any 
# later version. 
# 
# Octave is distributed in the hope that it will be useful, but WITHOUT 
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or 
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License 
# for more details.
# 
# You should have received a copy of the GNU General Public License 
# along with Octave; see the file COPYING.  If not, write to the Free 
# Software Foundation, 59 Temple Place, Suite 330, Boston, MA 02111 USA. 
 
## -*- texinfo -*-
## @deftypefn {Function File } { outputs =} starp ( inputs ) 
## @format
## 
##  [sys] = starp(P, K, ny, nu)
## 
##  Redheffer star product or upper/lower LFT, respectively.
## 
## 
##                +-------+
##      --------->|       |---------> 
##                |   P   |
##           +--->|       |---+  ny
##           |    +-------+   |
##           +-------------------+
##                            |  |
##           +----------------+  |
##           |                   |
##           |    +-------+      |
##           +--->|       |------+ nu 
##                |   K   |
##      --------->|       |--------->
##                +-------+
## 
##  If ny and nu "consume" all inputs and outputs of K then the result
##  is a lower fractional transformation. If ny and nu "consume" all
##  inputs and outputs of P then the result is an upper fractional
##  transformation.
## 
##  ny and/or nu may be negative (= negative feedback)
## @end format
## @end deftypefn

function [sys] = starp(P, K, ny, nu);
# Written by Kai Mueller May 1998

  if((nargin != 2) && (nargin != 4))
    usage("[sys] = starp(P, K, ny, nu)");
  endif
  if (!is_struct(P))
    error("---> P must be in system data structure");
  endif
  if (!is_struct(K))
    error("---> K must be in system data structure");
  endif

  P = sysupdate(P, "ss");
  [n, nz, mp, pp] = sysdimensions(P);
  np = n + nz;
  K = sysupdate(K, "ss");
  [n, nz, mk, pk] = sysdimensions(K);
  nk = n + nz;
  ny_sign = 1;
  nu_sign = 1;
  if (nargin == 2)
    # perform a LFT of P and K (upper or lower)
    ny = min([pp, mk]);
    nu = min([pk, mp]);
  else
    if (ny < 0)
      ny = -ny;
      ny_sign = -1;
    endif
    if (nu < 0)
      nu = -nu;
      nu_sign = -1;
    endif
  endif
  if (ny > pp)
    error("---> P has not enough outputs.");
  endif
  if (nu > mp)
    error("---> P has not enough inputs.");
  endif
  if (ny > mk)
    error("---> K has not enough inputs.");
  endif
  if (nu > pk)
    error("---> K has not enough outputs.");
  endif
  nwp  = mp - nu;
  nzp  = pp - ny;
  nwk  = mk - ny;
  nzk  = pk - nu;
  if ((nwp + nwk) < 1)
    error("---> no inputs left for star product.");
  endif
  if ((nzp + nzk) < 1)
    error("---> no outputs left for star product.");
  endif

  # checks done, form sys
  if (nzp)  Olst = [1:nzp];  endif
  if (nzk)  Olst = [Olst, pp+nu+1:pp+pk];  endif
  if (nwp)  Ilst = [1:nwp];  endif
  if (nwk)  Ilst = [Ilst, mp+ny+1:mp+mk];  endif
  Clst = zeros(ny+nu,2);
  for ii = 1:nu
    Clst(ii,:) = [nwp+ii, nu_sign*(pp+ii)];
  endfor
  for ii = 1:ny
    Clst(nu+ii,:) = [mp+ii, ny_sign*(nzp+ii)];
  endfor
  sys = buildssic(Clst,[],Olst,Ilst,P,K);

endfunction
