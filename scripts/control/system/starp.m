## Copyright (C) 1996 Auburn University.  All rights reserved.
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
## @deftypefn {Function File} {} starp (@var{P}, @var{K}, @var{ny}, @var{nu})
##
## Redheffer star product or upper/lower LFT, respectively.
## @example
## @group
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
## @end group
## @end example
## If @var{ny} and @var{nu} ``consume'' all inputs and outputs of
## @var{K} then the result is a lower fractional transformation. 
## If @var{ny} and @var{nu} ``consume'' all inputs and outputs of 
## @var{P} then the result is an upper fractional transformation.
##
## @var{ny} and/or @var{nu} may be negative (i.e. negative feedback).
## @end deftypefn

## Author: Kai P. Mueller <mueller@ifr.ing.tu-bs.de>
## Created: May 1998

function sys = starp (P, K, ny, nu);

  if((nargin != 2) && (nargin != 4))
    print_usage ();
  endif
  if (!isstruct(P))
    error("---> P must be in system data structure");
  endif
  if (!isstruct(K))
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
    ## perform a LFT of P and K (upper or lower)
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

  ## checks done, form sys
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
