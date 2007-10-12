## Copyright (C) 1996, 2000, 2002, 2004, 2005, 2007
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

## O B S O L E T E: use ss instead.
## function Asys = packsys(a,b,c[,d,dflg])
##
##   dflg: 0 for continuous time system, 1 for discrete-time system.
##
## defaults:
##      D: 0 matrix of appropriate dimension.
##   dflg: 0 (continuous time)
##
## Note: discrete-state sampling time is not included!

## Author: R. Bruce Tenison <btenison@eng.auburn.edu>
## Created: July 29, 1994
## Modified by David Clem November 13, 1994
## Modified by A. S. Hodel April 1995

function Asys = packsys (a, b, c, d, dflg)

  warning("packsys is obsolete!  Use ss instead.");

  if (nargin < 3 || nargin > 5)
    disp("packsys: Invalid number of arguments")
  endif

  ## check dflg
  if(nargin == 5)
    if( !isscalar(dflg))
      [m,n] = size(dflg);
      error(["packsys: dflg (",num2str(m),",",num2str(n), ...
        ") must be a scalar."]);
    elseif( (dflg != 0) && (dflg != 1))
      error(["packsys: dflg=",num2str(dflg),"must be 0 or 1"]);
    endif
  else
    ## default condition
    dflg = 0;
  endif

  if (nargin == 3)
    ## No D matrix.  Form a zero one!
    [brows,bcols] = size(b);
    [crows,ccols] = size(c);
    d = zeros(crows,bcols);
  endif

  [n,m,p] = abcddim(a,b,c,d);
  if (n == -1 || m == -1 || p == -1)
    error("packsys: incompatible dimensions")
  endif

  Asys = ss(a,b,c,d,dflg);

endfunction
