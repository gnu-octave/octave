# Copyright (C) 1993, 1994, 1995 Auburn University.  All Rights Reserved
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
## @deftypefn {Function File } { @var{retval} =} is_stable (@var{a}@{,@var{tol},@var{dflg}@})
## @deftypefnx {Function File } { @var{retval} =} is_stable (@var{sys}@{,@var{tol}@})
##  Returns retval = 1 if the matrix @var{a} or the system @var{sys}
## is stable, or 0 if not.
## 
## @strong{Inputs}
## @table @var
## @item  tol
## is a roundoff paramter, set to 200*@var{eps} if omitted.
## @item dflg
## Digital system flag (not required for system data structure):
## @table @code
## @item @var{dflg} != 0
## stable if eig(a) in unit circle
## 
## @item @var{dflg} == 0
## stable if eig(a) in open LHP (default)
## @end table
## @end table
## @end deftypefn

## See also: size, rows, columns, length, is_matrix, is_scalar, is_vector
##     is_observable, is_stabilizable, is_detectable, krylov, krylovb

function retval = is_stable (a, tol, disc)
# Written by A. S. Hodel (scotte@eng.auburn.edu) August, 1993.
# Updated by John Ingram (ingraje@eng.auburn.edu) July, 1996 for systems
# Updated to simpler form by a.s.hodel 1998

  if( (nargin < 1) | (nargin > 3) )   usage("is_stable(a {,tol,disc})");
  elseif(is_struct(a))
    # system was passed
    if(nargin < 3)			disc = is_digital(a);
    elseif(disc != is_digital(a))
      warning("is_stable: disc =%d does not match system",disc)
    endif
    sys = sysupdate(a,"ss");
    a = sys2ss(sys);
  else
    if(nargin < 3)		disc = 0;		endif
    if(is_square(a) == 0)
      error("A(%dx%d) must be square",rows(A), columns(A));
    endif
  endif

  if(nargin < 2)		tol = 200*eps;
  elseif( !is_scalar(tol) )
    error("is_stable: tol(%dx%d) must be a scalar",rows(tol),columns(tol));
  endif
 
  l = eig(a);
  if(disc)	nbad = sum(abs(l)*(1+tol) > 1);
  else		nbad = sum(real(l)+tol > 0);		endif
  retval = (nbad == 0);   

endfunction
