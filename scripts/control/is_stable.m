# Copyright (C) 1993, 1994, 1995 John W. Eaton
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
# Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

function retval = is_stable (a, tol, disc)

# Usage: retval = is_stable (a {,tol,disc})
# or     retval = is_stable (sys{,tol})
#
# Returns retval = 1 if the matrix a or the system a is stable, or 0 if not.
#
# tol is a roundoff paramter, set to 200*eps if omitted.
# disc != 0: stable if eig(a) in unit circle
#         0: stable if eig(a) in open LHP (default)
#
# See also: size, rows, columns, length, is_matrix, is_scalar, is_vector
#     is_observable, is_stabilizable, is_detectable, krylov, krylovb

# Written by A. S. Hodel (scotte@eng.auburn.edu) August, 1993.
# Updated by John Ingram (ingraje@eng.auburn.edu) July, 1996 for systems
# Updated to simpler form by a.s.hodel 1998
# $Revision: 2.0.0.2 $

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
