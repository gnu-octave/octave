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

function [retval,U] = is_controllable (a, b, tol)
# [retval, U] = is_controllable (a, b {,tol})
#             = is_controllable (sys{, tol})
#      Returns retval=1 if the system sys or the pair (a, b) is controllable
#                     0 if not.
# U is an orthogonal basis of the controllable subspace. 
#
# Controllability is determined by applying Arnoldi iteration with
# complete re-orthogonalization to obtain an orthogonal basis of the
# Krylov subspace.
#
#   span ([b,a*b,...,a^   b]).
#
# tol is a roundoff paramter, set to 10*eps if omitted.
#
# See also: size, rows, columns, length, is_matrix, is_scalar, is_vector
#     is_observable, is_stabilizable, is_detectable, krylov, krylovb

# Written by A. S. Hodel (scotte@eng.auburn.edu) August, 1993.
# Updated by A. S. Hodel (scotte@eng.auburn.edu) Aubust, 1995 to use krylovb 
# Updated by John Ingram (ingraje@eng.auburn.edu) July, 1996 for packed systems

  deftol = 1;    # assume default tolerance
  if(nargin < 1 | nargin > 3)
    usage("[retval,U] = %s\n\t%s", "is_controllable(a {, b ,tol})", ...
	"is_controllable(sys{,tol})");
  elseif(is_struct(a))
    # system structure passed.
    sys = sysupdate(a,"ss");
    [a,bs] = sys2ss(sys);
    if(nargin > 2)
      usage("[retval,U] = is_controllable(sys{,tol})");
    elseif(nargin == 2)
      tol = b;		% get tolerance
      deftol = 0;
    endif
    b = bs;
  else
    # a,b arguments sent directly.
    if(nargin < 2)
      usage("[retval,U] = is_controllable(a {, b ,tol})");
    else
      deftol = 1;
    endif
  endif

  # check for default tolerance
  if(deftol) tol = 1000*eps; endif

  # check tol dimensions
  if( !is_scalar(tol) )
    error("is_controllable: tol(%dx%d) must be a scalar", ...
	rows(tol),columns(tol));
  elseif( !is_sample(tol) )
    error("is_controllable: tol=%e must be positive",tol);
  endif

  # check dimensions compatibility
  n = is_square (a);
  [nr, nc] = size (b);

  if (n == 0 | n != nr | nc == 0)
    warning("is_controllable: a=(%dx%d), b(%dx%d)",rows(a),columns(a),nr,nc);
    retval = 0;
  else
    # call block-krylov subspace routine to get an orthogonal basis
    # of the controllable subspace.
    if(nc == 1)
      [U,H,Ucols] = krylov(a,b,n,tol,1);
      U = U(:,1:Ucols);
    else
      [U,Ucols] = krylovb(a,b,n,tol);
      U = U(:,1:Ucols);
    endif

    retval = (Ucols == n);
  endif
endfunction
