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

function [retval,U] = is_stabilizable (a, b, tol)

# Usage: [retval,U] = is_stabilizable (a {, b, tol})
#
# Returns retval = 1 if the system, a, is stabilizable, if the pair (a, b) is 
# stabilizable, or 0 if not.
#         U = orthogonal basis of controllable subspace.
#
# Controllable subspace is determined by applying Arnoldi iteration with
# complete re-orthogonalization to obtain an orthogonal basis of the
# Krylov subspace.
#
#   span ([b,a*b,...,a^   b]).
#
# tol is a roundoff paramter, set to 200*eps if omitted.
#
# See also: size, rows, columns, length, is_matrix, is_scalar, is_vector
#     is_observable, is_stabilizable, is_detectable

# Written by A. S. Hodel (scotte@eng.auburn.edu) August, 1993.
# Updated by A. S. Hodel (scotte@eng.auburn.edu) Aubust, 1995 to use krylovb 
# Updated by John Ingram (ingraje@eng.auburn.edu) July, 1996 to accept systems

  if(nargin < 1)        usage("[retval,U] = is_stabilizable(a {, b ,tol})");
  elseif(is_struct(a))
    # sustem passed.
    if(nargin == 2)
      tol = b;          % get tolerance
    elseif(nargin > 2)
      usage("[retval,U] = is_stabilizable(sys{,tol})");
    endif
    [a,b] = sys2ss(sys);
  else
    # a,b arguments sent directly.
    if(nargin > 3)
      usage("[retval,U] = is_stabilizable(a {, b ,tol})");
    endif
  endif

  if(exist("tol"))
    [retval,U] = is_controllable(a,b,tol);
  else
    [retval,U] = is_controllable(a,b);
    tol = 1e2*rows(b)*eps;
  endif
  
  if( !retval & columns(U) > 0)
    # now use an ordered Schur decomposition to get an orthogonal
    # basis of the unstable subspace...
    n = rows(a);
    [ua,s] = schur(-(a+eye(n)*tol),'A');
    k = sum( real(eig(a)) >= 0 );	# count unstable poles 

    if( k > 0 )
      ua = ua(:,1:k);
      # now see if span(ua) is contained in span(U)
      retval = (norm(ua - U*U'*ua) < tol);
    else
      retval = 1;			# all poles stable
    endif
  endif

endfunction
