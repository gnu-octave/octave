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

function [retval,U] = is_observable (a,c,tol)

# [retval,U] = is_observable (a,c,tol)
# usage: is_observable (a , c {,tol})
#     or is_observable (sys {,tol})
#
# Default: tol = 10*norm(a,'fro')*eps
#
# Returns 1 if the system, a, is observable, 1 if the pair (a, c) is 
# observable, or 0 if not.
#
# See also: size, rows, columns, length, is_matrix, is_scalar, is_vector.

# Written by A. S. Hodel (scotte@eng.auburn.edu) August 1993.
# Updated by John Ingram (ingraje@eng.auburn.edu) July 1996.

  if( nargin < 1) 
    usage("[retval,U] = is_observable(a , c {, tol})");
  elseif(is_struct(a))
    # system form
    if(nargin == 2)
      tol = c;
    elseif(nargin > 2)
      usage("[retval,U] = is_observable(sys {, tol})");
    endif
    [a,b,c] = sys2ss(a);
  elseif(nargin > 3)
    usage("[retval,U] = is_observable(a , c {, tol})");
  endif
  if(exist("tol"))
    [retval,U] = is_controllable (a', c', tol);
  else
    [retval,U] = is_controllable (a', c');
  endif

endfunction

