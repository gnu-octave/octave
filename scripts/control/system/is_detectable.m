## Copyright (C) 1993, 1994, 1995 Auburn University.  All rights reserved.
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

## -*- texinfo -*-
## @deftypefn {Function File} {[@var{retval}, @var{u}] =} is_detectable (@var{a}, @var{c}, @var{tol})
## @deftypefnx {Function File} {[@var{retval}, @var{u}] =} is_detectable (@var{sys}, @var{tol})
## Test for detactability (observability of unstable modes) of
## (@var{a},@var{c}).
##
## Returns 1 if the system @var{a} or the pair (@var{a},@var{c})is
## detectable, 0 if not.
##
## @strong{See} @code{is_stabilizable} for detailed description of
## arguments and computational method.
##
## Default: tol = 10*norm(a,'fro')*eps
##
## @end deftypefn
## @seealso{is_stabilizable, size, rows, columns, length, is_matrix,
## is_scalar, and is_vector}

## Author: A. S. Hodel <a.s.hodel@eng.auburn.edu>
## Created: August 1993
## Updated by John Ingram (ingraje@eng.auburn.edu) July 1996.

function [retval, U] = is_detectable (a, c, tol)

  if( nargin < 1)
    usage("[retval,U] = is_detectable(a , c {, tol})");
  elseif(is_struct(a))
    ## system form
    if(nargin == 2)
      tol = c;
    elseif(nargin > 2)
      usage("[retval,U] = is_detectable(sys {, tol})");
    endif
    [a,b,c] = sys2ss(a);
  elseif(nargin > 3)
    usage("[retval,U] = is_detectable(a , c {, tol})");
  endif
  if(exist("tol"))
    [retval,U] = is_stabilizable (a', c', tol);
  else
    [retval,U] = is_stabilizable (a', c');
  endif


endfunction

