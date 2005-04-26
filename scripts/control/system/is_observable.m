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
## Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301 USA.

## -*- texinfo -*-
## @deftypefn {Function File} {[@var{retval}, @var{u}] =} is_observable (@var{a}, @var{c}, @var{tol})
## @deftypefnx {Function File} {[@var{retval}, @var{u}] =} is_observable (@var{sys}, @var{tol})
## Logical check for system observability.
##
## Default: tol = @code{tol = 10*norm(a,'fro')*eps}
##
## Returns 1 if the system @var{sys} or the pair (@var{a}, @var{c}) is
## observable, 0 if not.
##
## See @command{is_controllable} for detailed description of arguments
## and default values.
## @end deftypefn
##
## @seealso{size, rows, columns, length, ismatrix, isscalar, and isvector}

## Author: A. S. Hodel <a.s.hodel@eng.auburn.edu>
## Created: August 1993
## Updated by John Ingram (ingraje@eng.auburn.edu) July 1996.

function [retval, U] = is_observable (a, c, tol)

  if( nargin < 1)
    usage("[retval,U] = is_observable(a , c {, tol})");
  elseif(isstruct(a))
    ## system form
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

