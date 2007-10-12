## Copyright (C) 1993, 1994, 1995, 2000, 2002, 2004, 2005, 2006, 2007
##               Auburn University.  All rights reserved.
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
## @seealso{size, rows, columns, length, ismatrix, isscalar, isvector}
## @end deftypefn

## Author: A. S. Hodel <a.s.hodel@eng.auburn.edu>
## Created: August 1993
## Updated by John Ingram (ingraje@eng.auburn.edu) July 1996.

function [retval, U] = is_observable (a, c, tol)

  if( nargin < 1)
    print_usage ();
  elseif(isstruct(a))
    ## system form
    if(nargin == 2)
      tol = c;
    elseif(nargin > 2)
      print_usage ();
    endif
    [a,b,c] = sys2ss(a);
  elseif(nargin > 3)
    print_usage ();
  endif
  if(exist("tol"))
    [retval,U] = is_controllable (a', c', tol);
  else
    [retval,U] = is_controllable (a', c');
  endif

endfunction

