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
## @deftypefn {Function File} {@var{retval} =} is_detectable (@var{a}, @var{c}, @var{tol}, @var{dflg})
## @deftypefnx {Function File} {@var{retval} =} is_detectable (@var{sys}, @var{tol})
## Test for detactability (observability of unstable modes) of (@var{a}, @var{c}).
##
## Returns 1 if the system @var{a} or the pair (@var{a}, @var{c}) is
## detectable, 0 if not, and -1 if the system has unobservable modes at the
## imaginary axis (unit circle for discrete-time systems).
##
## @strong{See} @command{is_stabilizable} for detailed description of
## arguments and computational method.
##
##
## @end deftypefn
## @seealso{is_stabilizable, size, rows, columns, length, ismatrix,
## isscalar, and isvector}

## Author: A. S. Hodel <a.s.hodel@eng.auburn.edu>
## Created: August 1993
## Updated by John Ingram (ingraje@eng.auburn.edu) July 1996.

function [retval, U] = is_detectable (a, c, tol, dflg)

  if( nargin < 1)
    usage("retval = is_detectable(a , {c , tol, dlfg})");
  elseif(isstruct(a))
    ## system form
    if(nargin == 2)
      tol = c;
    elseif(nargin > 2)
      usage("retval = is_detectable(sys {, tol})");
    endif
    dflg = is_digital(a);
    [a,b,c] = sys2ss(a);
  else
    if ((nargin > 4)||(nargin == 1))
      usage("retval = is_detectable(a , {c , tol, dflg})");
    endif
    if (~exist("dflg"))
      dflg = 0;
    end
  end

  if(~exist("tol"))
    tol = 200*eps;
  end    
  retval = is_stabilizable(a',c',tol,dflg);

endfunction

