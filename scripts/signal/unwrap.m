## Copyright (C) 2000  Bill Lash
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2, or (at your option)
## any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, write to the Free
## Software Foundation, 59 Temple Place - Suite 330, Boston, MA
## 02111-1307, USA.

## -*- texinfo -*-
## @deftypefn {Function File} {@var{b} =} unwrap (@var{a}, @var{tol}, @var{dim})
## 
## Unwrap radian phases by adding multiples of 2*pi as appropriate to
## remove jumps greater than @var{tol}.  @var{tol} defaults to pi.
##
## Unwrap will unwrap along the columns of @var{a} unless the row
## dimension of @var{a} is 1 or @var{dim} is given with a
## value of 1, when it will unwrap along the row(s).
## @end deftypefn

## Author: Bill Lash <lash@tellabs.com>

function retval = unwrap (a, tol, dim)
        
  if (nargin < 1 || nargin > 3)
    usage ("unwrap (a [, tol [, dim]])")
  endif

  if (nargin < 3)
    if (rows (a) == 1)
      ## Row vector, go along the row.
      dim = 1;
    else
      ## Otherwise go along the columns.
      dim = 2;
    endif
  endif

  if (nargin < 2)
    tol = pi;
  endif

  ## If TOL is not provided but dim is, handle it.
  if (isempty (tol))
    tol = pi;
  endif

  ## Don't let anyone use a negative value for TOL.
  tol = abs (tol);
  
  rng = 2*pi;
  
  ## Put data in a form so that we can unwrap each column.
  if (dim == 1)
    ra = a.';
  else
    ra = a;
  endif
  n = columns (ra);
  m = rows (ra);

  ## Handle case where we are trying to unwrap a scalar, or only have
  ## one sample in the specified dimension.
  if (m == 1)       
    retval = a;     
    return;         
  endif

  ## Take first order difference to see so that wraps will show up
  ## as large values, and the sign will show direction.
  d = [ zeros(1,n); ra(1:m-1,:)-ra(2:m,:) ];

  ## Find only the peaks, and multiply them by the range so that there
  ## are kronecker deltas at each wrap point multiplied by the range
  ## value.
  p =  rng * (((d > tol) > 0) - ((d < -tol) > 0));

  ## Now need to "integrate" this so that the deltas become steps.
  r = cumsum (p);

  ## Now add the "steps" to the original data and put output in the
  ## same shape as originally.
  if (dim == 1)
    retval = (ra + r).';
  else
    retval = (ra + r);
  endif

endfunction
