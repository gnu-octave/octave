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
## Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301, USA.

## -*- texinfo -*-
## @deftypefn {Function File} {@var{b} =} unwrap (@var{a}, @var{tol}, @var{dim})
## 
## Unwrap radian phases by adding multiples of 2*pi as appropriate to
## remove jumps greater than @var{tol}.  @var{tol} defaults to pi.
##
## Unwrap will unwrap along the first non-singleton dimension of
## @var{a}, unless the optional argument @var{dim} is given, in 
## which case the data will be unwrapped along this dimension
## @end deftypefn

## Author: Bill Lash <lash@tellabs.com>

function retval = unwrap (a, tol, dim)
        
  if (nargin < 1 || nargin > 3)
    usage ("unwrap (a [, tol [, dim]])")
  endif

  nd = ndims (a);
  sz = size (a);

  if (nargin == 3)
    if (! (isscalar (dim) && dim == round (dim)) && dim > 0 && 
	dim < (nd + 1))
      error ("unwrap: dim must be an integer and valid dimension");
    endif
  else
    ## Find the first non-singleton dimension
    dim  = 1;
    while (dim < nd + 1 && sz (dim) == 1)
      dim = dim + 1;
    endwhile
    if (dim > nd)
      dim = 1;
    endif
  endif

  if (nargin < 2 || isempty (tol))
    tol = pi;
  endif

  ## Don't let anyone use a negative value for TOL.
  tol = abs (tol);
  
  rng = 2*pi;
  m = sz (dim);

  ## Handle case where we are trying to unwrap a scalar, or only have
  ## one sample in the specified dimension.
  if (m == 1)       
    retval = a;     
    return;         
  endif

  ## Take first order difference to see so that wraps will show up
  ## as large values, and the sign will show direction.
  idx = cell ();
  for i = 1:nd
    idx {i} = 1:sz(i);
  endfor
  idx {dim} = [1,1:m-1];
  d = a (idx {:}) - a;

  ## Find only the peaks, and multiply them by the range so that there
  ## are kronecker deltas at each wrap point multiplied by the range
  ## value.
  p =  rng * (((d > tol) > 0) - ((d < -tol) > 0));

  ## Now need to "integrate" this so that the deltas become steps.
  r = cumsum (p, dim);

  ## Now add the "steps" to the original data and put output in the
  ## same shape as originally.
  retval = a + r;

endfunction
