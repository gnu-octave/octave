## Copyright (C) 2000, 2006, 2007, 2008, 2009 Kai Habel
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
## @deftypefn  {Function File} {@var{z} =} trapz (@var{y})
## @deftypefnx {Function File} {@var{z} =} trapz (@var{x}, @var{y})
## @deftypefnx {Function File} {@var{z} =} trapz (@dots{}, @var{dim})
## 
## Numerical integration using trapezoidal method.  @code{trapz
## (@var{y})} computes the integral of the @var{y} along the first
## non-singleton dimension.  If the argument @var{x} is omitted a 
## equally spaced vector is assumed.  @code{trapz (@var{x}, @var{y})} 
## evaluates the integral with respect to @var{x}.
##  
## @seealso{cumtrapz}
## @end deftypefn

## Author:      Kai Habel <kai.habel@gmx.de>
##
## also: June 2000 - Paul Kienzle (fixes,suggestions) 
## 2006-05-12 David Bateman - Modified for NDArrays

function z = trapz (x, y, dim)
        
  if (nargin < 1) || (nargin > 3)
    print_usage ();
  endif

  nd = ndims (x);
  sz = size (x);

  have_x = false;
  have_dim = false;
  if (nargin == 3)
    have_x = true;
    have_dim = true;
  endif
  if (nargin == 2)
    if (! size_equal (x, y) && isscalar (y))
      dim = y;
      have_dim = true;
    else
      have_x = true;
    endif
  endif

  if (! have_dim)
    ## Find the first non-singleton dimension.
    dim = find (sz > 1, 1);
    if (isempty (dim))
      dim = 1;
    endif
  else
    dim = floor (dim);
    if (dim < 1 || dim > nd)
      error ("trapz: invalid dimension DIM along which to sort");
    endif
  endif
 
  n = sz(dim);
  idx1 = cell ();
  for i = 1:nd
    idx1{i} = 1:sz(i);
  endfor
  idx2 = idx1;
  idx1{dim} = 2 : n;    
  idx2{dim} = 1 : (n - 1);

  if (! have_x)
    z = 0.5 * sum (x(idx1{:}) + x(idx2{:}), dim);
  else
    if (! size_equal (x, y))
      error ("trapz: x and y must have same shape");
    endif
    z = 0.5 * sum ((x(idx1{:}) - x(idx2{:})) .* 
                   (y(idx1{:}) + y(idx2{:})), dim);
  endif
endfunction

%!assert (trapz(1:5), 12)
%!assert (trapz(0:0.5:2,1:5), 6)
%!assert (trapz([1:5;1:5],2),[12;12])
%!assert (trapz([1:5;1:5].',1),[12,12])
%!assert (trapz([0:0.5:2;0:0.5:2],[1:5;1:5],2),[6;6])
%!assert (trapz([0:0.5:2;0:0.5:2].',[1:5;1:5].',1),[6,6])
