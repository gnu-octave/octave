## Copyright (C) 2007  David Bateman  <dbateman@free.fr>
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
## @deftypefn {Function File} {@var{vi} =} griddata3 (@var{x}, @var{y}, @var{z}, @var{v} @var{xi}, @var{yi}, @var{zi}, @var{method}, @var{options})
## 
## Generate a regular mesh from irregular data using interpolation.
## The function is defined by @code{@var{y} = f (@var{x},@var{y},@var{z})}.
## The interpolation points are all @var{xi}.  
##
## The interpolation method can be 'nearest' or 'linear'. If method is 
## omitted it defaults to 'linear'.
## @seealso{griddata, delaunayn}
## @end deftypefn

function [yi] = griddata3 (x,y,z,v,xi,yi,zi,method,varargin)
	
  if (nargin < 7) 
    print_usage();
  endif

  if (!all( (size(x)==size(y)) & (size(x)==size(z)) & ...
	   (size(x)==size(v))))
    error("griddata3: x,y,z,v must be vectors of same length");
  endif

  ## meshgrid xi, yi and zi if they are vectors unless they
  ## are vectors of the same length 
  if (isvector(xi) && isvector(yi) && isvector(zi) && ...
      (numel(xi) != numel(yi) || numel(xi) != numel(zi))) 
    [xi,yi,zi] = meshgrid(xi,yi,zi);
  endif

  if (any(size(xi) != size(yi)) || any(size(xi) != size(zi)))
    error("griddata: xi, yi and zi must be vectors or matrices of same size");
  endif

  vi = gridata ([x(:),y(:),z(:)], v(:), [xi(:),yi(:),zi(:)], varargin{:});
  vi = reshape (vi, size(xi));
endfunction

