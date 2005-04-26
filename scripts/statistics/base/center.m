## Copyright (C) 1995, 1996, 1997  Kurt Hornik
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
## @deftypefn {Function File} {} center (@var{x})
## @deftypefnx {Function File} {} center (@var{x}, @var{dim})
## If @var{x} is a vector, subtract its mean.
## If @var{x} is a matrix, do the above for each column.
## If the optional argument @var{dim} is given, perform the above
## operation along this dimension
## @end deftypefn

## Author: KH <Kurt.Hornik@ci.tuwien.ac.at>
## Description: Center by subtracting means

function retval = center (x, varargin)

  if (nargin != 1 && nargin != 2)
    usage ("center (x)");
  endif

  if (isvector (x))
    retval = x - mean (x, varargin{:});
  elseif (ismatrix (x))
    if nargin < 2
      dim = min (find (size (x) > 1));
      if isempty (dim), 
	dim=1; 
      endif;
    else
      dim = varargin {1};
    endif
    sz = ones (1, ndims (x));
    sz (dim) = size (x, dim);
    retval = x - repmat (mean (x, dim), sz);
  elseif (isempty (x))
    retval = x;
  else
    error ("center: x must be a vector or a matrix");
  endif

endfunction
