## Copyright (C) 2000, 2006, 2007 Kai Habel
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
## @deftypefn {Function File} {@var{x} = } gradient (@var{M})
## @deftypefnx {Function File} {[@var{x}, @var{y}, @dots{}] = } gradient (@var{M})
## @deftypefnx {Function File} {[@dots{}] = } gradient (@var{M}, @var{s})
## @deftypefnx {Function File} {[@dots{}] = } gradient (@var{M}, @var{dx}, @var{dy}, @dots{})
##
## Calculates the gradient. @code{@var{x} = gradient (@var{M})}
## calculates the one dimensional gradient if @var{M} is a vector. If
## @var{M} is a matrix the gradient is calculated for each row.
##
## @code{[@var{x}, @var{y}] = gradient (@var{M})} calculates the one
## dimensional gradient for each direction if @var{M} if @var{M} is a
## matrix. Additional return arguments can be use for multi-dimensional
## matrices.
##
## Spacing values between two points can be provided by the
## @var{dx}, @var{dy} or @var{h} parameters. If @var{h} is supplied it
## is assumed to be the spacing in all directions. Otherwise, separate
## values of the spacing can be supplied by the @var{dx}, etc variables.
## A scalar value specifies an equidistant spacing, while a vector value
## can be used to specify a variable spacing. The length must match
## their respective dimension of @var{M}.
## 
## At boundary points a linear extrapolation is applied. Interior points
## are calculated with the first approximation of the numerical gradient
##
## @example
## y'(i) = 1/(x(i+1)-x(i-1)) *(y(i-1)-y(i+1)).
## @end example
## 
## @end deftypefn

## Author:  Kai Habel <kai.habel@gmx.de>
## Modified: David Bateman <dbateman@free.fr> Added NDArray support

function [varargout] = gradient (M, varargin)
  
  if (nargin < 1)
    print_usage ()
  endif

  transposed = false;
  if (isvector (M))
    ## make a column vector
    transposed = (size (M, 2) == 1);
    M = M(:)';
  endif

  nd = ndims (M);
  sz = size (M);
  if (nargin > 2 && nargin != nd + 1)
    print_usage ()
  endif
    
  d = cell (1, nd);
  if (nargin == 1)
    for i=1:nd
      d{i} = ones (sz(i), 1);
    endfor
  elseif (nargin == 2)
    if (isscalar (varargin{1}))
      for i = 1:nd
	d{i} = varargin{1} * ones (sz(i), 1);
      endfor
    else
      for i = 1:nd
	d{i} = varargin{1};
      endfor
    endif
  else
    for i=1:nd
      if (isscalar (varargin{i}))
	## Why the hell did matlab decide to swap these two values?
	if (i == 1)
	  d{2} = varargin{1} * ones (sz(2), 1);
	elseif (i == 2)
	  d{1} = varargin{2} * ones (sz(1), 1);
	else
	  d{i} = varargin{i} * ones (sz(i), 1);
	endif
      else
	## Why the hell did matlab decide to swap these two values?
	if (i == 1)
	  d{2} = varargin{1};
	elseif (i == 2)
	  d{1} = varargin{2};
	else
	  d{i} = varargin{i};
	endif
      endif
    endfor
  endif

  for i = 1:max (2, min (nd, nargout))
    mr = sz(i);
    mc = prod ([sz(1:i-1), sz(i+1:nd)]);
    Y = zeros (size (M), class (M));

    if (mr > 1)
      ## top and bottom boundary
      Y(1,:) = diff (M(1:2,:)) / d{i}(1);
      Y(mr,:) = diff (M(mr-1:mr,:)) / d{i}(mr-1);
    endif

    if (mr > 2)
      ## interior points
      Y(2:mr-1,:) = (M(3:mr,:) .- M(1:mr-2,:)) ...
	  ./ kron (d{i}(1:mr-2) .+ d{i}(2:mr-1), ones (1, mc));
    endif
    varargout{i} = ipermute (Y, [i:nd,1:i-1]);
    M = permute (M, [2:nd,1]);
  endfor

  ## Why the hell did matlab decide to swap these two values?
  tmp = varargout{1};
  varargout{1} = varargout{2};
  varargout{2} = tmp;

  if (transposed)
    varargout{1} = varargout{1}.';
  endif
endfunction
