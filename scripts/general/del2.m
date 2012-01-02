## Copyright (C) 2000-2012 Kai Habel
## Copyright (C) 2007  David Bateman
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
## @deftypefn  {Function File} {@var{d} =} del2 (@var{M})
## @deftypefnx {Function File} {@var{d} =} del2 (@var{M}, @var{h})
## @deftypefnx {Function File} {@var{d} =} del2 (@var{M}, @var{dx}, @var{dy}, @dots{})
##
## Calculate the discrete Laplace
## @tex
## operator $( \nabla^2 )$.
## @end tex
## @ifnottex
## operator.
## @end ifnottex
## For a 2-dimensional matrix @var{M} this is defined as
## @tex
## $$d = {1 \over 4} \left( {d^2 \over dx^2} M(x,y) + {d^2 \over dy^2} M(x,y) \right)$$
## @end tex
## @ifnottex
##
## @example
## @group
##       1    / d^2            d^2         \
## D  = --- * | ---  M(x,y) +  ---  M(x,y) |
##       4    \ dx^2           dy^2        /
## @end group
## @end example
##
## @end ifnottex
## For N-dimensional arrays the sum in parentheses is expanded to include second
## derivatives over the additional higher dimensions.
##
## The spacing between evaluation points may be defined by @var{h}, which is a
## scalar defining the equidistant spacing in all dimensions.  Alternatively,
## the spacing in each dimension may be defined separately by @var{dx},
## @var{dy}, etc.  A scalar spacing argument defines equidistant spacing,
## whereas a vector argument can be used to specify variable spacing.  The
## length of the spacing vectors must match the respective dimension of
## @var{M}.  The default spacing value is 1.
##
## At least 3 data points are needed for each dimension.  Boundary points are
## calculated from the linear extrapolation of interior points.
##
## @seealso{gradient, diff}
## @end deftypefn

## Author:  Kai Habel <kai.habel@gmx.de>

function D = del2 (M, varargin)

  if (nargin < 1)
    print_usage ();
  endif

  nd = ndims (M);
  sz = size (M);
  dx = cell (1, nd);
  if (nargin == 2 || nargin == 1)
    if (nargin == 1)
      h = 1;
    else
      h = varargin{1};
    endif
    for i = 1 : nd
      if (isscalar (h))
        dx{i} = h * ones (sz (i), 1);
      else
        if (length (h) == sz (i))
          dx{i} = diff (h)(:);
        else
          error ("del2: dimensionality mismatch in %d-th spacing vector", i);
        endif
      endif
    endfor
  elseif (nargin - 1 == nd)
    ## Reverse dx{1} and dx{2} as the X-dim is the 2nd dim of the ND array
    tmp = varargin{1};
    varargin{1} = varargin{2};
    varargin{2} = tmp;

    for i = 1 : nd
      if (isscalar (varargin{i}))
        dx{i} = varargin{i} * ones (sz (i), 1);
      else
        if (length (varargin{i}) == sz (i))
          dx{i} = diff (varargin{i})(:);
        else
          error ("del2: dimensionality mismatch in %d-th spacing vector", i);
        endif
      endif
    endfor
  else
    print_usage ();
  endif

  idx = cell (1, nd);
  for i = 1: nd
    idx{i} = ":";
  endfor

  D = zeros (sz);
  for i = 1: nd
    if (sz(i) >= 3)
      DD = zeros (sz);
      idx1 = idx2 = idx3 = idx;

      ## interior points
      idx1{i} = 1 : sz(i) - 2;
      idx2{i} = 2 : sz(i) - 1;
      idx3{i} = 3 : sz(i);
      szi = sz;
      szi (i) = 1;

      h1 = repmat (shiftdim (dx{i}(1 : sz(i) - 2), 1 - i), szi);
      h2 = repmat (shiftdim (dx{i}(2 : sz(i) - 1), 1 - i), szi);
      DD(idx2{:}) = ((M(idx1{:}) - M(idx2{:})) ./ h1 + ...
                     (M(idx3{:}) - M(idx2{:})) ./ h2) ./ (h1 + h2);

      ## left and right boundary
      if (sz(i) == 3)
        DD(idx1{:}) = DD(idx3{:}) = DD(idx2{:});
      else
        idx1{i} = 1;
        idx2{i} = 2;
        idx3{i} = 3;
        DD(idx1{:}) = (dx{i}(1) + dx{i}(2)) / dx{i}(2) * DD (idx2{:}) - ...
            dx{i}(1) / dx{i}(2) * DD (idx3{:});

        idx1{i} = sz(i);
        idx2{i} = sz(i) - 1;
        idx3{i} = sz(i) - 2;
        DD(idx1{:}) =  (dx{i}(sz(i) - 1) + dx{i}(sz(i) - 2)) / ...
            dx{i}(sz(i) - 2) * DD (idx2{:}) - ...
            dx{i}(sz(i) - 1) / dx{i}(sz(i) - 2) * DD (idx3{:});
      endif

      D += DD;
    endif
  endfor

  D = D ./ nd;
endfunction
