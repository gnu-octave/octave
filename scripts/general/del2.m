########################################################################
##
## Copyright (C) 2000-2023 The Octave Project Developers
##
## See the file COPYRIGHT.md in the top-level directory of this
## distribution or <https://octave.org/copyright/>.
##
## This file is part of Octave.
##
## Octave is free software: you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <https://www.gnu.org/licenses/>.
##
########################################################################

## -*- texinfo -*-
## @deftypefn  {} {@var{L} =} del2 (@var{M})
## @deftypefnx {} {@var{L} =} del2 (@var{M}, @var{h})
## @deftypefnx {} {@var{L} =} del2 (@var{M}, @var{dx}, @var{dy}, @dots{})
##
## Calculate the discrete Laplace
## @tex
## operator $( \nabla^2 )$.
## @end tex
## @ifnottex
## operator.
## @end ifnottex
##
## For a 2-dimensional matrix @var{M} this is defined as
## @tex
## $$L = {1 \over 4} \left( {d^2 \over dx^2} M(x,y) + {d^2 \over dy^2} M(x,y) \right)$$
## @end tex
## @ifnottex
##
## @example
## @group
##       1    / d^2            d^2         \
## L  = --- * | ---  M(x,y) +  ---  M(x,y) |
##       4    \ dx^2           dy^2        /
## @end group
## @end example
##
## @end ifnottex
## For N-dimensional arrays the sum in parentheses is expanded to include
## second derivatives over the additional higher dimensions.
##
## The spacing between evaluation points may be defined by @var{h}, which is a
## scalar defining the equidistant spacing in all dimensions.  Alternatively,
## the spacing in each dimension may be defined separately by @var{dx},
## @var{dy}, etc.  A scalar spacing argument defines equidistant spacing,
## whereas a vector argument can be used to specify variable spacing.  The
## length of the spacing vectors must match the respective dimension of
## @var{M}.  The default spacing value is 1.
##
## Dimensions with fewer than 3 data points are skipped.  Boundary points are
## calculated from the linear extrapolation of interior points.
##
## Example: Second derivative of 2*x^3
##
## @example
## @group
## f = @@(x) 2*x.^3;
## dd = @@(x) 12*x;
## x = 1:6;
## L = 4*del2 (f(x));
## assert (L, dd (x));
## @end group
## @end example
##
## @seealso{gradient, diff}
## @end deftypefn

function L = del2 (M, varargin)

  if (nargin < 1)
    print_usage ();
  endif

  nd = ndims (M);
  sz = size (M);
  dx = cell (1, nd);
  if (nargin == 1)
    for i = 1 : nd
      dx(i) = ones (sz(i), 1);
    endfor
  elseif (nargin == 2 && isscalar (varargin{1}))
    h = varargin{1};
    for i = 1 : nd
      dx(i) = h * ones (sz(i), 1);
    endfor
  elseif (numel (varargin) <= nd)
    ndx = numel (varargin);
    varargin(ndx+1:nd) = 1;   # Fill missing dims with 1.
    ## Reverse dx{1} and dx{2} as the X-dim is the 2nd dim of a meshgrid array
    varargin([1, 2]) = varargin([2, 1]);
    for i = 1 : nd
      arg = varargin{i};
      if (isscalar (arg))
        dx(i) = arg * ones (sz(i), 1);
      elseif (isvector (arg))
        if (length (arg) != sz(i))
          error ("del2: number of elements in spacing vector %d does not match dimension %d of M", i, i);
        endif
        dx(i) = diff (varargin{i})(:);
      else
        error ("del2: spacing element %d must be a scalar or vector", i);
      endif
    endfor
  else
    print_usage ();
  endif

  idx = cell (1, nd);
  idx(:) = ":";

  L = zeros (sz);
  for i = 1 : nd
    if (sz(i) >= 3)
      DD = zeros (sz);
      idx1 = idx2 = idx3 = idx;

      ## interior points
      idx1{i} = 1 : sz(i) - 2;
      idx2{i} = 2 : sz(i) - 1;
      idx3{i} = 3 : sz(i);
      szi = sz;
      szi(i) = 1;

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
        DD(idx1{:}) = (dx{i}(1) + dx{i}(2)) / dx{i}(2) * DD(idx2{:}) - ...
            dx{i}(1) / dx{i}(2) * DD(idx3{:});

        idx1{i} = sz(i);
        idx2{i} = sz(i) - 1;
        idx3{i} = sz(i) - 2;
        DD(idx1{:}) = (dx{i}(sz(i) - 1) + dx{i}(sz(i) - 2)) / ...
            dx{i}(sz(i) - 2) * DD(idx2{:}) - ...
            dx{i}(sz(i) - 1) / dx{i}(sz(i) - 2) * DD(idx3{:});
      endif

      L += DD;
    endif
  endfor

  L ./= nd;

endfunction


## 3x3 constant test
%!test
%! a = ones (3,3);
%! b = del2 (a);
%! assert (b(:,1), [0.00;0.00;0.00]);
%! assert (b(:,2), [0.00;0.00;0.00]);
%! assert (b(:,3), [0.00;0.00;0.00]);

## 3x3 planar test
%!test
%! a = [1,2,3;2,3,4;3,4,5];
%! b = del2 (a);
%! assert (b(:,1), [0.00;0.00;0.00]);
%! assert (b(:,2), [0.00;0.00;0.00]);
%! assert (b(:,3), [0.00;0.00;0.00]);

## 3x3 corner test
%!test
%! a = zeros (3,3);
%! a(1,1) = 1.0;
%! b = 2*del2 (a);
%! assert (b(:,1), [1.00;0.50;0.50]);
%! assert (b(:,2), [0.50;0.00;0.00]);
%! assert (b(:,3), [0.50;0.00;0.00]);
%! assert (b, flipud (2*del2 (flipud (a))));
%! assert (b, fliplr (2*del2 (fliplr (a))));
%! assert (b, flipud (fliplr (2*del2 (fliplr (flipud (a))))));

## 3x3 boundary test
%!test
%! a = zeros (3,3);
%! a(2,1)=1.0;
%! b = 2*del2 (a);
%! assert (b(:,1), [-1.00;-0.50;-1.00]);
%! assert (b(:,2), [0.00;0.50;0.00]);
%! assert (b(:,3), [0.00;0.50;0.00]);
%! assert (b, flipud (2*del2 (flipud (a))));
%! assert (b, fliplr (2*del2 (fliplr (a))));
%! assert (b, flipud (fliplr (2*del2 (fliplr (flipud (a))))));

## 3x3 center test
%!test
%! a = zeros (3,3);
%! a(2,2) = 1.0;
%! b = del2 (a);
%! assert (b(:,1), [0.00;-0.50;0.00]);
%! assert (b(:,2), [-0.50;-1.00;-0.50]);
%! assert (b(:,3), [0.00;-0.50;0.00]);

## 4x4 constant test
%!test
%! a = ones (4,4);
%! b = del2 (a);
%! assert (b(:,1), [0.00;0.00;0.00;0.00]);
%! assert (b(:,2), [0.00;0.00;0.00;0.00]);
%! assert (b(:,3), [0.00;0.00;0.00;0.00]);
%! assert (b(:,4), [0.00;0.00;0.00;0.00]);

## 4x4 planar test
%!test
%! a = [1,2,3,4;2,3,4,5;3,4,5,6;4,5,6,7];
%! b = del2 (a);
%! assert (b(:,1), [0.00;0.00;0.00;0.00]);
%! assert (b(:,2), [0.00;0.00;0.00;0.00]);
%! assert (b(:,3), [0.00;0.00;0.00;0.00]);
%! assert (b(:,4), [0.00;0.00;0.00;0.00]);

## 4x4 corner test
%!test
%! a = zeros (4,4);
%! a(1,1) = 1.0;
%! b = 2*del2 (a);
%! assert (b(:,1), [2.00;0.50;0.00;-0.50]);
%! assert (b(:,2), [0.50;0.00;0.00;0.00]);
%! assert (b(:,3), [0.00;0.00;0.00;0.00]);
%! assert (b(:,4), [-0.50;0.00;0.00;0.00]);
%! assert (b, flipud (2*del2 (flipud (a))));
%! assert (b, fliplr (2*del2 (fliplr (a))));
%! assert (b, flipud (fliplr (2*del2 (fliplr (flipud (a))))));

## 9x9 center test
%!test
%! a = zeros (9,9);
%! a(5,5) = 1.0;
%! b = 2*del2 (a);
%! assert (b(:,1), [0.00;0.00;0.00;0.00;0.00;0.00;0.00;0.00;0.00]);
%! assert (b(:,2), [0.00;0.00;0.00;0.00;0.00;0.00;0.00;0.00;0.00]);
%! assert (b(:,3), [0.00;0.00;0.00;0.00;0.00;0.00;0.00;0.00;0.00]);
%! assert (b(:,4), [0.00;0.00;0.00;0.00;0.50;0.00;0.00;0.00;0.00]);
%! assert (b(:,5), [0.00;0.00;0.00;0.50;-2.00;0.50;0.00;0.00;0.00]);
%! assert (b(:,6), b(:,4));
%! assert (b(:,7), b(:,3));
%! assert (b(:,8), b(:,2));
%! assert (b(:,9), b(:,1));

## 9x9 boundary test
%!test
%! a = zeros (9,9);
%! a(1,5) = 1.0;
%! b = 2*del2 (a);
%! assert (b(1,:), [0.00,0.00,0.00,0.50,0.00,0.50,0.00,0.00,0.00]);
%! assert (b(2,:), [0.00,0.00,0.00,0.00,0.50,0.00,0.00,0.00,0.00]);
%! assert (b(3:9,:), zeros (7,9));
%! a(1,5) = 0.0;
%! a(5,1) = 1.0;
%! b = 2*del2 (a);
%! assert (b(:,1), [0.00;0.00;0.00;0.50;0.00;0.50;0.00;0.00;0.00]);
%! assert (b(:,2), [0.00;0.00;0.00;0.00;0.50;0.00;0.00;0.00;0.00]);
%! assert (b(:,3:9), zeros (9,7));

## 9x9 dh center test
%!test
%! a = zeros (9,9);
%! a(5,5) = 1.0;
%! b = 8*del2 (a,2);
%! assert (b(:,1:3), zeros (9,3));
%! assert (b(:,4), [0.00;0.00;0.00;0.00;0.50;0.00;0.00;0.00;0.00]);
%! assert (b(:,5), [0.00;0.00;0.00;0.50;-2.00;0.50;0.00;0.00;0.00]);
%! assert (b(:,6), b(:,4));
%! assert (b(:,7:9), zeros (9,3));

## 9x9 dx test
%!test
%! a = zeros (9,9);
%! a(5,5) = 1.0;
%! b = 4*del2 (a,2,1);
%! assert (b(1:3,:), zeros (3,9));
%! assert (b(4,:), [0.00;0.00;0.00;0.00;1.00;0.00;0.00;0.00;0.00]');
%! assert (b(5,:), [0.00;0.00;0.00;0.25;-2.5;0.25;0.00;0.00;0.00]');
%! assert (b(6,:), b(4,:));
%! assert (b(7:9,:), zeros (3,9));

## 9x9 dy test
%!test
%! a = zeros (9,9);
%! a(5,5) = 1.0;
%! b = 4*del2 (a,1,2);
%! assert (b(:,1:3), zeros (9,3));
%! assert (b(:,4), [0.00;0.00;0.00;0.00;1.00;0.00;0.00;0.00;0.00]);
%! assert (b(:,5), [0.00;0.00;0.00;0.25;-2.5;0.25;0.00;0.00;0.00]);
%! assert (b(:,6), b(:,4));
%! assert (b(:,7:9), zeros (9,3));

## 3-D test
%!test
%! a = zeros (9,9,9);
%! a(5,5,5) = 1.0;
%! b = 8*3*del2 (a,2);
%! assert (b(:,:,1:3), zeros (9,9,3));
%! assert (b(:,1:3,:), zeros (9,3,9));
%! assert (b(1:3,:,:), zeros (3,9,9));
%! assert (b(4:5,4,4), [0.0,0.0]');
%! assert (b(5,5,4), 1.00);
%! assert (b(4,4,5), 0.00);
%! assert (b(5,4,5), 1.00);
%! assert (b(5,5,5),-6.00);
%! assert (b, flip (b,1));
%! assert (b, flip (b,2));
%! assert (b, flip (b,3));

%!test <*51728>
%! x = linspace (-2*pi, 2*pi);
%! U = cos (x);
%! L = 4*del2 (U, x);

## Test input validation
%!error <Invalid call> del2 ()
%!error <Invalid call> del2 (1, 1, 2, 3)
%!error <in spacing vector 1> del2 (1, 2, [1 1])
%!error <in spacing vector 2> del2 (1, [1 1], 2)
%!error <must be a scalar or vector> del2 (1, ones (2,2), 2)

