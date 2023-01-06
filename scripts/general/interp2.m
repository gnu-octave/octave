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
## @deftypefn  {} {@var{zi} =} interp2 (@var{x}, @var{y}, @var{z}, @var{xi}, @var{yi})
## @deftypefnx {} {@var{zi} =} interp2 (@var{z}, @var{xi}, @var{yi})
## @deftypefnx {} {@var{zi} =} interp2 (@var{z}, @var{n})
## @deftypefnx {} {@var{zi} =} interp2 (@var{z})
## @deftypefnx {} {@var{zi} =} interp2 (@dots{}, @var{method})
## @deftypefnx {} {@var{zi} =} interp2 (@dots{}, @var{method}, @var{extrap})
##
## Two-dimensional interpolation.
##
## Interpolate reference data @var{x}, @var{y}, @var{z} to determine @var{zi}
## at the coordinates @var{xi}, @var{yi}.  The reference data @var{x}, @var{y}
## can be matrices, as returned by @code{meshgrid}, in which case the sizes of
## @var{x}, @var{y}, and @var{z} must be equal.  If @var{x}, @var{y} are
## vectors describing a grid then @code{length (@var{x}) == columns (@var{z})}
## and @code{length (@var{y}) == rows (@var{z})}.  In either case the input
## data must be strictly monotonic.
##
## If called without @var{x}, @var{y}, and just a single reference data matrix
## @var{z}, the 2-D region
## @code{@var{x} = 1:columns (@var{z}), @var{y} = 1:rows (@var{z})} is assumed.
## This saves memory if the grid is regular and the distance between points is
## not important.
##
## If called with a single reference data matrix @var{z} and a refinement
## value @var{n}, then perform interpolation over a grid where each original
## interval has been recursively subdivided @var{n} times.  This results in
## @code{2^@var{n}-1} additional points for every interval in the original
## grid.  If @var{n} is omitted a value of 1 is used.  As an example, the
## interval [0,1] with @code{@var{n}==2} results in a refined interval with
## points at [0, 1/4, 1/2, 3/4, 1].
##
## The interpolation @var{method} is one of:
##
## @table @asis
## @item @qcode{"nearest"}
## Return the nearest neighbor.
##
## @item @qcode{"linear"} (default)
## Linear interpolation from nearest neighbors.
##
## @item @qcode{"pchip"}
## Piecewise cubic Hermite interpolating polynomial---shape-preserving
## interpolation with smooth first derivative.
##
## @item @qcode{"cubic"}
## Cubic interpolation using a convolution kernel function---third order
## method with smooth first derivative.
##
## @item @qcode{"spline"}
## Cubic spline interpolation---smooth first and second derivatives
## throughout the curve.
## @end table
##
## @var{extrap} is a scalar number.  It replaces values beyond the endpoints
## with @var{extrap}.  Note that if @var{extrap} is used, @var{method} must
## be specified as well.  If @var{extrap} is omitted and the @var{method} is
## @qcode{"spline"}, then the extrapolated values of the @qcode{"spline"} are
## used.  Otherwise the default @var{extrap} value for any other @var{method}
## is @qcode{"NA"}.
## @seealso{interp1, interp3, interpn, meshgrid}
## @end deftypefn

function ZI = interp2 (varargin)

  narginchk (1, 7);
  nargs = nargin;

  Z = X = Y = XI = YI = n = [];
  method = "linear";
  extrap = [];

  ## Check for method and extrap
  if (nargs > 1 && ischar (varargin{end-1}))
    if (! isnumeric (varargin{end}) || ! isscalar (varargin{end}))
      error ("interp2: EXTRAP must be a numeric scalar");
    endif
    extrap = varargin{end};
    method = varargin{end-1};
    nargs -= 2;
  elseif (ischar (varargin{end}))
    method = varargin{end};
    nargs -= 1;
  endif
  if (method(1) == "*")
    warning ("interp2: ignoring unsupported '*' flag to METHOD");
    method(1) = [];
  endif
  method = validatestring (method, ...
                           {"nearest", "linear", "pchip", "cubic", "spline"});

  ## Read numeric input
  switch (nargs)
    case 1
      Z = varargin{1};
      n = 1;
    case 2
      [Z, n] = deal (varargin{1:nargs});
    case 3
      [Z, XI, YI] = deal (varargin{1:nargs});
    case 5
      [X, Y, Z, XI, YI] = deal (varargin{1:nargs});
    otherwise
      print_usage ();
  endswitch

  ## Type checking
  if (! isnumeric (Z) || isscalar (Z) || ! ismatrix (Z))
    error ("interp2: Z must be a 2-D matrix");
  endif
  if (! isempty (n) && ! (isscalar (n) && n >= 0 && n == fix (n)))
    error ("interp2: N must be an integer >= 0");
  endif

  ## Define X, Y, XI, YI if needed
  [zr, zc] = size (Z);
  if (isempty (X))
    X = 1:zc;
    Y = 1:zr;
  endif
  if (! isnumeric (X) || ! isnumeric (Y))
    error ("interp2: X, Y must be numeric matrices");
  endif
  if (! isempty (n))
    ## Calculate the interleaved input vectors.
    p = 2^n;
    XI = (p:p*zc)/p;
    YI = (p:p*zr).'/p;
  endif
  if (! isnumeric (XI) || ! isnumeric (YI))
    error ("interp2: XI, YI must be numeric");
  endif

  if (isvector (X) && isvector (Y))
    X = X(:);  Y = Y(:);
  elseif (size_equal (X, Y))
    X = X(1,:).';  Y = Y(:,1);
  else
    error ("interp2: X and Y must be matrices of equal size");
  endif
  if (columns (Z) != length (X) || rows (Z) != length (Y))
    error ("interp2: X and Y size must match the dimensions of Z");
  endif
  dx = diff (X);
  if (all (dx < 0))
    X = flipud (X);
    Z = fliplr (Z);
  elseif (any (dx <= 0))
    error ("interp2: X must be strictly monotonic");
  endif
  dy = diff (Y);
  if (all (dy < 0))
    Y = flipud (Y);
    Z = flipud (Z);
  elseif (any (dy <= 0))
    error ("interp2: Y must be strictly monotonic");
  endif

  if (strcmp (method, "cubic") && (rows (Z) < 3 || columns (Z) < 3))
    warning (["interp2: cubic requires at least 3 points in each " ...
              "dimension.  Falling back to linear interpolation."]);
    method = "linear";
  endif

  if (any (strcmp (method, {"nearest", "linear", "pchip"})))

    ## If Xi and Yi are vectors of different orientation build a grid
    if ((isrow (XI) && iscolumn (YI)) || (iscolumn (XI) && isrow (YI)))
      [XI, YI] = meshgrid (XI, YI);
    elseif (! size_equal (XI, YI))
      error ("interp2: XI and YI must be matrices of equal size");
    endif

    ## if XI, YI are vectors, X and Y should share their orientation.
    if (isrow (XI))
      if (rows (X) != 1)
        X = X.';
      endif
      if (rows (Y) != 1)
        Y = Y.';
      endif
    elseif (iscolumn (XI))
      if (columns (X) != 1)
        X = X.';
      endif
      if (columns (Y) != 1)
        Y = Y.';
      endif
    endif

    xidx = lookup (X, XI, "lr");
    yidx = lookup (Y, YI, "lr");

    if (strcmp (method, "linear"))
      ## each quad satisfies the equation z(x,y)=a+b*x+c*y+d*xy
      ##
      ## a-b
      ## | |
      ## c-d
      a = Z(1:(zr - 1), 1:(zc - 1));
      b = Z(1:(zr - 1), 2:zc) - a;
      c = Z(2:zr, 1:(zc - 1)) - a;
      d = Z(2:zr, 2:zc) - a - b - c;

      ## scale XI, YI values to a 1-spaced grid
      Xsc = (XI - X(xidx)) ./ (diff (X)(xidx));
      Ysc = (YI - Y(yidx)) ./ (diff (Y)(yidx));

      ## Get 2-D index.
      idx = sub2ind (size (a), yidx, xidx);
      ## Dispose of the 1-D indices at this point to save memory.
      clear xidx yidx;

      ## Apply plane equation
      ## Handle case where idx and coefficients are both vectors and resulting
      ## coeff(idx) follows orientation of coeff, rather than that of idx.
      forient = @(x) reshape (x, size (idx));
      ZI =   forient (a(idx))        ...
           + forient (b(idx)) .* Xsc ...
           + forient (c(idx)) .* Ysc ...
           + forient (d(idx)) .* Xsc.*Ysc;

    elseif (strcmp (method, "nearest"))
      ii = (XI - X(xidx) >= X(xidx + 1) - XI);
      jj = (YI - Y(yidx) >= Y(yidx + 1) - YI);
      idx = sub2ind (size (Z), yidx+jj, xidx+ii);
      ZI = Z(idx);

    elseif (strcmp (method, "pchip"))

      if (length (X) < 2 || length (Y) < 2)
        error ("interp2: pchip requires at least 2 points in each dimension");
      endif

      ## first order derivatives
      DX = __pchip_deriv__ (X, Z, 2);
      DY = __pchip_deriv__ (Y, Z, 1);
      ## Compute mixed derivatives row-wise and column-wise.  Use the average.
      DXY = (__pchip_deriv__ (X, DY, 2) + __pchip_deriv__ (Y, DX, 1)) / 2;

      ## do the bicubic interpolation
      hx = diff (X); hx = hx(xidx);
      hy = diff (Y); hy = hy(yidx);

      tx = (XI - X(xidx)) ./ hx;
      ty = (YI - Y(yidx)) ./ hy;

      ## construct the cubic hermite base functions in x, y

      ## formulas:
      ## b{1,1} =    ( 2*t.^3 - 3*t.^2     + 1);
      ## b{2,1} = h.*(   t.^3 - 2*t.^2 + t    );
      ## b{1,2} =    (-2*t.^3 + 3*t.^2        );
      ## b{2,2} = h.*(   t.^3 -   t.^2        );

      ## optimized equivalents of the above:
      t1 = tx.^2;
      t2 = tx.*t1 - t1;
      xb{2,2} = hx.*t2;
      t1 = t2 - t1;
      xb{2,1} = hx.*(t1 + tx);
      t2 += t1;
      xb{1,2} = -t2;
      xb{1,1} = t2 + 1;

      t1 = ty.^2;
      t2 = ty.*t1 - t1;
      yb{2,2} = hy.*t2;
      t1 = t2 - t1;
      yb{2,1} = hy.*(t1 + ty);
      t2 += t1;
      yb{1,2} = -t2;
      yb{1,1} = t2 + 1;

      ZI = zeros (size (XI));
      for ix = 1:2
        for iy = 1:2
          zidx = sub2ind (size (Z), yidx+(iy-1), xidx+(ix-1));
          ZI += xb{1,ix} .* yb{1,iy} .*   Z(zidx);
          ZI += xb{2,ix} .* yb{1,iy} .*  DX(zidx);
          ZI += xb{1,ix} .* yb{2,iy} .*  DY(zidx);
          ZI += xb{2,ix} .* yb{2,iy} .* DXY(zidx);
        endfor
      endfor

    endif

  else  # cubic or spline methods

    ## Check dimensions of XI and YI
    if (isvector (XI) && isvector (YI) && ! size_equal (XI, YI))
      XI = XI(:).';  YI = YI(:);
    elseif (! size_equal (XI, YI))
      error ("interp2: XI and YI must be matrices of equal size");
    endif

    if (strcmp (method, "spline"))
      if (isgriddata (XI) && isgriddata (YI.'))
        ZI = __splinen__ ({Y, X}, Z, {YI(:,1), XI(1,:)}, extrap, "spline");
      else
        error ("interp2: XI, YI must have uniform spacing ('meshgrid' format)");
      endif
      return;  # spline doesn't use extrapolation value (MATLAB compatibility)
    elseif (strcmp (method, "cubic"))
      ## reduce to vectors if interpolation points are a meshgrid
      if (size_equal (XI, YI) && all (all (XI(1, :) == XI & YI(:, 1) == YI)))
        XI = XI(1, :);
        YI = YI(:, 1);
      endif

      ## make X a row vector
      X = X.';

      ## quadratic padding + additional zeros for the special case of copying
      ## the last line (like x=1:5, xi=5, requires to have indices 6 and 7)
      row_1 = 3*Z(1, :, :) - 3*Z(2, :, :) + Z(3, :, :);
      row_end = 3*Z(end, :, :) - 3*Z(end-1, :, :) + Z(end-2, :, :);
      ZI = [3*row_1(:, 1, :) - 3*row_1(:, 2, :) + row_1(:, 3, :), ...
            row_1, ...
            3*row_1(:, end, :) - 3*row_1(:, end-1, :) + row_1(:, end-2, :), ...
            0;
            #
            3*Z(:, 1, :) - 3*Z(:, 2, :) + Z(:, 3, :), ...
            Z, ...
            3*Z(:, end, :) - 3*Z(:, end-1, :) + Z(:, end-2, :), ...
            zeros(rows (Z), 1, size (Z, 3));
            #
            3*row_end(:, 1, :) - 3*row_end(:, 2, :) + row_end(:, 3, :), ...
            row_end, ...
            3*row_end(:, end, :) - 3*row_end(:, end-1, :) + row_end(:, end-2, :), ...
            0;
            zeros(1, columns (Z) + 3, size (Z, 3))];

      ## interpolate
      if (isrow (XI) && iscolumn (YI))
        ZI = conv_interp_vec (ZI, Y, YI, @cubic, [-2, 2], 1);
        ZI = conv_interp_vec (ZI, X, XI, @cubic, [-2, 2], 2);
      else
        ZI = conv_interp_pairs (ZI, X, Y, XI, YI, @cubic, [-2, 2]);
      endif
    endif
  endif

  ## extrapolation 'extrap'
  if (isempty (extrap))
    if (iscomplex (Z))
      extrap = complex (NA, NA);
    else
      extrap = NA;
    endif
  endif

  if (X(1) < X(end))
    if (Y(1) < Y(end))
      ZI(XI < X(1,1) | XI > X(end) | YI < Y(1,1) | YI > Y(end)) = extrap;
    else
      ZI(XI < X(1) | XI > X(end) | YI < Y(end) | YI > Y(1)) = extrap;
    endif
  else
    if (Y(1) < Y(end))
      ZI(XI < X(end) | XI > X(1) | YI < Y(1) | YI > Y(end)) = extrap;
    else
      ZI(XI < X(1,end) | XI > X(1) | YI < Y(end) | YI > Y(1)) = extrap;
    endif
  endif

endfunction

function b = isgriddata (X)
  d1 = diff (X, 1, 1);
  b = ! any (d1(:) != 0);
endfunction

## cubic convolution kernel with a = -0.5 for MATLAB compatibility.
function w = cubic (h)

  absh = abs (h);
  absh01 = absh <= 1;
  absh12 = absh <= 2 & ! absh01;
  absh_sqr = absh .* absh;
  absh_cube = absh_sqr .* absh;
  w = ...  # for |h| <= 1
      (1.5 * absh_cube - 2.5 * absh_sqr + 1) .* absh01 ...
      ...  # for 1 < |h| <= 2
      + (-0.5 * absh_cube + 2.5 * absh_sqr - 4 * absh + 2) .* absh12;

endfunction

## bicubic interpolation of full matrix in one direction with vector
function out = conv_interp_vec (Z, XY, XIYI, kernel, kernel_bounds, axis)

  ## allocate output
  out_shape = size (Z);
  out_shape(axis) = length (XIYI);
  out = zeros (out_shape);

  ## get indexes and distances h
  spread = abs (XY(1) - XY(2));
  idx = lookup (XY, XIYI, "l");
  h = (XIYI - XY(idx)) / spread;
  idx += -kernel_bounds(1) - 1;  # apply padding for indexes

  ## interpolate
  for shift = kernel_bounds(1)+1 : kernel_bounds(2)
    if (axis == 1)
      out += Z(idx + shift, :) .* kernel (shift - h);
    else
      out += Z(:, idx + shift) .* kernel (shift - h);
    endif
  endfor

endfunction

## bicubic interpolation of arbitrary XI-YI-pairs
function out = conv_interp_pairs (Z, X, Y, XI, YI, kernel, kernel_bounds)

  spread_x = abs (X(1, 1) - X(1, 2));
  spread_y = abs (Y(1, 1) - Y(2, 1));
  idx_x = lookup (X, XI, "l");
  idx_y = lookup (Y, YI, "l");
  h_x = (XI - reshape (X(idx_x), size (idx_x))) / spread_x;
  h_y = (YI - reshape (Y(idx_y), size (idx_y))) / spread_y;

  # adjust indexes for padding
  idx_x += -kernel_bounds(1) - 1;
  idx_y += -kernel_bounds(1) - 1;

  shifts = kernel_bounds(1)+1 : kernel_bounds(2);
  [SX(1,1,:,:), SY(1,1,:,:)] = meshgrid (shifts, shifts);
  pixels = Z(sub2ind (size (Z), idx_y + SY, idx_x + SX));
  kernel_y = kernel (reshape (shifts, 1, 1, [], 1) - h_y);
  kernel_x = kernel (reshape (shifts, 1, 1, 1, []) - h_x);
  out_x = sum (pixels .* kernel_x, 4);
  out = sum (out_x .* kernel_y, 3);

endfunction


%!demo
%! clf;
%! colormap ("default");
%! A = [13,-1,12;5,4,3;1,6,2];
%! x = [0,1,4];  y = [10,11,12];
%! xi = linspace (min (x), max (x), 17);
%! yi = linspace (min (y), max (y), 26).';
%! mesh (xi,yi,interp2 (x,y,A,xi,yi, "linear"));
%! [x,y] = meshgrid (x,y);
%! hold on; plot3 (x,y,A,"b*"); hold off;

%!demo
%! clf;
%! colormap ("default");
%! [x,y,A] = peaks (10);
%! x = x(1,:).';  y = y(:,1);
%! xi = linspace (min (x), max (x), 41);
%! yi = linspace (min (y), max (y), 41).';
%! mesh (xi,yi,interp2 (x,y,A,xi,yi, "linear"));
%! [x,y] = meshgrid (x,y);
%! hold on; plot3 (x,y,A,"b*"); hold off;

%!demo
%! clf;
%! colormap ("default");
%! A = [13,-1,12;5,4,3;1,6,2];
%! x = [0,1,4];  y = [10,11,12];
%! xi = linspace (min (x), max (x), 17);
%! yi = linspace (min (y), max (y), 26).';
%! mesh (xi,yi,interp2 (x,y,A,xi,yi, "nearest"));
%! [x,y] = meshgrid (x,y);
%! hold on; plot3 (x,y,A,"b*"); hold off;

%!demo
%! clf;
%! colormap ("default");
%! [x,y,A] = peaks (10);
%! x = x(1,:).';  y = y(:,1);
%! xi = linspace (min (x), max (x), 41);
%! yi = linspace (min (y), max (y), 41).';
%! mesh (xi,yi,interp2 (x,y,A,xi,yi, "nearest"));
%! [x,y] = meshgrid (x,y);
%! hold on; plot3 (x,y,A,"b*"); hold off;

%!demo
%! clf;
%! colormap ("default");
%! A = [13,-1,12;5,4,3;1,6,2];
%! x = [0,1,2];  y = [10,11,12];
%! xi = linspace (min (x), max (x), 17);
%! yi = linspace (min (y), max (y), 26).';
%! mesh (xi,yi,interp2 (x,y,A,xi,yi, "pchip"));
%! [x,y] = meshgrid (x,y);
%! hold on; plot3 (x,y,A,"b*"); hold off;

%!demo
%! clf;
%! colormap ("default");
%! [x,y,A] = peaks (10);
%! x = x(1,:).';  y = y(:,1);
%! xi = linspace (min (x), max (x), 41);
%! yi = linspace (min (y), max (y), 41).';
%! mesh (xi,yi,interp2 (x,y,A,xi,yi, "pchip"));
%! [x,y] = meshgrid (x,y);
%! hold on; plot3 (x,y,A,"b*"); hold off;

%!demo
%! clf;
%! colormap ("default");
%! A = [13,-1,12;5,4,3;1,6,2];
%! x = [0,1,2];  y = [10,11,12];
%! xi = linspace (min (x), max (x), 17);
%! yi = linspace (min (y), max (y), 26).';
%! mesh (xi,yi,interp2 (x,y,A,xi,yi, "cubic"));
%! [x,y] = meshgrid (x,y);
%! hold on; plot3 (x,y,A,"b*"); hold off;

%!demo
%! clf;
%! colormap ("default");
%! [x,y,A] = peaks (10);
%! x = x(1,:).';  y = y(:,1);
%! xi = linspace (min (x), max (x), 41);
%! yi = linspace (min (y), max (y), 41).';
%! mesh (xi,yi,interp2 (x,y,A,xi,yi, "cubic"));
%! [x,y] = meshgrid (x,y);
%! hold on; plot3 (x,y,A,"b*"); hold off;

%!demo
%! clf;
%! colormap ("default");
%! A = [13,-1,12;5,4,3;1,6,2];
%! x = [0,1,2];  y = [10,11,12];
%! xi = linspace (min (x), max (x), 17);
%! yi = linspace (min (y), max (y), 26).';
%! mesh (xi,yi,interp2 (x,y,A,xi,yi, "spline"));
%! [x,y] = meshgrid (x,y);
%! hold on; plot3 (x,y,A,"b*"); hold off;

%!demo
%! clf;
%! colormap ("default");
%! [x,y,A] = peaks (10);
%! x = x(1,:).';  y = y(:,1);
%! xi = linspace (min (x), max (x), 41);
%! yi = linspace (min (y), max (y), 41).';
%! mesh (xi,yi,interp2 (x,y,A,xi,yi, "spline"));
%! [x,y] = meshgrid (x,y);
%! hold on; plot3 (x,y,A,"b*"); hold off;

%!shared x, y, orig, xi, yi, expected
%!test  # simple test
%! x = [1,2,3];
%! y = [4,5,6,7];
%! [X, Y] = meshgrid (x, y);
%! orig = X.^2 + Y.^3;
%! xi = [0.8, 1.2, 2.0, 1.5];
%! yi = [6.2, 4.0, 5.0, 7.1].';
%!
%! # check nearest neighbor
%! expected = ...
%!   [NA, 217, 220, 220;
%!    NA,  65,  68,  68;
%!    NA, 126, 129, 129;
%!    NA,  NA,  NA,  NA];
%! result = interp2 (x, y, orig, xi, yi, "nearest");
%! assert (result, expected);
%!
%! # check invariance of translation
%! result = interp2 (x+3, y-7, orig, xi+3, yi-7, "nearest");
%! assert (result, expected);
%!
%! # check invariance of scaling
%! result = interp2 (x*3, y*(-7), orig, xi*3, yi*(-7), "nearest");
%! assert (result, expected);
%!
%! # check interpolation with index pairs
%! result = interp2 (x, y, orig, xi(2:4), yi(1:3).', "nearest");
%! assert (result, expected(sub2ind(size(expected), 1:3, 2:4)));
%!
%! # check bilinear interpolation
%! expected = ...
%!   [NA, 243,     245.4,   243.9;
%!    NA,  65.6,    68,      66.5;
%!    NA, 126.6,   129,     127.5;
%!    NA,  NA,      NA,      NA];
%! result = interp2 (x, y, orig, xi, yi);
%! assert (result, expected, 1000*eps);
%!
%! # check invariance of translation
%! result = interp2 (x+3, y-7, orig, xi+3, yi-7);
%! assert (result, expected, 1000*eps);
%!
%! # check invariance of scaling
%! result = interp2 (x*3, y*(-7), orig, xi*3, yi*(-7));
%! assert (result, expected, 1000*eps);
%!
%! # check interpolation with index pairs
%! result = interp2 (x, y, orig, xi(2:4), yi(1:3).');
%! assert (result, expected(sub2ind(size(expected), 1:3, 2:4)), 1000*eps);
%!
%! # check spline interpolation
%! expected = ...
%!   [238.968,  239.768,  242.328,  240.578;
%!     64.64,    65.44,    68,       66.25;
%!    125.64,   126.44,   129,      127.25;
%!    358.551,  359.351,  361.911,  360.161];
%! result = interp2 (x, y, orig, xi, yi, "spline");
%! assert (result, expected, 1000*eps);
%!
%! # check invariance of translation
%! result = interp2 (x+3, y-7, orig, xi+3, yi-7, "spline");
%! assert (result, expected, 1000*eps);
%!
%! # check invariance of scaling
%! result = interp2 (x*3, y*(-7), orig, xi*3, yi*(-7), "spline");
%! assert (result, expected, 1000*eps);
%!
%!test <62133>
%! # FIXME: spline interpolation does not support index pairs, Matlab does.
%! result = interp2 (x, y, orig, xi(2:4), yi(1:3).', "spline");
%! assert (result, expected(sub2ind(size(expected), 1:3, 2:4)), 1000*eps);
%!
%!test <*61754>
%! # check bicubic interpolation
%! expected = ...
%!   [NA, 239.96,  242.52,  240.77;
%!    NA,  65.44,   68,      66.25;
%!    NA, 126.44,  129,     127.25;
%!    NA,  NA,      NA,      NA];
%! result = interp2 (x, y, orig, xi, yi, "cubic");
%! assert (result, expected, 10000*eps);
%!
%! # check invariance of translation
%! result = interp2 (x+3, y-7, orig, xi+3, yi-7, "cubic");
%! assert (result, expected, 10000*eps);
%!
%! # check invariance of scaling
%! result = interp2 (x*3, y*(-7), orig, xi*3, yi*(-7), "cubic");
%! assert (result, expected, 10000*eps);
%!
%! # check interpolation with index pairs
%! result = interp2 (x, y, orig, xi(2:4), yi(1:3).', "cubic");
%! assert (result, expected(sub2ind(size(expected), 1:3, 2:4)), 10000*eps);

## Test that interpolating a complex matrix is equivalent to interpolating its
## real and imaginary parts separately.
%!test <*61863>
%! xi = [2.5, 3.5];
%! yi = [0.5, 1.5].';
%! orig = rand (4, 3) + 1i * rand (4, 3);
%! for method = {"nearest", "linear", "pchip", "cubic", "spline"}
%!   interp_complex = interp2 (orig, xi, yi, method{1});
%!   interp_real = interp2 (real (orig), xi, yi, method{1});
%!   interp_imag = interp2 (imag (orig), xi, yi, method{1});
%!   assert (real (interp_complex), interp_real);
%!   assert (imag (interp_complex), interp_imag);
%! endfor

%!test  # 2^n refinement form
%! x = [1,2,3];
%! y = [4,5,6,7];
%! [X, Y] = meshgrid (x, y);
%! orig = X.^2 + Y.^3;
%! xi = [1:0.25:3];  yi = [4:0.25:7].';
%! expected = interp2 (x,y,orig, xi, yi);
%! result = interp2 (orig, 2);
%!
%! assert (result, expected, 10*eps);

%!test  # matrix slice
%! A = eye (4);
%! assert (interp2 (A,[1:4],[1:4]), [1,1,1,1]);

%!test  # non-gridded XI,YI
%! A = eye (4);
%! assert (interp2 (A,[1,2;3,4],[1,3;2,4]), [1,0;0,1]);

%!test  # for values outside of boundaries
%! x = [1,2,3];
%! y = [4,5,6,7];
%! [X, Y] = meshgrid (x,y);
%! orig = X.^2 + Y.^3;
%! xi = [0,4];
%! yi = [3,8].';
%! assert (interp2 (x,y,orig, xi, yi), [NA,NA;NA,NA]);
%! assert (interp2 (x,y,orig, xi, yi,"linear", 0), [0,0;0,0]);
%! assert (interp2 (x,y,orig, xi, yi,"linear", 2), [2,2;2,2]);
%! assert (interp2 (x,y,orig, xi, yi,"spline", 2), [2,2;2,2]);
%! assert (interp2 (x,y,orig, xi, yi,"linear", 0+1i), [0+1i,0+1i;0+1i,0+1i]);
%! assert (interp2 (x,y,orig, xi, yi,"spline"), [27,43;512,528]);
%! assert (interp2 (x,y,orig, xi, yi,"cubic"), [NA,NA;NA,NA]);
%! assert (interp2 (x,y,orig, xi, yi,"cubic", 2), [2,2;2,2]);

%!test  # for values at boundaries
%! A = [1,2;3,4];
%! x = [0,1];
%! y = [2,3].';
%! assert (interp2 (x,y,A,x,y,"linear"), A);
%! assert (interp2 (x,y,A,x,y,"nearest"), A);

%!test  # for Matlab-compatible rounding for 'nearest'
%! X = meshgrid (1:4);
%! assert (interp2 (X, 2.5, 2.5, "nearest"), 3);

## re-order monotonically decreasing
%!assert <*41838> (interp2 ([1 2 3], [3 2 1], magic (3), 2.5, 3), 3.5)
%!assert <*41838> (interp2 ([3 2 1], [1 2 3], magic (3), 1.5, 1), 3.5)

## Linear interpretation with vector XI doesn't lead to matrix output
%!assert <*49506> (interp2 ([2 3], [2 3 4], [1 2; 3 4; 5 6], [2 3], 3, "linear"), [3 4])

%!shared z, zout, tol
%! z = [1 3 5; 3 5 7; 5 7 9];
%! zout = [1 2 3 4 5; 2 3 4 5 6; 3 4 5 6 7; 4 5 6 7 8; 5 6 7 8 9];
%! tol = 2 * eps;
%!
%!assert (interp2 (z), zout, tol)
%!assert (interp2 (z, "linear"), zout, tol)
%!assert (interp2 (z, "pchip"), zout, tol)
%!assert (interp2 (z, "cubic"), zout, tol)
%!assert (interp2 (z, "spline"), zout, tol)
%!assert (interp2 (z, [2 3 1], [2 2 2].', "linear"),
%!        repmat ([5, 7, 3], [3, 1]), tol)
%!assert (interp2 (z, [2 3 1], [2 2 2].', "pchip"),
%!        repmat ([5, 7, 3], [3, 1]), tol)
%!assert (interp2 (z, [2 3 1], [2 2 2].', "cubic"),
%!        repmat ([5, 7, 3], [3, 1]), tol)
%!assert (interp2 (z, [2 3 1], [2 2 2].', "spline"),
%!        repmat ([5, 7, 3], [3, 1]), tol)
%!assert (interp2 (z, [2 3 1], [2 2 2], "linear"), [5 7 3], tol)
%!assert (interp2 (z, [2 3 1], [2 2 2], "pchip"), [5 7 3], tol)
%!assert (interp2 (z, [2 3 1], [2 2 2], "cubic"), [5 7 3], tol)
%!assert (interp2 (z, [2 3 1], [2 2 2], "spline"), [5 7 3], tol)
%!assert (interp2 (z, [3; 3; 3], [2; 3; 1], "linear"), [7; 9; 5], tol)
%!assert (interp2 (z, [3; 3; 3], [2; 3; 1], "pchip"), [7; 9; 5], tol)
%!assert (interp2 (z, [3; 3; 3], [2; 3; 1], "cubic"), [7; 9; 5], tol)
%!test <62132>
%! # FIXME: single column yields single row with spline interpolation (numbers are correct)
%! assert (interp2 (z, [3; 3; 3], [2; 3; 1], "spline"), [7; 9; 5], tol);

## Test input validation
%!error interp2 (1, 1, 1, 1, 1, 2)    # only 5 numeric inputs
%!error interp2 (1, 1, 1, 1, 1, 2, 2) # only 5 numeric inputs
%!error <Z must be a 2-D matrix> interp2 ({1})
%!error <Z must be a 2-D matrix> interp2 (1,1,1)
%!error <Z must be a 2-D matrix> interp2 (ones (2,2,2))
%!error <N must be an integer .= 0> interp2 (ones (2), ones (2))
%!error <N must be an integer .= 0> interp2 (ones (2), -1)
%!error <N must be an integer .= 0> interp2 (ones (2), 1.5)
%!warning <ignoring unsupported '\*' flag> interp2 (rand (3,3), 1, "*linear");
%!error <EXTRAP must be a numeric scalar> interp2 (1, 1, 1, 1, 1, "linear", {1})
%!error <EXTRAP must be a numeric scalar> interp2 (1, 1, 1, 1, 1, "linear", ones (2,2))
%!error <EXTRAP must be a numeric scalar> interp2 (1, 1, 1, 1, 1, "linear", "abc")
%!error <EXTRAP must be a numeric scalar> interp2 (1, 1, 1, 1, 1, "linear", "extrap")
%!error <X, Y must be numeric matrices> interp2 ({1}, 1, ones (2), 1, 1)
%!error <X, Y must be numeric matrices> interp2 (1, {1}, ones (2), 1, 1)
%!error <XI, YI must be numeric> interp2 (1, 1, ones (2), {1}, 1)
%!error <XI, YI must be numeric> interp2 (1, 1, ones (2), 1, {1})
%!error <X and Y must be matrices of equal size> interp2 (ones (2,2), 1, ones (2), 1, 1)
%!error <X and Y must be matrices of equal size> interp2 (ones (2,2), ones (2,3), ones (2), 1, 1)
%!error <X and Y size must match the dimensions of Z> interp2 (1:3, 1:3, ones (3,2), 1, 1)
%!error <X and Y size must match the dimensions of Z> interp2 (1:2, 1:2, ones (3,2), 1, 1)
%!error <X must be strictly monotonic> interp2 ([1 0 2], 1:3, ones (3,3), 1, 1)
%!error <Y must be strictly monotonic> interp2 (1:3, [1 0 2], ones (3,3), 1, 1)
%!warning <cubic requires at least 3 points in each dimension.> interp2 (eye(2), 1.5, 1.5, "cubic");
%!error <XI and YI must be matrices of equal size> interp2 (1:2, 1:2, ones (2), ones (2,2), 1)
%!error <XI and YI must be matrices of equal size> interp2 (1:2, 1:2, ones (2), 1, ones (2,2))
%!error <XI, YI must have uniform spacing> interp2 (1:2, 1:2, ones (2), [1 2 4], [1 2 3], "spline")
%!error <XI, YI must have uniform spacing> interp2 (1:2, 1:2, ones (2), [1 2 3], [1 2 4], "spline")
%!error interp2 (1, 1, 1, 1, 1, "foobar")
