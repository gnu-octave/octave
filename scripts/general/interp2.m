## Copyright (C) 2000-2012 Kai Habel
## Copyright (C) 2009 Jaroslav Hajek
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
## @deftypefn  {Function File} {@var{zi} =} interp2 (@var{x}, @var{y}, @var{z}, @var{xi}, @var{yi})
## @deftypefnx {Function File} {@var{zi} =} interp2 (@var{Z}, @var{xi}, @var{yi})
## @deftypefnx {Function File} {@var{zi} =} interp2 (@var{Z}, @var{n})
## @deftypefnx {Function File} {@var{zi} =} interp2 (@dots{}, @var{method})
## @deftypefnx {Function File} {@var{zi} =} interp2 (@dots{}, @var{method}, @var{extrapval})
##
## Two-dimensional interpolation.  @var{x}, @var{y} and @var{z} describe a
## surface function.  If @var{x} and @var{y} are vectors their length
## must correspondent to the size of @var{z}.  @var{x} and @var{y} must be
## monotonic.  If they are matrices they must have the @code{meshgrid}
## format.
##
## @table @code
## @item interp2 (@var{x}, @var{y}, @var{Z}, @var{xi}, @var{yi}, @dots{})
## Returns a matrix corresponding to the points described by the
## matrices @var{xi}, @var{yi}.
##
## If the last argument is a string, the interpolation method can
## be specified.  The method can be 'linear', 'nearest' or 'cubic'.
## If it is omitted 'linear' interpolation is assumed.
##
## @item interp2 (@var{z}, @var{xi}, @var{yi})
## Assumes @code{@var{x} = 1:rows (@var{z})} and @code{@var{y} =
## 1:columns (@var{z})}
##
## @item interp2 (@var{z}, @var{n})
## Interleaves the matrix @var{z} n-times.  If @var{n} is omitted a value
## of @code{@var{n} = 1} is assumed.
## @end table
##
## The variable @var{method} defines the method to use for the
## interpolation.  It can take one of the following values
##
## @table @asis
## @item 'nearest'
## Return the nearest neighbor.
##
## @item 'linear'
## Linear interpolation from nearest neighbors.
##
## @item 'pchip'
## Piecewise cubic Hermite interpolating polynomial.
##
## @item 'cubic'
## Cubic interpolation from four nearest neighbors.
##
## @item 'spline'
## Cubic spline interpolation---smooth first and second derivatives
## throughout the curve.
## @end table
##
## If a scalar value @var{extrapval} is defined as the final value, then
## values outside the mesh as set to this value.  Note that in this case
## @var{method} must be defined as well.  If @var{extrapval} is not
## defined then NA is assumed.
##
## @seealso{interp1}
## @end deftypefn

## Author:      Kai Habel <kai.habel@gmx.de>
## 2005-03-02 Thomas Weber <weber@num.uni-sb.de>
##     * Add test cases
## 2005-03-02 Paul Kienzle <pkienzle@users.sf.net>
##     * Simplify
## 2005-04-23 Dmitri A. Sergatskov <dasergatskov@gmail.com>
##     * Modified demo and test for new gnuplot interface
## 2005-09-07 Hoxide <hoxide_dirac@yahoo.com.cn>
##     * Add bicubic interpolation method
##     * Fix the eat line bug when the last element of XI or YI is
##       negative or zero.
## 2005-11-26 Pierre Baldensperger <balden@libertysurf.fr>
##     * Rather big modification (XI,YI no longer need to be
##       "meshgridded") to be consistent with the help message
##       above and for compatibility.

function ZI = interp2 (varargin)
  Z = X = Y = XI = YI = n = [];
  method = "linear";
  extrapval = NA;

  switch (nargin)
    case 1
      Z = varargin{1};
      n = 1;
    case 2
      if (ischar (varargin{2}))
        [Z, method] = deal (varargin{:});
        n = 1;
      else
        [Z, n] = deal (varargin{:});
      endif
    case 3
      if (ischar (varargin{3}))
        [Z, n, method] = deal (varargin{:});
      else
        [Z, XI, YI] = deal (varargin{:});
      endif
    case 4
      if (ischar (varargin{4}))
        [Z, XI, YI, method] = deal (varargin{:});
      else
        [Z, n, method, extrapval] = deal (varargin{:});
      endif
    case 5
      if (ischar (varargin{4}))
        [Z, XI, YI, method, extrapval] = deal (varargin{:});
      else
        [X, Y, Z, XI, YI] = deal (varargin{:});
      endif
    case 6
        [X, Y, Z, XI, YI, method] = deal (varargin{:});
    case 7
        [X, Y, Z, XI, YI, method, extrapval] = deal (varargin{:});
    otherwise
      print_usage ();
  endswitch

  ## Type checking.
  if (!ismatrix (Z))
    error ("interp2: Z must be a matrix");
  endif
  if (!isempty (n) && !isscalar (n))
    error ("interp2: N must be a scalar");
  endif
  if (!ischar (method))
    error ("interp2: METHOD must be a string");
  endif
  if (ischar (extrapval) || strcmp (extrapval, "extrap"))
    extrapval = [];
  elseif (!isscalar (extrapval))
    error ("interp2: EXTRAPVAL must be a scalar");
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
    YI = (p:p*zr)'/p;
  endif
  if (! isnumeric (XI) || ! isnumeric (YI))
    error ("interp2: XI, YI must be numeric");
  endif


  if (strcmp (method, "linear") || strcmp (method, "nearest") ...
      || strcmp (method, "pchip"))

    ## If X and Y vectors produce a grid from them
    if (isvector (X) && isvector (Y))
      X = X(:); Y = Y(:);
    elseif (size_equal (X, Y))
      X = X(1,:)'; Y = Y(:,1);
    else
      error ("interp2: X and Y must be matrices of same size");
    endif
    if (columns (Z) != length (X) || rows (Z) != length (Y))
      error ("interp2: X and Y size must match the dimensions of Z");
    endif

    ## If Xi and Yi are vectors of different orientation build a grid
    if ((rows (XI) == 1 && columns (YI) == 1)
        || (columns (XI) == 1 && rows (YI) == 1))
      [XI, YI] = meshgrid (XI, YI);
    elseif (! size_equal (XI, YI))
      error ("interp2: XI and YI must be matrices of equal size");
    endif

    ## if XI, YI are vectors, X and Y should share their orientation.
    if (rows (XI) == 1)
      if (rows (X) != 1)
        X = X.';
      endif
      if (rows (Y) != 1)
        Y = Y.';
      endif
    elseif (columns (XI) == 1)
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

      ## Get 2D index.
      idx = sub2ind (size (a), yidx, xidx);
      ## We can dispose of the 1D indices at this point to save memory.
      clear xidx yidx;

      ## apply plane equation
      ZI = a(idx) + b(idx).*Xsc + c(idx).*Ysc + d(idx).*Xsc.*Ysc;

    elseif (strcmp (method, "nearest"))
      ii = (XI - X(xidx) >= X(xidx + 1) - XI);
      jj = (YI - Y(yidx) >= Y(yidx + 1) - YI);
      idx = sub2ind (size (Z), yidx+jj, xidx+ii);
      ZI = Z(idx);

    elseif (strcmp (method, "pchip"))

      if (length (X) < 2 || length (Y) < 2)
        error ("interp2: pchip2 requires at least 2 points in each dimension");
      endif

      ## first order derivatives
      DX = __pchip_deriv__ (X, Z, 2);
      DY = __pchip_deriv__ (Y, Z, 1);
      ## Compute mixed derivatives row-wise and column-wise, use the average.
      DXY = (__pchip_deriv__ (X, DY, 2) + __pchip_deriv__ (Y, DX, 1))/2;

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
      for i = 1:2
        for j = 1:2
          zidx = sub2ind (size (Z), yidx+(j-1), xidx+(i-1));
          ZI += xb{1,i} .* yb{1,j} .*   Z(zidx);
          ZI += xb{2,i} .* yb{1,j} .*  DX(zidx);
          ZI += xb{1,i} .* yb{2,j} .*  DY(zidx);
          ZI += xb{2,i} .* yb{2,j} .* DXY(zidx);
        endfor
      endfor

    endif

    if (! isempty (extrapval))
      ## set points outside the table to 'extrapval'
      if (X (1) < X (end))
        if (Y (1) < Y (end))
          ZI (XI < X(1,1) | XI > X(end) | YI < Y(1,1) | YI > Y(end)) = ...
                  extrapval;
        else
          ZI (XI < X(1) | XI > X(end) | YI < Y(end) | YI > Y(1)) = ...
                  extrapval;
        endif
      else
        if (Y (1) < Y (end))
          ZI (XI < X(end) | XI > X(1) | YI < Y(1) | YI > Y(end)) = ...
                  extrapval;
        else
          ZI (XI < X(1,end) | XI > X(1) | YI < Y(end) | YI > Y(1)) = ...
                  extrapval;
        endif
      endif
    endif

  else

    ## Check dimensions of X and Y
    if (isvector (X) && isvector (Y))
      X = X(:).';
      Y = Y(:);
      if (!isequal ([length(Y), length(X)], size(Z)))
        error ("interp2: X and Y size must match the dimensions of Z");
      endif
    elseif (!size_equal (X, Y))
      error ("interp2: X and Y must be matrices of equal size");
      if (! size_equal (X, Z))
        error ("interp2: X and Y size must match the dimensions of Z");
      endif
    endif

    ## Check dimensions of XI and YI
    if (isvector (XI) && isvector (YI) && ! size_equal (XI, YI))
      XI = XI(:).';
      YI = YI(:);
      [XI, YI] = meshgrid (XI, YI);
    elseif (! size_equal (XI, YI))
      error ("interp2: XI and YI must be matrices of equal size");
    endif

    if (strcmp (method, "cubic"))
      if (isgriddata (XI) && isgriddata (YI'))
        ZI = bicubic (X, Y, Z, XI (1, :), YI (:, 1), extrapval);
      elseif (isgriddata (X) && isgriddata (Y'))
        ## Allocate output
        ZI = zeros (size (X));

        ## Find inliers
        inside = !(XI < X (1) | XI > X (end) | YI < Y (1) | YI > Y (end));

        ## Scale XI and YI to match indices of Z
        XI = (columns (Z) - 1) * (XI - X (1)) / (X (end) - X (1)) + 1;
        YI = (rows (Z) - 1) * (YI - Y (1)) / (Y (end) - Y (1)) + 1;

        ## Start the real work
        K = floor (XI);
        L = floor (YI);

        ## Coefficients
        AY1  = bc ((YI - L + 1));
        AX1  = bc ((XI - K + 1));
        AY0  = bc ((YI - L + 0));
        AX0  = bc ((XI - K + 0));
        AY_1 = bc ((YI - L - 1));
        AX_1 = bc ((XI - K - 1));
        AY_2 = bc ((YI - L - 2));
        AX_2 = bc ((XI - K - 2));

        ## Perform interpolation
        sz = size(Z);
        ZI = AY_2 .* AX_2 .* Z (sym_sub2ind (sz, L+2, K+2)) ...
           + AY_2 .* AX_1 .* Z (sym_sub2ind (sz, L+2, K+1)) ...
           + AY_2 .* AX0  .* Z (sym_sub2ind (sz, L+2, K))   ...
           + AY_2 .* AX1  .* Z (sym_sub2ind (sz, L+2, K-1)) ...
           + AY_1 .* AX_2 .* Z (sym_sub2ind (sz, L+1, K+2)) ...
           + AY_1 .* AX_1 .* Z (sym_sub2ind (sz, L+1, K+1)) ...
           + AY_1 .* AX0  .* Z (sym_sub2ind (sz, L+1, K))   ...
           + AY_1 .* AX1  .* Z (sym_sub2ind (sz, L+1, K-1)) ...
           + AY0  .* AX_2 .* Z (sym_sub2ind (sz, L,   K+2)) ...
           + AY0  .* AX_1 .* Z (sym_sub2ind (sz, L,   K+1)) ...
           + AY0  .* AX0  .* Z (sym_sub2ind (sz, L,   K))   ...
           + AY0  .* AX1  .* Z (sym_sub2ind (sz, L,   K-1)) ...
           + AY1  .* AX_2 .* Z (sym_sub2ind (sz, L-1, K+2)) ...
           + AY1  .* AX_1 .* Z (sym_sub2ind (sz, L-1, K+1)) ...
           + AY1  .* AX0  .* Z (sym_sub2ind (sz, L-1, K))   ...
           + AY1  .* AX1  .* Z (sym_sub2ind (sz, L-1, K-1));
        ZI (!inside) = extrapval;

      else
        error ("interp2: input data must have 'meshgrid' format");
      endif

    elseif (strcmp (method, "spline"))
      if (isgriddata (XI) && isgriddata (YI'))
        ZI = __splinen__ ({Y(:,1).', X(1,:)}, Z, {YI(:,1), XI(1,:)}, extrapval,
                        "spline");
      else
        error ("interp2: input data must have 'meshgrid' format");
      endif
    else
      error ("interp2: interpolation METHOD not recognized");
    endif

  endif
endfunction

function b = isgriddata (X)
  d1 = diff (X, 1, 1);
  b = all (d1 (:) == 0);
endfunction

## Compute the bicubic interpolation coefficients
function o = bc(x)
  x = abs(x);
  o = zeros(size(x));
  idx1 = (x < 1);
  idx2 = !idx1 & (x < 2);
  o(idx1) = 1 - 2.*x(idx1).^2 + x(idx1).^3;
  o(idx2) = 4 - 8.*x(idx2) + 5.*x(idx2).^2 - x(idx2).^3;
endfunction

## This version of sub2ind behaves as if the data was symmetrically padded
function ind = sym_sub2ind(sz, Y, X)
  Y (Y < 1) = 1 - Y (Y < 1);
  while (any (Y (:) > 2 * sz (1)))
    Y (Y > 2 * sz (1)) = round (Y (Y > 2 * sz (1)) / 2);
  endwhile
  Y (Y > sz (1)) = 1 + 2 * sz (1) - Y (Y > sz (1));
  X (X < 1) = 1 - X (X < 1);
  while (any (X (:) > 2 * sz (2)))
    X (X > 2 * sz (2)) = round (X (X > 2 * sz (2)) / 2);
  endwhile
  X (X > sz (2)) = 1 + 2 * sz (2) - X (X > sz (2));
  ind = sub2ind(sz, Y, X);
endfunction


%!demo
%! A=[13,-1,12;5,4,3;1,6,2];
%! x=[0,1,4]; y=[10,11,12];
%! xi=linspace(min(x),max(x),17);
%! yi=linspace(min(y),max(y),26)';
%! mesh(xi,yi,interp2(x,y,A,xi,yi,'linear'));
%! [x,y] = meshgrid(x,y);
%! hold on; plot3(x(:),y(:),A(:),"b*"); hold off;

%!demo
%! [x,y,A] = peaks(10);
%! x = x(1,:)'; y = y(:,1);
%! xi=linspace(min(x),max(x),41);
%! yi=linspace(min(y),max(y),41)';
%! mesh(xi,yi,interp2(x,y,A,xi,yi,'linear'));
%! [x,y] = meshgrid(x,y);
%! hold on; plot3(x(:),y(:),A(:),"b*"); hold off;

%!demo
%! A=[13,-1,12;5,4,3;1,6,2];
%! x=[0,1,4]; y=[10,11,12];
%! xi=linspace(min(x),max(x),17);
%! yi=linspace(min(y),max(y),26)';
%! mesh(xi,yi,interp2(x,y,A,xi,yi,'nearest'));
%! [x,y] = meshgrid(x,y);
%! hold on; plot3(x(:),y(:),A(:),"b*"); hold off;

%!demo
%! [x,y,A] = peaks(10);
%! x = x(1,:)'; y = y(:,1);
%! xi=linspace(min(x),max(x),41);
%! yi=linspace(min(y),max(y),41)';
%! mesh(xi,yi,interp2(x,y,A,xi,yi,'nearest'));
%! [x,y] = meshgrid(x,y);
%! hold on; plot3(x(:),y(:),A(:),"b*"); hold off;

%!demo
%! A=[13,-1,12;5,4,3;1,6,2];
%! x=[0,1,2]; y=[10,11,12];
%! xi=linspace(min(x),max(x),17);
%! yi=linspace(min(y),max(y),26)';
%! mesh(xi,yi,interp2(x,y,A,xi,yi,'pchip'));
%! [x,y] = meshgrid(x,y);
%! hold on; plot3(x(:),y(:),A(:),"b*"); hold off;

%!demo
%! [x,y,A] = peaks(10);
%! x = x(1,:)'; y = y(:,1);
%! xi=linspace(min(x),max(x),41);
%! yi=linspace(min(y),max(y),41)';
%! mesh(xi,yi,interp2(x,y,A,xi,yi,'pchip'));
%! [x,y] = meshgrid(x,y);
%! hold on; plot3(x(:),y(:),A(:),"b*"); hold off;

%!demo
%! A=[13,-1,12;5,4,3;1,6,2];
%! x=[0,1,2]; y=[10,11,12];
%! xi=linspace(min(x),max(x),17);
%! yi=linspace(min(y),max(y),26)';
%! mesh(xi,yi,interp2(x,y,A,xi,yi,'cubic'));
%! [x,y] = meshgrid(x,y);
%! hold on; plot3(x(:),y(:),A(:),"b*"); hold off;

%!demo
%! [x,y,A] = peaks(10);
%! x = x(1,:)'; y = y(:,1);
%! xi=linspace(min(x),max(x),41);
%! yi=linspace(min(y),max(y),41)';
%! mesh(xi,yi,interp2(x,y,A,xi,yi,'cubic'));
%! [x,y] = meshgrid(x,y);
%! hold on; plot3(x(:),y(:),A(:),"b*"); hold off;

%!demo
%! A=[13,-1,12;5,4,3;1,6,2];
%! x=[0,1,2]; y=[10,11,12];
%! xi=linspace(min(x),max(x),17);
%! yi=linspace(min(y),max(y),26)';
%! mesh(xi,yi,interp2(x,y,A,xi,yi,'spline'));
%! [x,y] = meshgrid(x,y);
%! hold on; plot3(x(:),y(:),A(:),"b*"); hold off;

%!demo
%! [x,y,A] = peaks(10);
%! x = x(1,:)'; y = y(:,1);
%! xi=linspace(min(x),max(x),41);
%! yi=linspace(min(y),max(y),41)';
%! mesh(xi,yi,interp2(x,y,A,xi,yi,'spline'));
%! [x,y] = meshgrid(x,y);
%! hold on; plot3(x(:),y(:),A(:),"b*"); hold off;

%!test % simple test
%!  x = [1,2,3];
%!  y = [4,5,6,7];
%!  [X, Y] = meshgrid(x,y);
%!  Orig = X.^2 + Y.^3;
%!  xi = [1.2,2, 1.5];
%!  yi = [6.2, 4.0, 5.0]';
%!
%!  Expected = ...
%!    [243,   245.4,  243.9;
%!      65.6,  68,     66.5;
%!     126.6, 129,    127.5];
%!  Result = interp2(x,y,Orig, xi, yi);
%!
%!  assert(Result, Expected, 1000*eps);

%!test % 2^n form
%!  x = [1,2,3];
%!  y = [4,5,6,7];
%!  [X, Y] = meshgrid(x,y);
%!  Orig = X.^2 + Y.^3;
%!  xi = [1:0.25:3]; yi = [4:0.25:7]';
%!  Expected = interp2(x,y,Orig, xi, yi);
%!  Result = interp2(Orig,2);
%!
%!  assert(Result, Expected, 10*eps);

%!test % matrix slice
%!  A = eye(4);
%!  assert(interp2(A,[1:4],[1:4]),[1,1,1,1]);

%!test % non-gridded XI,YI
%!  A = eye(4);
%!  assert(interp2(A,[1,2;3,4],[1,3;2,4]),[1,0;0,1]);

%!test % for values outside of boundaries
%!  x = [1,2,3];
%!  y = [4,5,6,7];
%!  [X, Y] = meshgrid(x,y);
%!  Orig = X.^2 + Y.^3;
%!  xi = [0,4];
%!  yi = [3,8]';
%!  assert(interp2(x,y,Orig, xi, yi),[NA,NA;NA,NA]);
%!  assert(interp2(x,y,Orig, xi, yi,'linear', 0),[0,0;0,0]);

%!test % for values at boundaries
%!  A=[1,2;3,4];
%!  x=[0,1];
%!  y=[2,3]';
%!  assert(interp2(x,y,A,x,y,'linear'), A);
%!  assert(interp2(x,y,A,x,y,'nearest'), A);

%!test % for Matlab-compatible rounding for 'nearest'
%! X = meshgrid (1:4);
%! assert (interp2 (X, 2.5, 2.5, 'nearest'), 3);

%!shared z, zout, tol
%!  z = [1 3 5; 3 5 7; 5 7 9];
%!  zout = [1 2 3 4 5; 2 3 4 5 6; 3 4 5 6 7; 4 5 6 7 8; 5 6 7 8 9];
%!  tol = 2 * eps;
%!assert (interp2 (z), zout, tol);
%!assert (interp2 (z, "linear"), zout, tol);
%!assert (interp2 (z, "pchip"), zout, tol);
%!assert (interp2 (z, "cubic"), zout, 10 * tol);
%!assert (interp2 (z, "spline"), zout, tol);
%!assert (interp2 (z, [2 3 1], [2 2 2]', "linear"), repmat ([5, 7, 3], [3, 1]), tol) 
%!assert (interp2 (z, [2 3 1], [2 2 2]', "pchip"), repmat ([5, 7, 3], [3, 1]), tol) 
%!assert (interp2 (z, [2 3 1], [2 2 2]', "cubic"), repmat ([5, 7, 3], [3, 1]), 10 * tol) 
%!assert (interp2 (z, [2 3 1], [2 2 2]', "spline"), repmat ([5, 7, 3], [3, 1]), tol) 
%!assert (interp2 (z, [2 3 1], [2 2 2], "linear"), [5 7 3], tol);
%!assert (interp2 (z, [2 3 1], [2 2 2], "pchip"), [5 7 3], tol);
%!assert (interp2 (z, [2 3 1], [2 2 2], "cubic"), [5 7 3], 10 * tol);
%!assert (interp2 (z, [2 3 1], [2 2 2], "spline"), [5 7 3], tol);
