## Copyright (C) 2000, 2006, 2007, 2008, 2009 Paul Kienzle
## Copyright (C) 2009 VZLU Prague
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
## @deftypefn  {Function File} {@var{yi} =} interp1 (@var{x}, @var{y}, @var{xi})
## @deftypefnx {Function File} {@var{yi} =} interp1 (@dots{}, @var{method})
## @deftypefnx {Function File} {@var{yi} =} interp1 (@dots{}, @var{extrap})
## @deftypefnx {Function File} {@var{pp} =} interp1 (@dots{}, 'pp')
##
## One-dimensional interpolation.  Interpolate @var{y}, defined at the
## points @var{x}, at the points @var{xi}.  The sample points @var{x} 
## must be monotonic.  If @var{y} is an array, treat the columns
## of @var{y} separately.
##
## Method is one of:
##
## @table @asis
## @item 'nearest'
## Return the nearest neighbor.
## @item 'linear'
## Linear interpolation from nearest neighbors
## @item 'pchip'
## Piece-wise cubic Hermite interpolating polynomial
## @item 'cubic'
## Cubic interpolation from four nearest neighbors
## @item 'spline'
## Cubic spline interpolation--smooth first and second derivatives
## throughout the curve
## @end table
##
## Appending '*' to the start of the above method forces @code{interp1}
## to assume that @var{x} is uniformly spaced, and only @code{@var{x}
## (1)} and @code{@var{x} (2)} are referenced.  This is usually faster,
## and is never slower.  The default method is 'linear'.
##
## If @var{extrap} is the string 'extrap', then extrapolate values beyond
## the endpoints.  If @var{extrap} is a number, replace values beyond the
## endpoints with that number.  If @var{extrap} is missing, assume NA.
##
## If the string argument 'pp' is specified, then @var{xi} should not be
## supplied and @code{interp1} returns the piece-wise polynomial that
## can later be used with @code{ppval} to evaluate the interpolation.
## There is an equivalence, such that @code{ppval (interp1 (@var{x},
## @var{y}, @var{method}, 'pp'), @var{xi}) == interp1 (@var{x}, @var{y},
## @var{xi}, @var{method}, 'extrap')}.
##
## Duplicate points in @var{x} specify a discontinuous interpolant.  There
## should be at most 2 consecutive points with the same value.
## The discontinuous interpolant is right-continuous if @var{x} is increasing,
## left-continuous if it is decreasing.
## Discontinuities are (currently) only allowed for "nearest" and "linear"
## methods; in all other cases, @var{x} must be strictly monotonic.
##
## An example of the use of @code{interp1} is
##
## @example
## @group
## xf = [0:0.05:10];
## yf = sin (2*pi*xf/5);
## xp = [0:10];
## yp = sin (2*pi*xp/5);
## lin = interp1 (xp, yp, xf);
## spl = interp1 (xp, yp, xf, "spline");
## cub = interp1 (xp, yp, xf, "cubic");
## near = interp1 (xp, yp, xf, "nearest");
## plot (xf, yf, "r", xf, lin, "g", xf, spl, "b",
##       xf, cub, "c", xf, near, "m", xp, yp, "r*");
## legend ("original", "linear", "spline", "cubic", "nearest")
## @end group
## @end example
##
## @seealso{interpft}
## @end deftypefn

## Author: Paul Kienzle
## Date: 2000-03-25
##    added 'nearest' as suggested by Kai Habel
## 2000-07-17 Paul Kienzle
##    added '*' methods and matrix y
##    check for proper table lengths
## 2002-01-23 Paul Kienzle
##    fixed extrapolation

function yi = interp1 (x, y, varargin)

  if (nargin < 3 || nargin > 6)
    print_usage ();
  endif

  method = "linear";
  extrap = NA;
  xi = [];
  pp = false;
  firstnumeric = true;

  if (nargin > 2)
    for i = 1:length (varargin)
      arg = varargin{i};
      if (ischar (arg))
        arg = tolower (arg);
        if (strcmp ("extrap", arg))
          extrap = "extrap";
        elseif (strcmp ("pp", arg))
          pp = true;
        else
          method = arg;
        endif
      else
        if (firstnumeric)
          xi = arg;
          firstnumeric = false;
        else
          extrap = arg;
        endif
      endif
    endfor
  endif

  ## reshape matrices for convenience
  x = x(:);
  nx = rows (x);
  szx = size (xi);
  if (isvector (y))
    y = y(:);
  elseif (isvector (xi))
    szx = length (xi);    
  endif
  szy = size (y);
  y = y(:,:);
  [ny, nc] = size (y);
  xi = xi(:);

  ## determine sizes
  if (nx < 2 || ny < 2)
    error ("interp1: table too short");
  endif

  ## check whether x is sorted; sort if not.
  if (! issorted (x, "either"))
    [x, p] = sort (x);
    y = y(p,:);
  endif

  starmethod = method(1) == "*";

  if (starmethod)
    dx = x(2) - x(1);
  else
    jumps = x(1:nx-1) == x(2:nx);
    have_jumps = any (jumps);
    if (have_jumps)
      if (any (strcmp (method, {"nearest", "linear"})))
        if (any (jumps(1:nx-2) & jumps(2:nx-1)))
          warning ("interp1: extra points in discontinuities");
        endif
      else
        error ("interp1: discontinuities not supported for method %s", method);
      endif
    endif
  endif

  ## Proceed with interpolating by all methods.

  switch (method)
  case "nearest"
    if (pp)
      yi = mkpp ([x(1); (x(1:nx-1)+x(2:nx))/2; x(nx)], y, szy(2:end));
    else
      idx = lookup (0.5*(x(1:nx-1)+x(2:nx)), xi) + 1;
      yi = y(idx,:);
    endif
  case "*nearest"
    if (pp)
      yi = mkpp ([x(1); x(1)+[0.5:(nx-1)]'*dx; x(nx)], y, szy(2:end));
    else
      idx = max (1, min (ny, floor((xi-x(1))/dx+1.5)));
      yi = y(idx,:);
    endif
  case "linear"
    dy = diff (y);
    dx = diff (x);
    if (pp)
      coefs = [dy./dx, y(1:nx-1)];
      xx = x;
      if (have_jumps)
        ## Omit zero-size intervals.
        coefs(jumps) = [];
        xx(jumps) = [];
      endif
      yi = mkpp (xx, coefs, szy(2:end));
    else
      ## find the interval containing the test point
      idx = lookup (x, xi, "lr");
      ## use the endpoints of the interval to define a line
      s = (xi - x(idx))./dx(idx);
      yi = bsxfun (@times, s, dy(idx,:)) + y(idx,:);
      if (have_jumps)
        ## Fix the corner cases of discontinuities at boundaries.
        ## Internal discontinuities already handled correctly.
        if (jumps (1))
          mask = xi < x(1);
          yi(mask,:) = y(1*ones (1, sum (mask)),:);
        endif
        if (jumps(nx-1))
          mask = xi >= x(nx);
          yi(mask,:) = y(nx*ones (1, sum (mask)),:);
        endif
      endif
    endif
  case "*linear"
    dy = diff (y);
    if (pp)
      yi = mkpp (x(1) + [0:ny-1]*dx, [dy./dx, y(1:end-1)], szy(2:end));
    else
      ## find the interval containing the test point
      t = (xi - x(1))/dx + 1;
      idx = max (1, min (ny - 1, floor (t)));

      ## use the endpoints of the interval to define a line
      s = t - idx;
      yi = bsxfun (@times, s, dy(idx,:)) + y(idx,:);
    endif
  case {"pchip", "*pchip"}
    if (nx == 2 || starmethod) 
      x = linspace (x(1), x(nx), ny);
    endif
    ## Note that pchip's arguments are transposed relative to interp1
    if (pp)
      yi = pchip (x.', y.');
      yi.d = szy(2:end);
    else
      yi = pchip (x.', y.', xi.').';
    endif

  case {"cubic", "*cubic"}
    if (nx < 4 || ny < 4)
      error ("interp1: table too short");
    endif

    ## FIXME Is there a better way to treat pp return and *cubic
    if (starmethod && ! pp)
      ## From: Miloje Makivic 
      ## http://www.npac.syr.edu/projects/nasa/MILOJE/final/node36.html
      t = (xi - x(1))/dx + 1;
      idx = max (min (floor (t), ny-2), 2);
      t = t - idx;
      t2 = t.*t;
      tp = 1 - 0.5*t;
      a = (1 - t2).*tp;
      b = (t2 + t).*tp;
      c = (t2 - t).*tp/3;
      d = (t2 - 1).*t/6;
      J = ones (1, nc);

      yi = a(:,J) .* y(idx,:) + b(:,J) .* y(idx+1,:) ...
      + c(:,J) .* y(idx-1,:) + d(:,J) .* y(idx+2,:);
    else
      if (starmethod)
        x = linspace (x(1), x(nx), ny).'; 
        nx = ny;
      endif

      idx = lookup (x(2:nx-1), xi, "lr");

      ## Construct cubic equations for each interval using divided
      ## differences (computation of c and d don't use divided differences
      ## but instead solve 2 equations for 2 unknowns). Perhaps
      ## reformulating this as a lagrange polynomial would be more efficient.
      i = 1:nx-3;
      J = ones (1, nc);
      dx = diff (x);
      dx2 = x(i+1).^2 - x(i).^2;
      dx3 = x(i+1).^3 - x(i).^3;
      a = diff (y, 3)./dx(i,J).^3/6;
      b = (diff (y(1:nx-1,:), 2)./dx(i,J).^2 - 6*a.*x(i+1,J))/2;
      c = (diff (y(1:nx-2,:), 1) - a.*dx3(:,J) - b.*dx2(:,J))./dx(i,J);
      d = y(i,:) - ((a.*x(i,J) + b).*x(i,J) + c).*x(i,J);

      if (pp)
        xs = [x(1);x(3:nx-2)];
        yi = mkpp ([x(1);x(3:nx-2);x(nx)], 
                   [a(:), (b(:) + 3.*xs(:,J).*a(:)), ... 
                    (c(:) + 2.*xs(:,J).*b(:) + 3.*xs(:,J)(:).^2.*a(:)), ...
                    (d(:) + xs(:,J).*c(:) + xs(:,J).^2.*b(:) + ...
                     xs(:,J).^3.*a(:))], szy(2:end));
      else
        yi = ((a(idx,:).*xi(:,J) + b(idx,:)).*xi(:,J) ...
              + c(idx,:)).*xi(:,J) + d(idx,:);
      endif
    endif
  case {"spline", "*spline"}
    if (nx == 2 || starmethod) 
      x = linspace(x(1), x(nx), ny); 
    endif
    ## Note that spline's arguments are transposed relative to interp1
    if (pp)
      yi = spline (x.', y.');
      yi.d = szy(2:end);
    else
      yi = spline (x.', y.', xi.').';
    endif
  otherwise
    error ("interp1: invalid method '%s'", method);
  endswitch

  if (! pp)
    if (! ischar (extrap))
      ## determine which values are out of range and set them to extrap,
      ## unless extrap == "extrap".
      minx = min (x(1), x(nx));
      maxx = max (x(1), x(nx));

      outliers = xi < minx | ! (xi <= maxx); # this catches even NaNs
      yi(outliers, :) = extrap;
    endif

    yi = reshape (yi, [szx, szy(2:end)]);
  endif

endfunction

%!demo
%! xf=0:0.05:10; yf = sin(2*pi*xf/5);
%! xp=0:10;      yp = sin(2*pi*xp/5);
%! lin=interp1(xp,yp,xf,"linear");
%! spl=interp1(xp,yp,xf,"spline");
%! cub=interp1(xp,yp,xf,"pchip");
%! near=interp1(xp,yp,xf,"nearest");
%! plot(xf,yf,"r",xf,near,"g",xf,lin,"b",xf,cub,"c",xf,spl,"m",xp,yp,"r*");
%! legend ("original","nearest","linear","pchip","spline")
%! %--------------------------------------------------------
%! % confirm that interpolated function matches the original

%!demo
%! xf=0:0.05:10; yf = sin(2*pi*xf/5);
%! xp=0:10;      yp = sin(2*pi*xp/5);
%! lin=interp1(xp,yp,xf,"*linear");
%! spl=interp1(xp,yp,xf,"*spline");
%! cub=interp1(xp,yp,xf,"*cubic");
%! near=interp1(xp,yp,xf,"*nearest");
%! plot(xf,yf,"r",xf,near,"g",xf,lin,"b",xf,cub,"c",xf,spl,"m",xp,yp,"r*");
%! legend ("*original","*nearest","*linear","*cubic","*spline")
%! %--------------------------------------------------------
%! % confirm that interpolated function matches the original

%!demo
%! t = 0 : 0.3 : pi; dt = t(2)-t(1);
%! n = length (t); k = 100; dti = dt*n/k;
%! ti = t(1) + [0 : k-1]*dti;
%! y = sin (4*t + 0.3) .* cos (3*t - 0.1);
%! ddyc = diff(diff(interp1(t,y,ti,'cubic'))./dti)./dti;
%! ddys = diff(diff(interp1(t,y,ti,'spline'))./dti)./dti;
%! ddyp = diff(diff(interp1(t,y,ti,'pchip'))./dti)./dti;
%! plot (ti(2:end-1), ddyc,'g+',ti(2:end-1),ddys,'b*', ...
%!       ti(2:end-1),ddyp,'c^');
%! legend('cubic','spline','pchip');
%! title("Second derivative of interpolated 'sin (4*t + 0.3) .* cos (3*t - 0.1)'");

%!demo
%! xf=0:0.05:10; yf = sin(2*pi*xf/5) - (xf >= 5);
%! xp=[0:.5:4.5,4.99,5:.5:10];      yp = sin(2*pi*xp/5) - (xp >= 5);
%! lin=interp1(xp,yp,xf,"linear");
%! near=interp1(xp,yp,xf,"nearest");
%! plot(xf,yf,"r",xf,near,"g",xf,lin,"b",xp,yp,"r*");
%! legend ("original","nearest","linear")
%! %--------------------------------------------------------
%! % confirm that interpolated function matches the original


## For each type of interpolated test, confirm that the interpolated
## value at the knots match the values at the knots.  Points away
## from the knots are requested, but only 'nearest' and 'linear'
## confirm they are the correct values.

%!shared xp, yp, xi, style
%! xp=0:2:10;      yp = sin(2*pi*xp/5);  
%! xi = [-1, 0, 2.2, 4, 6.6, 10, 11];


## The following BLOCK/ENDBLOCK section is repeated for each style
##    nearest, linear, cubic, spline, pchip
## The test for ppval of cubic has looser tolerance, but otherwise
## the tests are identical.
## Note that the block checks style and *style; if you add more tests
## before to add them to both sections of each block.  One test, 
## style vs. *style, occurs only in the first section.
## There is an ENDBLOCKTEST after the final block
%!test style = "nearest";
## BLOCK
%!assert (interp1(xp, yp, [min(xp)-1, max(xp)+1],style), [NA, NA]);
%!assert (interp1(xp,yp,xp,style), yp, 100*eps);
%!assert (interp1(xp,yp,xp',style), yp', 100*eps);
%!assert (interp1(xp',yp',xp',style), yp', 100*eps);
%!assert (interp1(xp',yp',xp,style), yp, 100*eps);
%!assert (isempty(interp1(xp',yp',[],style)));
%!assert (isempty(interp1(xp,yp,[],style)));
%!assert (interp1(xp,[yp',yp'],xi(:),style),...
%!        [interp1(xp,yp,xi(:),style),interp1(xp,yp,xi(:),style)]);
%!assert (interp1(xp,yp,xi,style),...
%!        interp1(fliplr(xp),fliplr(yp),xi,style),100*eps);
%!assert (ppval(interp1(xp,yp,style,"pp"),xi),
%!        interp1(xp,yp,xi,style,"extrap"),10*eps);
%!error interp1(1,1,1, style);
%!assert (interp1(xp,[yp',yp'],xi,style),
%!        interp1(xp,[yp',yp'],xi,["*",style]),100*eps);
%!test style=['*',style];
%!assert (interp1(xp, yp, [min(xp)-1, max(xp)+1],style), [NA, NA]);
%!assert (interp1(xp,yp,xp,style), yp, 100*eps);
%!assert (interp1(xp,yp,xp',style), yp', 100*eps);
%!assert (interp1(xp',yp',xp',style), yp', 100*eps);
%!assert (interp1(xp',yp',xp,style), yp, 100*eps);
%!assert (isempty(interp1(xp',yp',[],style)));
%!assert (isempty(interp1(xp,yp,[],style)));
%!assert (interp1(xp,[yp',yp'],xi(:),style),...
%!        [interp1(xp,yp,xi(:),style),interp1(xp,yp,xi(:),style)]);
%!assert (interp1(xp,yp,xi,style),...
%!        interp1(fliplr(xp),fliplr(yp),xi,style),100*eps);
%!assert (ppval(interp1(xp,yp,style,"pp"),xi),
%!        interp1(xp,yp,xi,style,"extrap"),10*eps);
%!error interp1(1,1,1, style);
## ENDBLOCK
%!test style='linear';
## BLOCK
%!assert (interp1(xp, yp, [min(xp)-1, max(xp)+1],style), [NA, NA]);
%!assert (interp1(xp,yp,xp,style), yp, 100*eps);
%!assert (interp1(xp,yp,xp',style), yp', 100*eps);
%!assert (interp1(xp',yp',xp',style), yp', 100*eps);
%!assert (interp1(xp',yp',xp,style), yp, 100*eps);
%!assert (isempty(interp1(xp',yp',[],style)));
%!assert (isempty(interp1(xp,yp,[],style)));
%!assert (interp1(xp,[yp',yp'],xi(:),style),...
%!        [interp1(xp,yp,xi(:),style),interp1(xp,yp,xi(:),style)]);
%!assert (interp1(xp,yp,xi,style),...
%!        interp1(fliplr(xp),fliplr(yp),xi,style),100*eps);
%!assert (ppval(interp1(xp,yp,style,"pp"),xi),
%!        interp1(xp,yp,xi,style,"extrap"),10*eps);
%!error interp1(1,1,1, style);
%!assert (interp1(xp,[yp',yp'],xi,style),
%!        interp1(xp,[yp',yp'],xi,["*",style]),100*eps);
%!test style=['*',style];
%!assert (interp1(xp, yp, [min(xp)-1, max(xp)+1],style), [NA, NA]);
%!assert (interp1(xp,yp,xp,style), yp, 100*eps);
%!assert (interp1(xp,yp,xp',style), yp', 100*eps);
%!assert (interp1(xp',yp',xp',style), yp', 100*eps);
%!assert (interp1(xp',yp',xp,style), yp, 100*eps);
%!assert (isempty(interp1(xp',yp',[],style)));
%!assert (isempty(interp1(xp,yp,[],style)));
%!assert (interp1(xp,[yp',yp'],xi(:),style),...
%!        [interp1(xp,yp,xi(:),style),interp1(xp,yp,xi(:),style)]);
%!assert (interp1(xp,yp,xi,style),...
%!        interp1(fliplr(xp),fliplr(yp),xi,style),100*eps);
%!assert (ppval(interp1(xp,yp,style,"pp"),xi),
%!        interp1(xp,yp,xi,style,"extrap"),10*eps);
%!error interp1(1,1,1, style);
## ENDBLOCK
%!test style='cubic';
## BLOCK
%!assert (interp1(xp, yp, [min(xp)-1, max(xp)+1],style), [NA, NA]);
%!assert (interp1(xp,yp,xp,style), yp, 100*eps);
%!assert (interp1(xp,yp,xp',style), yp', 100*eps);
%!assert (interp1(xp',yp',xp',style), yp', 100*eps);
%!assert (interp1(xp',yp',xp,style), yp, 100*eps);
%!assert (isempty(interp1(xp',yp',[],style)));
%!assert (isempty(interp1(xp,yp,[],style)));
%!assert (interp1(xp,[yp',yp'],xi(:),style),...
%!        [interp1(xp,yp,xi(:),style),interp1(xp,yp,xi(:),style)]);
%!assert (interp1(xp,yp,xi,style),...
%!        interp1(fliplr(xp),fliplr(yp),xi,style),100*eps);
%!assert (ppval(interp1(xp,yp,style,"pp"),xi),
%!        interp1(xp,yp,xi,style,"extrap"),100*eps);
%!error interp1(1,1,1, style);
%!assert (interp1(xp,[yp',yp'],xi,style),
%!        interp1(xp,[yp',yp'],xi,["*",style]),100*eps);
%!test style=['*',style];
%!assert (interp1(xp, yp, [min(xp)-1, max(xp)+1],style), [NA, NA]);
%!assert (interp1(xp,yp,xp,style), yp, 100*eps);
%!assert (interp1(xp,yp,xp',style), yp', 100*eps);
%!assert (interp1(xp',yp',xp',style), yp', 100*eps);
%!assert (interp1(xp',yp',xp,style), yp, 100*eps);
%!assert (isempty(interp1(xp',yp',[],style)));
%!assert (isempty(interp1(xp,yp,[],style)));
%!assert (interp1(xp,[yp',yp'],xi(:),style),...
%!        [interp1(xp,yp,xi(:),style),interp1(xp,yp,xi(:),style)]);
%!assert (interp1(xp,yp,xi,style),...
%!        interp1(fliplr(xp),fliplr(yp),xi,style),100*eps);
%!assert (ppval(interp1(xp,yp,style,"pp"),xi),
%!        interp1(xp,yp,xi,style,"extrap"),100*eps);
%!error interp1(1,1,1, style);
## ENDBLOCK
%!test style='pchip';
## BLOCK
%!assert (interp1(xp, yp, [min(xp)-1, max(xp)+1],style), [NA, NA]);
%!assert (interp1(xp,yp,xp,style), yp, 100*eps);
%!assert (interp1(xp,yp,xp',style), yp', 100*eps);
%!assert (interp1(xp',yp',xp',style), yp', 100*eps);
%!assert (interp1(xp',yp',xp,style), yp, 100*eps);
%!assert (isempty(interp1(xp',yp',[],style)));
%!assert (isempty(interp1(xp,yp,[],style)));
%!assert (interp1(xp,[yp',yp'],xi(:),style),...
%!        [interp1(xp,yp,xi(:),style),interp1(xp,yp,xi(:),style)]);
%!assert (interp1(xp,yp,xi,style),...
%!        interp1(fliplr(xp),fliplr(yp),xi,style),100*eps);
%!assert (ppval(interp1(xp,yp,style,"pp"),xi),
%!        interp1(xp,yp,xi,style,"extrap"),10*eps);
%!error interp1(1,1,1, style);
%!assert (interp1(xp,[yp',yp'],xi,style),
%!        interp1(xp,[yp',yp'],xi,["*",style]),100*eps);
%!test style=['*',style];
%!assert (interp1(xp, yp, [min(xp)-1, max(xp)+1],style), [NA, NA]);
%!assert (interp1(xp,yp,xp,style), yp, 100*eps);
%!assert (interp1(xp,yp,xp',style), yp', 100*eps);
%!assert (interp1(xp',yp',xp',style), yp', 100*eps);
%!assert (interp1(xp',yp',xp,style), yp, 100*eps);
%!assert (isempty(interp1(xp',yp',[],style)));
%!assert (isempty(interp1(xp,yp,[],style)));
%!assert (interp1(xp,[yp',yp'],xi(:),style),...
%!        [interp1(xp,yp,xi(:),style),interp1(xp,yp,xi(:),style)]);
%!assert (interp1(xp,yp,xi,style),...
%!        interp1(fliplr(xp),fliplr(yp),xi,style),100*eps);
%!assert (ppval(interp1(xp,yp,style,"pp"),xi),
%!        interp1(xp,yp,xi,style,"extrap"),10*eps);
%!error interp1(1,1,1, style);
## ENDBLOCK
%!test style='spline';
## BLOCK
%!assert (interp1(xp, yp, [min(xp)-1, max(xp)+1],style), [NA, NA]);
%!assert (interp1(xp,yp,xp,style), yp, 100*eps);
%!assert (interp1(xp,yp,xp',style), yp', 100*eps);
%!assert (interp1(xp',yp',xp',style), yp', 100*eps);
%!assert (interp1(xp',yp',xp,style), yp, 100*eps);
%!assert (isempty(interp1(xp',yp',[],style)));
%!assert (isempty(interp1(xp,yp,[],style)));
%!assert (interp1(xp,[yp',yp'],xi(:),style),...
%!        [interp1(xp,yp,xi(:),style),interp1(xp,yp,xi(:),style)]);
%!assert (interp1(xp,yp,xi,style),...
%!        interp1(fliplr(xp),fliplr(yp),xi,style),100*eps);
%!assert (ppval(interp1(xp,yp,style,"pp"),xi),
%!        interp1(xp,yp,xi,style,"extrap"),10*eps);
%!error interp1(1,1,1, style);
%!assert (interp1(xp,[yp',yp'],xi,style),
%!        interp1(xp,[yp',yp'],xi,["*",style]),100*eps);
%!test style=['*',style];
%!assert (interp1(xp, yp, [min(xp)-1, max(xp)+1],style), [NA, NA]);
%!assert (interp1(xp,yp,xp,style), yp, 100*eps);
%!assert (interp1(xp,yp,xp',style), yp', 100*eps);
%!assert (interp1(xp',yp',xp',style), yp', 100*eps);
%!assert (interp1(xp',yp',xp,style), yp, 100*eps);
%!assert (isempty(interp1(xp',yp',[],style)));
%!assert (isempty(interp1(xp,yp,[],style)));
%!assert (interp1(xp,[yp',yp'],xi(:),style),...
%!        [interp1(xp,yp,xi(:),style),interp1(xp,yp,xi(:),style)]);
%!assert (interp1(xp,yp,xi,style),...
%!        interp1(fliplr(xp),fliplr(yp),xi,style),100*eps);
%!assert (ppval(interp1(xp,yp,style,"pp"),xi),
%!        interp1(xp,yp,xi,style,"extrap"),10*eps);
%!error interp1(1,1,1, style);
## ENDBLOCK
## ENDBLOCKTEST

%!# test linear extrapolation
%!assert (interp1([1:5],[3:2:11],[0,6],"linear","extrap"), [1, 13], eps);
%!assert (interp1(xp, yp, [-1, max(xp)+1],"linear",5), [5, 5]);

%!error interp1
%!error interp1(1:2,1:2,1,"bogus")

%!assert (interp1(1:2,1:2,1.4,"nearest"),1);
%!error interp1(1,1,1, "linear");
%!assert (interp1(1:2,1:2,1.4,"linear"),1.4);
%!error interp1(1:3,1:3,1, "cubic");
%!assert (interp1(1:4,1:4,1.4,"cubic"),1.4);
%!error interp1(1:2,1:2,1, "spline");
%!assert (interp1(1:3,1:3,1.4,"spline"),1.4);

%!error interp1(1,1,1, "*nearest");
%!assert (interp1(1:2:4,1:2:4,1.4,"*nearest"),1);
%!error interp1(1,1,1, "*linear");
%!assert (interp1(1:2:4,1:2:4,[0,1,1.4,3,4],"*linear"),[NA,1,1.4,3,NA]);
%!error interp1(1:3,1:3,1, "*cubic");
%!assert (interp1(1:2:8,1:2:8,1.4,"*cubic"),1.4);
%!error interp1(1:2,1:2,1, "*spline");
%!assert (interp1(1:2:6,1:2:6,1.4,"*spline"),1.4);

%!assert (interp1([3,2,1],[3,2,2],2.5),2.5)

%!assert (interp1 ([1,2,2,3,4],[0,1,4,2,1],[-1,1.5,2,2.5,3.5], "linear", "extrap"), [-2,0.5,4,3,1.5])
%!assert (interp1 ([4,4,3,2,0],[0,1,4,2,1],[1.5,4,4.5], "linear"), [0,1,NA])
