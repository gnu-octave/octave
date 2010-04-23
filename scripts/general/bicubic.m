## Copyright (C) 2005, 2006, 2007, 2008, 2009 Hoxide Ma
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
## @deftypefn {Function File} {@var{zi} =} bicubic (@var{x}, @var{y}, @var{z}, @var{xi}, @var{yi}, @var{extrapval})
##
## Return a matrix @var{zi} corresponding to the bicubic
## interpolations at @var{xi} and @var{yi} of the data supplied
## as @var{x}, @var{y} and @var{z}.  Points outside the grid are set
## to @var{extrapval}.
##
## See @url{http://wiki.woodpecker.org.cn/moin/Octave/Bicubic}
## for further information.
## @seealso{interp2}
## @end deftypefn

## Bicubic interpolation method.
## Author: Hoxide Ma <hoxide_dirac@yahoo.com.cn>

function F = bicubic (X, Y, Z, XI, YI, extrapval, spline_alpha)

  if (nargin < 1 || nargin > 7)
    print_usage ();
  endif

  if (nargin == 7 && isscalar(spline_alpha))
    a = spline_alpha
  else
    a = 0.5;
  endif

  if (nargin < 6)
    extrapval = NaN;
  endif

  if (isa (X, "single") || isa (Y, "single") || isa (Z, "single") || 
      isa (XI, "single") || isa (YI, "single"))
    myeps = eps("single");
  else
    myeps = eps;
  endif

  if (nargin <= 2)
    ## bicubic (Z) or bicubic (Z, 2)
    if (nargin == 1) 
      n = 1;
    else
      n = Y;
    endif
    Z = X;
    X = [];
    [rz, cz] = size (Z);
    s = linspace (1, cz, (cz-1)*pow2(n)+1);
    t = linspace (1, rz, (rz-1)*pow2(n)+1);
  elseif (nargin == 3)
    if (! isvector (X) || ! isvector (Y))
      error ("XI and YI must be vector");
    endif
    s = Y;
    t = Z;
    Z = X;
    [rz, cz] = size (Z);
  elseif (nargin == 5 || nargin == 6)
    [rz, cz] = size (Z) ; 
    if (isvector (X) && isvector (Y))
      if (rz != length (Y) || cz != length (X))
        error ("length of X and Y must match the size of Z");
      endif
    elseif (size_equal (X, Y) && size_equal (X, Z))
      X = X(1,:);
      Y = Y(:,1);
    else
      error ("X, Y and Z must be martrices of same size");
    endif
    
    ## Mark values outside the lookup table.
    xfirst_ind = find (XI < X(1));
    xlast_ind  = find (XI > X(cz));    
    yfirst_ind = find (YI < Y(1));
    ylast_ind  = find (YI > Y(rz));
    ## Set value outside the table preliminary to min max index.
    XI(xfirst_ind) = X(1);
    XI(xlast_ind) = X(cz);
    YI(yfirst_ind) = Y(1);
    YI(ylast_ind) = Y(rz);


    X = reshape (X, 1, cz);
    X(cz) *= 1 + sign (X(cz))*myeps;
    if (X(cz) == 0) 
      X(cz) = myeps;
    endif; 
    XI = reshape (XI, 1, length (XI));
    [m, i] = sort ([X, XI]);
    o = cumsum (i <= cz);
    xidx = o(find (i > cz));
    
    Y = reshape (Y, rz, 1);
    Y(rz) *= 1 + sign (Y(rz))*myeps;
    if (Y(rz) == 0) 
      Y(rz) = myeps;
    endif; 
    YI = reshape (YI, length (YI), 1);
    [m, i] = sort ([Y; YI]);
    o = cumsum (i <= rz);
    yidx = o([find(i > rz)]);
    
    ## Set s and t used follow codes.
    s = xidx + ((XI .- X(xidx))./(X(xidx+1) .- X(xidx)));
    t = yidx + ((YI - Y(yidx))./(Y(yidx+1) - Y(yidx)));
  else
    print_usage ();
  endif
  
  if (rz < 3 || cz < 3)
    error ("Z at least a 3 by 3 matrices");
  endif

  inds = floor (s);
  d = find (s == cz);
  s = s - floor (s);
  inds(d) = cz-1;
  s(d) = 1.0;
  
  d = [];
  indt = floor (t);
  d = find (t == rz);
  t = t - floor (t);
  indt(d) = rz-1;
  t(d) = 1.0;
  d = [];

  p = zeros (size (Z) + 2);
  p(2:rz+1,2:cz+1) = Z;
  p(1,:) =    (6*(1-a))*p(2,:)    - 3*p(3,:)  + (6*a-2)*p(4,:);
  p(rz+2,:) = (6*(1-a))*p(rz+1,:) - 3*p(rz,:) + (6*a-2)*p(rz-1,:);
  p(:,1) =    (6*(1-a))*p(:,2)    - 3*p(:,3)  + (6*a-2)*p(:,4);
  p(:,cz+2) = (6*(1-a))*p(:,cz+1) - 3*p(:,cz) + (6*a-2)*p(:,cz-1);

  ## Calculte the C1(t) C2(t) C3(t) C4(t) and C1(s) C2(s) C3(s) C4(s).
  t2 = t.*t;
  t3 = t2.*t;

  ct0 =    -a .* t3 +     (2 * a) .* t2 - a .* t ;      # -a G0
  ct1 = (2-a) .* t3 +      (-3+a) .* t2          + 1 ;  # F0 - a G1
  ct2 = (a-2) .* t3 + (-2 *a + 3) .* t2 + a .* t ;      # F1 + a G0
  ct3 =     a .* t3 -           a .* t2;                # a G1
  t = []; t2 = []; t3 = [];

  s2 = s.*s;
  s3 = s2.*s;

  cs0 =    -a .* s3 +     (2 * a) .* s2 - a .*s ;      # -a G0
  cs1 = (2-a) .* s3 +    (-3 + a) .* s2         + 1 ;  # F0 - a G1
  cs2 = (a-2) .* s3 + (-2 *a + 3) .* s2 + a .*s ;      # F1 + a G0
  cs3 =     a .* s3 -           a .* s2;               # a G1
  s = []; s2 = []; s3 = [];

  cs0 = cs0([1,1,1,1],:);
  cs1 = cs1([1,1,1,1],:);
  cs2 = cs2([1,1,1,1],:);
  cs3 = cs3([1,1,1,1],:);

  lent = length (ct0);
  lens = length (cs0);
  F = zeros (lent, lens);
  
  for i = 1:lent
    it = indt(i);
    int = [it, it+1, it+2, it+3];
    F(i,:) = ([ct0(i),ct1(i),ct2(i),ct3(i)]
              * (p(int,inds) .* cs0 + p(int,inds+1) .* cs1
                 + p(int,inds+2) .* cs2 + p(int,inds+3) .* cs3));
  endfor

  ## Set points outside the table to extrapval.
  if (! (isempty (xfirst_ind) && isempty (xlast_ind)))
    F(:, [xfirst_ind, xlast_ind]) = extrapval;
  endif
  if (! (isempty (yfirst_ind) && isempty (ylast_ind)))
    F([yfirst_ind; ylast_ind], :) = extrapval;
  endif

endfunction

%!demo
%! A=[13,-1,12;5,4,3;1,6,2];
%! x=[0,1,4]+10; y=[-10,-9,-8];
%! xi=linspace(min(x),max(x),17);
%! yi=linspace(min(y),max(y),26)';
%! mesh(xi,yi,bicubic(x,y,A,xi,yi));
%! [x,y] = meshgrid(x,y);
%! hold on; plot3(x(:),y(:),A(:),"b*"); hold off;
