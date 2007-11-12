## Copyright (C) 2007 Kai Habel
##
## This program is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2, or (at your option)
## any later version.
##
## OctPlot is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with OctPlot; see the file COPYING.  If not, write to the Free
## Software Foundation, 59 Temple Place - Suite 330, Boston, MA
## 02111-1307, USA.

## -*- texinfo -*-
## @deftypefn {Function File}  ribbon (@var{X}, @var{Y}, @var{WIDTH})
## @deftypefnx {Function File}  ribbon (@var{X}, @var{Y})
## @deftypefnx {Function File}  ribbon (@var{Y})
## @deftypefnx {Function File}  @var{h} = ribbon (...)
## Plots ribbon plot for the columns of @var{Y} vs. @var{X}. The optional 
## parameter @var{WIDTH} specifies the width of a single ribbon (default is 0.75).
## If @var{X} is omitted, a vector containing the row numbers is assumed (1:rows(Y)).
## If requested a vector @var{h} of the handles to the surface objects is returned.
## @end deftypefn
## @seealso{gca, colorbar}

## Author: Kai Habel <kai.habel at gmx.de>

function h = ribbon(X, Y, W)

  newplot ();

  if (nargin == 1)
    Y = X;
    if (isvector(Y))
      Y = Y(:);
    endif
    [nr, nc] = size(Y);
    X = repmat((1 : nr)', 1, nc);
    W = 0.75;
  elseif (nargin == 2)
    W = 0.75;
  elseif (nargin == 3)
  else
    print_usage();
  end

  if (isvector(X) && isvector(Y))
    if (length(X) != length(Y))
      error("In case of vectors, X and Y must have same length")
    else
      [X, Y] = meshgrid(X, Y);
    endif
  else
    if (!all(size(X) == size(Y)))
      error("In case of matrices, X and Y must have same size")
    endif
  endif

  [nr,nc] = size(Y);
  tmp = zeros(1,nc);

  for c = nc:-1:1
    ZZ = [Y(:,c) Y(:,c)];
    y = X(:,c);
    x = [c - W / 2, c + W / 2];
    [XX,YY] = meshgrid(x,y);
    CC = ones(size(ZZ))*c;
    tmp(c) = surface(XX,YY,ZZ,CC);
  endfor

  ax = get (tmp(c), "parent");

  if (!ishold ())
    set (ax, "view", [-37.5, 30], "box","off","xgrid","on","ygrid","on","zgrid","on");
  endif

  if (nargout > 0)
    h = tmp;
  endif
end
