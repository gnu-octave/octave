## Copyright (C) 2007 Kai Habel
## Copyright (C) 2003 Shai Ayal
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
## @deftypefn {Function File} {[@var{c}, @var{h}] =} contourf (@var{x}, @var{y}, @var{z}, @var{lvl})
## @deftypefnx {Function File} {[@var{c}, @var{h}] =} contourf (@var{x}, @var{y}, @var{z}, @var{n})
## @deftypefnx {Function File} {[@var{c}, @var{h}] =} contourf (@var{x}, @var{y}, @var{z})
## @deftypefnx {Function File} {[@var{c}, @var{h}] =} contourf (@var{z}, @var{n})
## @deftypefnx {Function File} {[@var{c}, @var{h}] =} contourf (@var{z}, @var{lvl})
## @deftypefnx {Function File} {[@var{c}, @var{h}] =} contourf (@var{z})
## @deftypefnx {Function File} {[@var{c}, @var{h}] =} contourf (@var{ax}, @dots{})
## @deftypefnx {Function File} {[@var{c}, @var{h}] =} contourf (@dots{}, @var{"property"}, @var{val})
## Compute and plot filled contours of the matrix @var{z}.
## Parameters @var{x}, @var{y} and @var{n} or @var{lvl} are optional.
##
## The return value @var{c} is a 2xn matrix containing the contour lines
## as described in the help to the contourc function.
##
## The return value @var{h} is handle-vector to the patch objects creating
## the filled contours.
##
## If @var{x} and @var{y} are ommited they are taken as the row/column
## index of @var{z}.  @var{n} is a scalar denoting the number of lines
## to compute.  Alternatively @var{lvl} is a vector containing the
## contour levels. If only one value (e.g. lvl0) is wanted, set
## @var{lvl} to [lvl0, lvl0].  If both @var{n} or @var{lvl} are omitted
## a default value of 10 contour level is assumed.
##
## If provided, the filled contours are added to the axes object
## @var{ax} instead of the current axis.
##
## The following example plots filled contours of the @code{peaks}
## function.
## @example
## [x, y, z] = peaks (50);
## contourf (x, y, z, -7:9)
## @end example
## @seealso{contour, contourc, patch}
## @end deftypefn

## Author: Kai Habel <kai.habel@gmx.de>
## Author: Shai Ayal <shaiay@users.sourceforge.net>

function varargout = contourf (varargin)

  [ax, varargin] = __plt_get_axis_arg__ ("contourf", varargin{:});

  [X, Y, Z, lvl, patch_props] = parse_args (varargin);

  [nr, nc] = size (Z);

  [minx, maxx] = deal (min (X(:)), max (X(:)));
  [miny, maxy] = deal (min (Y(:)), max (Y(:)));

  if (diff (lvl) < 10*eps) 
    lvl_eps = 1e-6;
  else
    lvl_eps = min (diff (lvl)) / 1000.0;
  endif

  X0 = prepad(X, nc+1, 2 * X(1, 1) - X(1, 2), 2);
  X0 = postpad(X0, nc+2, 2 * X(1, nc) - X(1, nc - 1), 2);
  X0 = [X0(1, :); X0; X0(1, :)];
  Y0 = prepad(Y, nr+1, 2 * Y(1, 1) - Y(2, 1), 1);
  Y0 = postpad(Y0, nr+2, 2 * Y(nr, 1) - Y(nr - 1, 1));
  Y0 = [Y0(:, 1), Y0, Y0(:, 1)];

  Z0 = -Inf(nr+2, nc+2);
  Z0(2:nr+1, 2:nc+1) = Z;
  [c, lev] = contourc (X0, Y0, Z0, lvl);
  cmap = colormap ();

  levx = linspace (min (lev), max (lev), size (cmap, 1));

  newplot ();

  ## Decode contourc output format.
  i1 = 1;
  ncont = 0;
  while (i1 < columns (c))
    ncont++;
    cont_lev(ncont) = c(1, i1);
    cont_len(ncont) = c(2, i1);
    cont_idx(ncont) = i1+1;

    ii = i1+1:i1+cont_len(ncont);
    cur_cont = c(:, ii);
    c(:, ii);
    startidx = ii(1);
    stopidx = ii(end);
    cont_area(ncont) = polyarea (c(1, ii), c(2, ii));
    i1 += c(2, i1) + 1;
  endwhile

  ## Handle for each level the case where we have (a) hole(s) in a patch.
  ## Those are to be filled with the color of level below or with the
  ## background colour.
  for k = 1:numel (lev)
    lvl_idx = find (abs (cont_lev - lev(k)) < lvl_eps);
    len = numel (lvl_idx);
    if (len > 1)
      ## mark = logical(zeros(size(lvl_idx)));
      mark = false (size (lvl_idx));
      a = 1;
      while (a < len)
        # take 1st patch
        b = a + 1;
        pa_idx = lvl_idx(a);
        # get pointer to contour start, and contour length
        curr_ct_idx = cont_idx(pa_idx);
        curr_ct_len = cont_len(pa_idx);
        # get contour
        curr_ct = c(:, curr_ct_idx:curr_ct_idx+curr_ct_len-1);
        b_vec = (a+1):len;
        next_ct_pt_vec = c(:, cont_idx(lvl_idx(b_vec)));
        in = inpolygon (next_ct_pt_vec(1,:), next_ct_pt_vec(2,:),
			curr_ct(1, :), curr_ct(2, :));
        mark(b_vec(in)) = !mark(b_vec(in));
        a++;
      endwhile
      if (numel (mark) > 0)
	## All marked contours describe a hole in a larger contour of
	## the same level and must be filled with colour of level below.
        ma_idx = lvl_idx(mark);
        if (k > 1)
	  ## Find color of level below.
          tmp = find(abs(cont_lev - lev(k - 1)) < lvl_eps);
          lvl_bel_idx = tmp(1);
	  ## Set color of patches found.
	  cont_lev(ma_idx) = cont_lev(lvl_bel_idx);
        else
	  ## Set lowest level contour to NaN.
	  cont_lev(ma_idx) = NaN;
        endif
      endif
    endif
  endfor

  ## The algorithm can create patches with the size of the plotting
  ## area, we would like to draw only the patch with the highest level.
  del_idx = [];
  max_idx = find (cont_area == max (cont_area));
  if (numel (max_idx) > 1)
    # delete double entries
    del_idx = max_idx(1:end-1);
    cont_area(del_idx) = cont_lev(del_idx) = [];
    cont_len(del_idx) = cont_idx(del_idx) = [];
  endif

  ## Now we have everything together and can start plotting the patches
  ## beginning with largest area.
  [tmp, svec] = sort (cont_area);
  len = ncont - numel (del_idx);
  h = zeros (1, len);
  for n = len:-1:1
    idx = svec(n);
    ii = cont_idx(idx):cont_idx(idx) + cont_len(idx) - 2;
    h(n) = patch (c(1, ii), c(2, ii), cont_lev(idx), patch_props{:});
  endfor

  if (min (lev) == max (lev))
    set (gca (), "clim", [min(lev)-1, max(lev)+1]);
  else
    set (gca(), "clim", [min(lev), max(lev)]);
  endif

  set (gca (), "layer", "top");

  if (nargout > 0)
    varargout{2} = h;
    varargout{1} = c;
  endif

endfunction

function [X, Y, Z, lvl, patch_props] = parse_args (arg)

  patch_props = {};
  nolvl = false;

  for n = 1:numel (arg)
    if (ischar (arg{n}))
      patch_props = arg(n:end);
      arg(n:end) = [];
      break;
    endif
  endfor

  if (mod (numel (patch_props), 2) != 0)
    error ("patch: property value is missing");
  endif

  if (numel (arg) < 3)
    Z = arg{1};
    [X, Y] = meshgrid (1:columns (Z), 1:rows (Z));
  endif

  if (numel (arg) == 1)
    nolvl = true;
    arg(1) = [];
  elseif (numel (arg) == 2)
    lvl = arg{2};
    arg(1:2) = [];
  elseif (numel (arg) == 3)
    arg{1:3};
    [X, Y, Z] = deal (arg{1:3});
    arg(1:3) = [];
    nolvl = true;
  elseif (numel (arg) == 4)
    [X, Y, Z, lvl] = deal (arg{1:4});
    arg(1:4) = [];
  endif

  if (!isvector (X) || !isvector (Y) && any (size (X) != size (Y)))
    error ("patch: X and Y must be of same size")
  endif

  if (isvector (X) || isvector (Y))
    [X, Y] = meshgrid (X, Y);
  endif

  Z_no_inf = Z(!isinf (Z));
  [minz, maxz] = deal (min (Z_no_inf(:)), max (Z_no_inf(:)));
  Z(isnan (Z)) = -Inf;

  if (nolvl)
    lvl = linspace (minz, maxz, 12);
  endif

  if (isscalar (lvl))
    lvl = linspace (minz, maxz, lvl + 2)(1:end-1);
  else
    idx1 = find(lvl < minz);
    idx2 = find(lvl > maxz);
    lvl(idx1(1:end-1)) = [];
    lvl(idx2) = [];
    if (isempty (lvl))
      lvl = [minz, minz];
    endif
  endif

endfunction

%!demo
%! [x, y, z] = peaks (50);
%! contourf (x, y, z, -7:9)

%!demo
%! [theta, r] = meshgrid (linspace (0, 2*pi, 64), linspace(0,1,64));
%! [X, Y] = pol2cart (theta, r);
%! Z = sin(2*theta).*(1-r);
%! contourf(X, Y, abs(Z), 10)
