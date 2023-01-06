########################################################################
##
## Copyright (C) 2019-2023 The Octave Project Developers
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
## @deftypefn  {} {@var{xyz} =} stream3 (@var{x}, @var{y}, @var{z}, @var{u}, @var{v}, @var{w}, @var{sx}, @var{sy}, @var{sz})
## @deftypefnx {} {@var{xyz} =} stream3 (@var{u}, @var{v}, @var{w}, @var{sx}, @var{sy}, @var{sz})
## @deftypefnx {} {@var{xyz} =} stream3 (@dots{}, @var{options})
## Compute 3-D streamline data.
##
## Calculate streamlines of a vector field given by @code{[@var{u}, @var{v},
## @var{w}]}.  The vector field is defined over a rectangular grid given by
## @code{[@var{x}, @var{y}, @var{z}]}.  The streamlines start at the seed
## points @code{[@var{sx}, @var{sy}, @var{sz}]}.  The returned value @var{xyz}
## contains a cell array of vertex arrays.  If the starting point is outside
## the vector field, @code{[]} is returned.
##
## The input parameter @var{options} is a 2-D vector of the form
## @code{[@var{stepsize}, @var{max_vertices}]}.  The first parameter
## specifies the step size used for trajectory integration (default 0.1).  A
## negative value is allowed which will reverse the direction of integration.
## The second parameter specifies the maximum number of segments used to
## create a streamline (default 10,000).
##
## The return value @var{xyz} is a @nospell{nverts x 3} matrix containing the
## coordinates of the field line segments.
##
## Example:
##
## @example
## @group
## [x, y, z] = meshgrid (0:3);
## u = 2 * x;
## v = y;
## w = 3 * z;
## xyz = stream3 (x, y, z, u, v, w, 1.0, 0.5, 0.0);
## @end group
## @end example
##
## @seealso{stream2, streamline, streamribbon, streamtube, ostreamtube}
## @end deftypefn

## References:
##
## @article{
##    title = {Particle Tracing Algorithms for 3D Curvilinear Grids},
##    year = {2000},
##    author = {Nielson, Gregory and Uller, H. and Sadarjoen, I. and Walsum, Theo and Hin, Andrea and Post, Frits}
## }
##
## @article{
##    title = {Sources of error in the graphical analysis of CFD results},
##    publisher = {Journal of Scientific Computing},
##    year = {1988},
##    volume = {3},
##    number = {2},
##    pages = {149--164},
##    author = {Buning, Pieter G.},
## }

function xyz = stream3 (varargin)

  options = [];
  switch (numel (varargin))
    case {6,7}
      if (numel (varargin) == 6)
        [u, v, w, spx, spy, spz] = varargin{:};
      else
        [u, v, w, spx, spy, spz, options] = varargin{:};
      endif
      [m, n, p] = size (u);
      [x, y, z] = meshgrid (1:n, 1:m, 1:p);
    case 9
      [x, y, z, u, v, w, spx, spy, spz] = varargin{:};
    case 10
      [x, y, z, u, v, w, spx, spy, spz, options] = varargin{:};
    otherwise
      print_usage ();
  endswitch

  stepsize = 0.1;
  max_vertices = 10_000;
  if (! isempty (options))
    switch (numel (options))
      case 1
        stepsize = options(1);
      case 2
        stepsize = options(1);
        max_vertices = options(2);
      otherwise
        error ("stream3: OPTIONS must be a 1- or 2-element vector");
    endswitch

    if (! isreal (stepsize) || stepsize == 0)
      error ("stream2: STEPSIZE must be a real scalar != 0");
    endif
    if (! isreal (max_vertices) || max_vertices < 1)
      error ("stream2: MAX_VERTICES must be an integer > 0");
    endif
    max_vertices = fix (max_vertices);
  endif

  if (! (size_equal (u, v, w, x, y, z) && size_equal (spx, spy, spz)))
    error ("stream3: matrix dimensions must match");
  endif
  if (iscomplex (u) || iscomplex (v) || iscomplex (w)
      || iscomplex (x) || iscomplex (y) || iscomplex (z)
      || iscomplex (spx) || iscomplex (spy) || iscomplex (spz))
    error ("stream3: all inputs must be real-valued");
  endif

  gx = x(1, :, 1);
  gy = y(:, 1, 1).';
  tmp = z(1, 1, :);
  gz = tmp(:).';

  ## Jacobian Matrix
  dx = diff (gx);
  dy = diff (gy);
  dz = diff (gz);
  ## "<" used to check if the mesh is ascending
  if (any (dx <= 0) || any (dy <= 0) || any (dz <= 0)
      || any (isnan (dx)) || any (isnan (dy)) || any (isnan (dz)))
    error ("stream3: non-monotonically increasing or NaN values found in mesh");
  endif
  tx = 1 ./ dx;
  ty = 1 ./ dy;
  tz = 1 ./ dz;
  ## "Don't cares" used for handling points located on the border
  tx(end + 1) = 0;
  ty(end + 1) = 0;
  tz(end + 1) = 0;
  dx(end + 1) = 0;
  dy(end + 1) = 0;
  dz(end + 1) = 0;

  px = spx(:);
  py = spy(:);
  pz = spz(:);

  for nseed = 1 : numel (px)

    xp = px(nseed);
    yp = py(nseed);
    zp = pz(nseed);
    idx = find (diff (gx <= xp), 1);
    if (gx(end) == xp)
      idx = numel (gx);
    endif
    idy = find (diff (gy <= yp), 1);
    if (gy(end) == yp)
      idy = numel (gy);
    endif
    idz = find (diff (gz <= zp), 1);
    if (gz(end) == zp)
      idz = numel (gz);
    endif

    if (isempty (idx) || isempty (idy) || isempty (idz))
      xyz{nseed} = [];
    else
      ## Transform seed from P coordinates to C coordinates
      zeta = (idx - 1) + (xp - gx(idx)) * tx(idx);
      xi = (idy - 1) + (yp - gy(idy)) * ty(idy);
      rho = (idz - 1) + (zp - gz(idz)) * tz(idz);

      C = __streameuler3d__ (u, v, w, tx, ty, tz, zeta, xi, rho, ...
                             stepsize, max_vertices);

      ## Transform from C coordinates to P coordinates
      idu = floor (C(:, 1));
      idv = floor (C(:, 2));
      idw = floor (C(:, 3));
      xyz{nseed} = [gx(idu + 1).' + (C(:, 1) - idu).*(dx(idu + 1).'), ...
                    gy(idv + 1).' + (C(:, 2) - idv).*(dy(idv + 1).'), ...
                    gz(idw + 1).' + (C(:, 3) - idw).*(dz(idw + 1).')];
    endif

  endfor

endfunction


%!demo
%! clf;
%! [x, y, z] = meshgrid (-30:1:30, -30:1:30, 0:1:50);
%! s = 10;
%! b = 8 / 3;
%! r = 28;
%! u = s * (y - x);
%! v = r * x - y - x.*z;
%! w = x.*y - b * z;
%! hold on;
%! sx = 0.1;
%! sy = 0.1;
%! sz = 0.1;
%! plot3 (sx, sy, sz, ".r", "markersize", 15);
%! h = streamline (x, y, z, u, v, w, sx, sy, sz, [0.1, 50000]);
%! set (h, "color", "r");
%! view (3);
%! title ("Lorenz System");
%! grid on;
%! axis equal;

%!test
%! [u, v, w] = meshgrid (0:3, 0:3, 0:3);
%! xyz = stream3 (u, v, w, 2, 2, 2, [0.01,5]);
%! assert (numel (xyz{:}), 15);

## Test input validation
%!error <Invalid call> stream3 ()
%!error <Invalid call> stream3 (1)
%!error <Invalid call> stream3 (1,2)
%!error <Invalid call> stream3 (1,2,3)
%!error <Invalid call> stream3 (1,2,3,4)
%!error <Invalid call> stream3 (1,2,3,4,5)
%!error <Invalid call> stream3 (1,2,3,4,5,6,7,8)
%!error <OPTIONS must be a 1- or 2-element> stream3 (1,2,3,4,5,6, [1,2,3])
%!error <STEPSIZE must be a real scalar != 0> stream3 (1,2,3,4,5,6, [1i])
%!error <STEPSIZE must be a real scalar != 0> stream3 (1,2,3,4,5,6, [0])
%!error <MAX_VERTICES must be an integer> stream3 (1,2,3,4,5,6, [1, 1i])
%!error <MAX_VERTICES must be an integer> stream3 (1,2,3,4,5,6, [1, 0])
%!error <matrix dimensions must match> stream3 ([1 1],2,3,4,5,6)
%!error <matrix dimensions must match> stream3 (1,[2 2],3,4,5,6)
%!error <matrix dimensions must match> stream3 (1,2,[3 3],4,5,6)
%!error <matrix dimensions must match> stream3 (1,2,3,[4 4],5,6)
%!error <matrix dimensions must match> stream3 (1,2,3,4,[5 5],6)
%!error <matrix dimensions must match> stream3 (1,2,3,4,5,[6 6])
%!error <all inputs must be real-valued> stream3 (1i,2,3,4,5,6)
%!error <all inputs must be real-valued> stream3 (1,2i,3,4,5,6)
%!error <all inputs must be real-valued> stream3 (1,2,3i,4,5,6)
%!error <all inputs must be real-valued> stream3 (1,2,3,4i,5,6)
%!error <all inputs must be real-valued> stream3 (1,2,3,4,5i,6)
%!error <all inputs must be real-valued> stream3 (1,2,3,4,5,6i)
%!error <non-monotonically increasing or NaN values found in mesh>
%! stream3 ([2 1], [1 2], [3 3], [4 4], [5 5], [6 6], [7 7], [8 8], [9 9]);
%!error <non-monotonically increasing or NaN values found in mesh>
%! stream3 ([1 NaN], [1 2], [3 3], [4 4], [5 5], [6 6], [7 7], [8 8], [9 9]);
## FIXME: vectors representing x, y, z mesh are not accepted.
%#!error <non-monotonically increasing or NaN values found in mesh>
%! stream3 ([1 2], [2 1], [3 3], [4 4], [5 5], [6 6], [7 7], [8 8], [9 9]);
%#!error <non-monotonically increasing or NaN values found in mesh>
%! stream3 ([1 2], [1 NaN], [3 3], [4 4], [5 5], [6 6], [7 7], [8 8], [9 9]);
%#!error <non-monotonically increasing or NaN values found in mesh>
%! stream3 ([1 2], [1 2], [2 1], [4 4], [5 5], [6 6], [7 7], [8 8], [9 9]);
%#!error <non-monotonically increasing or NaN values found in mesh>
%! stream3 ([1 2], [1 2], [1 NaN], [4 4], [5 5], [6 6], [7 7], [8 8], [9 9]);
