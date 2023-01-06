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
## @deftypefn  {} {} ostreamtube (@var{x}, @var{y}, @var{z}, @var{u}, @var{v}, @var{w}, @var{sx}, @var{sy}, @var{sz})
## @deftypefnx {} {} ostreamtube (@var{u}, @var{v}, @var{w}, @var{sx}, @var{sy}, @var{sz})
## @deftypefnx {} {} ostreamtube (@var{xyz}, @var{x}, @var{y}, @var{z}, @var{u}, @var{v}, @var{w})
## @deftypefnx {} {} ostreamtube (@dots{}, @var{options})
## @deftypefnx {} {} ostreamtube (@var{hax}, @dots{})
## @deftypefnx {} {@var{h} =} ostreamtube (@dots{})
## Calculate and display streamtubes.
##
## Streamtubes are approximated by connecting circular crossflow areas
## along a streamline.  The expansion of the flow is determined by the local
## crossflow divergence.
##
## The vector field is given by @code{[@var{u}, @var{v}, @var{w}]} and is
## defined over a rectangular grid given by @code{[@var{x}, @var{y}, @var{z}]}.
## The streamtubes start at the seed points
## @code{[@var{sx}, @var{sy}, @var{sz}]}.
##
## The tubes are colored based on the local vector field strength.
##
## The input parameter @var{options} is a 2-D vector of the form
## @code{[@var{scale}, @var{n}]}.  The first parameter scales the start radius
## of the streamtubes (default 1).  The second parameter specifies the number
## of vertices that are used to construct the tube circumference (default 20).
##
## @code{ostreamtube} can be called with a cell array containing pre-computed
## streamline data.  To do this, @var{xyz} must be created with the
## @code{stream3} function.  This option is useful if you need to alter the
## integrator step size or the maximum number of vertices of the streamline.
##
## If the first argument @var{hax} is an axes handle, then plot into this axes,
## rather than the current axes returned by @code{gca}.
##
## The optional return value @var{h} is a graphics handle to the plot
## objects created for each streamtube.
##
## Example:
##
## @example
## @group
## [x, y, z] = meshgrid (-1:0.1:1, -1:0.1:1, -3:0.1:0);
## u = -x / 10 - y;
## v = x - y / 10;
## w = - ones (size (x)) / 10;
## ostreamtube (x, y, z, u, v, w, 1, 0, 0);
## @end group
## @end example
##
## @seealso{stream3, streamline, streamribbon, streamtube}
## @end deftypefn

## References:
##
## @inproceedings{
##    title = {Visualization of 3-D vector fields - Variations on a stream},
##    author = {Dave Darmofal and Robert Haimes},
##    year = {1992}
## }
##
## @article{
##    title = {Efficient streamline, streamribbon, and streamtube constructions on unstructured grids},
##    author = {Ueng, Shyh-Kuang and Sikorski, C. and Ma, Kwan-Liu},
##    year = {1996},
##    month = {June},
##    publisher = {IEEE Transactions on Visualization and Computer Graphics},
## }

function h = ostreamtube (varargin)

  [hax, varargin, nargin] = __plt_get_axis_arg__ ("ostreamtube", varargin{:});

  options = [];
  xyz = [];
  switch (nargin)
    case 6
      [u, v, w, spx, spy, spz] = varargin{:};
      [m, n, p] = size (u);
      [x, y, z] = meshgrid (1:n, 1:m, 1:p);
    case 7
      if (iscell (varargin{1}))
        [xyz, x, y, z, u, v, w] = varargin{:};
      else
        [u, v, w, spx, spy, spz, options] = varargin{:};
        [m, n, p] = size (u);
        [x, y, z] = meshgrid (1:n, 1:m, 1:p);
      endif
    case 8
      [xyz, x, y, z, u, v, w, options] = varargin{:};
    case 9
      [x, y, z, u, v, w, spx, spy, spz] = varargin{:};
    case 10
      [x, y, z, u, v, w, spx, spy, spz, options] = varargin{:};
    otherwise
      print_usage ();
  endswitch

  scale = 1;
  num_circum = 20;
  if (! isempty (options))
    switch (numel (options))
      case 1
        scale = options(1);
      case 2
        scale = options(1);
        num_circum = options(2);
      otherwise
        error ("ostreamtube: OPTIONS must be a 1- or 2-element vector");
    endswitch

    if (! isreal (scale) || scale <= 0)
      error ("ostreamtube: SCALE must be a real scalar > 0");
    endif
    if (! isreal (num_circum) || num_circum < 3)
      error ("ostreamtube: number of tube vertices N must be greater than 2");
    endif
    num_circum = fix (num_circum);
  endif

  if (isempty (hax))
    hax = gca ();
  else
    hax = hax(1);
  endif

  if (isempty (xyz))
    xyz = stream3 (x, y, z, u, v, w, spx, spy, spz, 0.2);
  endif

  div = divergence (x, y, z, u, v, w);

  ## Use the bounding box diagonal to determine the starting radius
  mxx = mnx = mxy = mny = mxz = mnz = [];
  j = 1;
  for i = 1 : length (xyz)
    sl = xyz{i};
    if (! isempty (sl))
      slx = sl(:,1); sly = sl(:,2); slz = sl(:,3);
      mxx(j) = max (slx); mnx(j) = min (slx);
      mxy(j) = max (sly); mny(j) = min (sly);
      mxz(j) = max (slz); mnz(j) = min (slz);
      j += 1;
    endif
  endfor
  dx = max (mxx) - min (mnx);
  dy = max (mxy) - min (mny);
  dz = max (mxz) - min (mnz);
  rstart = scale * sqrt (dx*dx + dy*dy + dz*dz) / 25;

  h = [];
  for i = 1 : length (xyz)
    sl = xyz{i};
    num_vertices = rows (sl);
    if (! isempty (sl) && num_vertices > 2)

      usl = interp3 (x, y, z, u, sl(:,1), sl(:,2), sl(:,3));
      vsl = interp3 (x, y, z, v, sl(:,1), sl(:,2), sl(:,3));
      wsl = interp3 (x, y, z, w, sl(:,1), sl(:,2), sl(:,3));
      vv = sqrt (usl.*usl + vsl.*vsl + wsl.*wsl);

      div_sl = interp3 (x, y, z, div, sl(:,1), sl(:,2), sl(:,3));
      is_singular_div = find (isnan (div_sl), 1, "first");

      if (! isempty (is_singular_div))
        max_vertices = is_singular_div - 1;
      else
        max_vertices = num_vertices;
      endif

      if (max_vertices > 2)

        htmp = plottube (hax, sl, div_sl, vv, max_vertices, ...
                         rstart, num_circum);
        h = [h; htmp];

      endif
    endif
  endfor

endfunction

function h = plottube (hax, sl, div_sl, vv, max_vertices, rstart, num_circum)

  phi = linspace (0, 2*pi, num_circum);
  cp = cos (phi);
  sp = sin (phi);

  ## 1st streamline segment
  X0 = sl(1,:);
  X1 = sl(2,:);
  R = X1 - X0;
  RE = R / norm (R);

  ## Guide point and its rotation to create a segment
  KE = get_normal1 (RE);
  K = rstart * KE;
  XS0 = rotation (K, RE, cp, sp) + repmat (X0.', 1, num_circum);

  ## End of first segment
  ract = rstart * exp (0.5 * div_sl(2) * norm (R) / vv(2)) * ...
                  sqrt (vv(1) / vv(2));
  rold = ract;
  K = ract * KE;
  XS = rotation (K, RE, cp, sp) + repmat (X1.', 1, num_circum);

  px = zeros (num_circum, max_vertices);
  py = zeros (num_circum, max_vertices);
  pz = zeros (num_circum, max_vertices);
  pc = zeros (num_circum, max_vertices);

  px(:,1) = XS0(1,:).';
  py(:,1) = XS0(2,:).';
  pz(:,1) = XS0(3,:).';
  pc(:,1) = vv(1) * ones (num_circum, 1);

  px(:,2) = XS(1,:).';
  py(:,2) = XS(2,:).';
  pz(:,2) = XS(3,:).';
  pc(:,2) = vv(2) * ones (num_circum, 1);

  for i = 3 : max_vertices

    ## Next streamline segment
    X0 = X1;
    X1 = sl(i,:);
    R = X1 - X0;
    RE = R / norm (R);

    ## Tube radius
    ract = rold * exp (0.5 * div_sl(i) * norm (R) / vv(i)) * ...
                  sqrt (vv(i-1) / vv(i));
    rold = ract;

    ## Project KE onto RE and get the difference in order to transport
    ## the normal vector KE along the vertex array
    Kp = KE - RE * dot (KE, RE);
    KE = Kp / norm (Kp);
    K = ract * KE;

    ## Rotate around RE and collect surface patches
    XS = rotation (K, RE, cp, sp) + repmat (X1.', 1, num_circum);

    px(:,i) = XS(1,:).';
    py(:,i) = XS(2,:).';
    pz(:,i) = XS(3,:).';
    pc(:,i) = vv(i) * ones (num_circum, 1);

  endfor

  h = surface (hax, px, py, pz, pc);

endfunction

## Arbitrary N normal to X
function N = get_normal1 (X)

  if ((X(3) == 0) && (X(1) == -X(2)))
    N = [(- X(2) - X(3)), X(1), X(1)];
  else
    N = [X(3), X(3), (- X(1) - X(2))];
  endif

  N /= norm (N);

endfunction

## Rotate X around U where |U| = 1
## cp = cos (angle), sp = sin (angle)
function Y = rotation (X, U, cp, sp)

  ux = U(1);
  uy = U(2);
  uz = U(3);

  Y(1,:) = X(1) * (cp + ux * ux * (1 - cp)) + ...
           X(2) * (ux * uy * (1 - cp) - uz * sp) + ...
           X(3) * (ux * uz * (1 - cp) + uy * sp);

  Y(2,:) = X(1) * (uy * ux * (1 - cp) + uz * sp) + ...
           X(2) * (cp + uy * uy * (1 - cp)) + ...
           X(3) * (uy * uz * (1 - cp) - ux * sp);

  Y(3,:) = X(1) * (uz * ux * (1 - cp) - uy * sp) + ...
           X(2) * (uz * uy * (1 - cp) + ux * sp) + ...
           X(3) * (cp + uz * uz * (1 - cp));

endfunction


%!demo
%! clf;
%! [x, y, z] = meshgrid (-1:0.1:1, -1:0.1:1, -3.5:0.1:0);
%! a = 0.1;
%! b = 0.1;
%! u = - a * x - y;
%! v = x - a * y;
%! w = - b * ones (size (x));
%! sx = 1.0;
%! sy = 0.0;
%! sz = 0.0;
%! ostreamtube (x, y, z, u, v, w, sx, sy, sz, [1.2, 30]);
%! colormap (jet);
%! shading interp;
%! view ([-47, 24]);
%! camlight ();
%! lighting gouraud;
%! grid on;
%! view (3);
%! axis equal;
%! set (gca, "cameraviewanglemode", "manual");
%! title ("Spiral Sink");

%!demo
%! clf;
%! [x, y, z] = meshgrid (-2:0.5:2);
%! t = sqrt (1.0./(x.^2 + y.^2 + z.^2)).^3;
%! u = - x.*t;
%! v = - y.*t;
%! w = - z.*t;
%! [sx, sy, sz] = meshgrid (-2:4:2);
%! xyz = stream3 (x, y, z, u, v, w, sx, sy, sz, [0.1, 60]);
%! ostreamtube (xyz, x, y, z, u, v, w, [2, 50]);
%! colormap (jet);
%! shading interp;
%! view ([-47, 24]);
%! camlight ();
%! lighting gouraud;
%! grid on;
%! view (3);
%! axis equal;
%! set (gca, "cameraviewanglemode", "manual");
%! title ("Integration Towards Sink");

## Test input validation
%!error <Invalid call> ostreamtube ()
%!error <Invalid call> ostreamtube (1)
%!error <Invalid call> ostreamtube (1,2)
%!error <Invalid call> ostreamtube (1,2,3)
%!error <Invalid call> ostreamtube (1,2,3,4)
%!error <Invalid call> ostreamtube (1,2,3,4,5)
%!error <OPTIONS must be a 1- or 2-element> ostreamtube (1,2,3,4,5,6,[1,2,3])
%!error <SCALE must be a real scalar . 0> ostreamtube (1,2,3,4,5,6,[1i])
%!error <SCALE must be a real scalar . 0> ostreamtube (1,2,3,4,5,6,[0])
%!error <N must be greater than 2> ostreamtube (1,2,3,4,5,6,[1,1i])
%!error <N must be greater than 2> ostreamtube (1,2,3,4,5,6,[1,2])
