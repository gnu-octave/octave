########################################################################
##
## Copyright (C) 2020-2023 The Octave Project Developers
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
## @deftypefn  {} {} streamribbon (@var{x}, @var{y}, @var{z}, @var{u}, @var{v}, @var{w}, @var{sx}, @var{sy}, @var{sz})
## @deftypefnx {} {} streamribbon (@var{u}, @var{v}, @var{w}, @var{sx}, @var{sy}, @var{sz})
## @deftypefnx {} {} streamribbon (@var{xyz}, @var{x}, @var{y}, @var{z}, @var{anlr_spd}, @var{lin_spd})
## @deftypefnx {} {} streamribbon (@var{xyz}, @var{anlr_spd}, @var{lin_spd})
## @deftypefnx {} {} streamribbon (@var{xyz}, @var{anlr_rot})
## @deftypefnx {} {} streamribbon (@dots{}, @var{width})
## @deftypefnx {} {} streamribbon (@var{hax}, @dots{})
## @deftypefnx {} {@var{h} =} streamribbon (@dots{})
## Calculate and display streamribbons.
##
## The streamribbon is constructed by rotating a normal vector around a
## streamline according to the angular rotation of the vector field.
##
## The vector field is given by @code{[@var{u}, @var{v}, @var{w}]} and is
## defined over a rectangular grid given by @code{[@var{x}, @var{y}, @var{z}]}.
## The streamribbons start at the seed points
## @code{[@var{sx}, @var{sy}, @var{sz}]}.
##
## @code{streamribbon} can be called with a cell array that contains
## pre-computed streamline data.  To do this, @var{xyz} must be created with
## the @code{stream3} function.  @var{lin_spd} is the linear speed of the
## vector field and can be calculated from @code{[@var{u}, @var{v}, @var{w}]}
## by the square root of the sum of the squares.  The angular speed
## @var{anlr_spd} is the projection of the angular velocity onto the velocity
## of the normalized vector field and can be calculated with the @code{curl}
## command.  This option is useful if you need to alter the integrator step
## size or the maximum number of streamline vertices.
##
## Alternatively, ribbons can be created from an array of vertices @var{xyz} of
## a path curve.  @var{anlr_rot} contains the angles of rotation around the
## edges between adjacent vertices of the path curve.
##
## The input parameter @var{width} sets the width of the streamribbons.
##
## Streamribbons are colored according to the total angle of rotation along the
## ribbon.
##
## If the first argument @var{hax} is an axes handle, then plot into this axes,
## rather than the current axes returned by @code{gca}.
##
## The optional return value @var{h} is a graphics handle to the plot objects
## created for each streamribbon.
##
## Example:
##
## @example
## @group
## [x, y, z] = meshgrid (0:0.2:4, -1:0.2:1, -1:0.2:1);
## u = - x + 10;
## v = 10 * z.*x;
## w = - 10 * y.*x;
## streamribbon (x, y, z, u, v, w, [0, 0], [0, 0.6], [0, 0]);
## view (3);
## @end group
## @end example
##
## @seealso{streamline, stream3, streamtube, ostreamtube}
##
## @end deftypefn

## References:
##
## @inproceedings{
##    title = {Feature Detection from Vector Quantities in a Numerically Simulated Hypersonic Flow Field in Combination with Experimental Flow Visualization},
##    author = {Pagendarm, Hans-Georg and Walter, Birgit},
##    year = {1994},
##    publisher = {IEEE Computer Society Press},
##    booktitle = {Proceedings of the Conference on Visualization ’94},
##    pages = {117–123},
## }
##
## @article{
##    title = {Efficient streamline, streamribbon, and streamtube constructions on unstructured grids},
##    author = {Ueng, Shyh-Kuang and Sikorski, C. and Ma, Kwan-Liu},
##    year = {1996},
##    month = {June},
##    publisher = {IEEE Transactions on Visualization and Computer Graphics},
## }
##
## @inproceedings{
##    title = {Visualization of 3-D vector fields - Variations on a stream},
##    author = {Dave Darmofal and Robert Haimes},
##    year = {1992}
## }
##
## @techreport{
##    title = {Parallel Transport Approach to Curve Framing},
##    author = {Andrew J. Hanson and Hui Ma},
##    year = {1995}
## }
##
## @article{
##    title = {There is More than One Way to Frame a Curve},
##    author = {Bishop, Richard},
##    year = {1975},
##    month = {03},
##    volume = {82},
##    publisher = {The American Mathematical Monthly}
## }

function h = streamribbon (varargin)

  [hax, varargin, nargin] = __plt_get_axis_arg__ ("streamribbon", varargin{:});

  width = [];
  xyz = [];
  anlr_spd = [];
  lin_spd = [];
  anlr_rot = [];
  switch (nargin)
    case 2
      [xyz, anlr_rot] = varargin{:};
    case 3
      if (numel (varargin{3}) == 1)
        [xyz, anlr_rot, width] = varargin{:};
      else
        [xyz, anlr_spd, lin_spd] = varargin{:};
        [m, n, p] = size (anlr_spd);
        [x, y, z] = meshgrid (1:n, 1:m, 1:p);
      endif
    case 4
      [xyz, anlr_spd, lin_spd, width] = varargin{:};
      [m, n, p] = size (anlr_spd);
      [x, y, z] = meshgrid (1:n, 1:m, 1:p);
    case 6
      if (iscell (varargin{1}))
        [xyz, x, y, z, anlr_spd, lin_spd] = varargin{:};
      else
        [u, v, w, spx, spy, spz] = varargin{:};
        [m, n, p] = size (u);
        [x, y, z] = meshgrid (1:n, 1:m, 1:p);
      endif
    case 7
      if (iscell (varargin{1}))
        [xyz, x, y, z, anlr_spd, lin_spd, width] = varargin{:};
      else
        [u, v, w, spx, spy, spz, width] = varargin{:};
        [m, n, p] = size (u);
        [x, y, z] = meshgrid (1:n, 1:m, 1:p);
      endif
    case 9
      [x, y, z, u, v, w, spx, spy, spz] = varargin{:};
    case 10
      [x, y, z, u, v, w, spx, spy, spz, width] = varargin{:};
    otherwise
      print_usage ();
  endswitch

  if (isempty (xyz))
    xyz = stream3 (x, y, z, u, v, w, spx, spy, spz);
    anlr_spd = curl (x, y, z, u, v, w);
    lin_spd = sqrt (u.*u + v.*v + w.*w);
  endif

  ## Derive scale factor from the bounding box diagonal
  if (isempty (width))
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
    width = sqrt (dx*dx + dy*dy + dz*dz) / 25;
  elseif (! isreal (width) || width <= 0)
    error ("streamribbon: WIDTH must be a real scalar > 0");
  endif

  if (! isempty (anlr_rot))
    for i = 1 : length (xyz)
      if (rows (anlr_rot{i}) != rows (xyz{i}))
        error ("streamribbon: ANLR_ROT must have same length as XYZ");
      endif
    endfor
  endif

  if (isempty (hax))
    hax = gca ();
  else
    hax = hax(1);
  endif

  ## Angular speed of a paddle wheel spinning around a streamline in a fluid
  ## flow "V":
  ## dtheta/dt = 0.5 * <curl(V), V/norm(V)>
  ##
  ## Integration along a streamline segment with the length "h" yields the
  ## rotation angle:
  ## theta = 0.25 * h * <curl(V), V(0)/norm(V(0))^2) + V(h)/norm(V(h))^2)>
  ##
  ## Alternative approach using the curl angular speed "c = curl()":
  ## theta = 0.5 * h * (c(0)/norm(V(0)) + c(h)/norm(V(h)))
  ##
  ## Hints:
  ## i. ) For integration use trapezoidal rule
  ## ii.) "V" can be assumend to be piecewise linear and curl(V) to be
  ##      piecewise constant because of the used linear interpolation

  h = [];
  for i = 1 : length (xyz)
    sl = xyz{i};
    num_vertices = rows (sl);
    if (! isempty (sl) && num_vertices > 1)
      if (isempty (anlr_rot))
        ## Plot from vector field
        ## Interpolate onto streamline vertices
        [lin_spd_sl, anlr_spd_sl, max_vertices] = ...
                                  interp_sl (x, y, z, lin_spd, anlr_spd, sl);
        if (max_vertices > 1)
          ## Euclidean distance between two adjacent vertices
          stepsize = vecnorm (diff (sl(1:max_vertices, :)), 2, 2);
          ## Angular rotation around edges between two adjacent sl-vertices
          ## Note: Potential "division by zero" is checked in interp_sl()
          anlr_rot_sl = 0.5 * stepsize.*(anlr_spd_sl(1:max_vertices - 1)./ ...
                                         lin_spd_sl(1:max_vertices - 1) + ...
                                         anlr_spd_sl(2:max_vertices)./ ...
                                         lin_spd_sl(2:max_vertices));

          htmp = plotribbon (hax, sl, anlr_rot_sl, max_vertices, 0.5 * width);
          h = [h; htmp];
        endif
      else
          ## Plot from vertice array
          anlr_rot_sl = anlr_rot{i};

          htmp = plotribbon (hax, sl, anlr_rot_sl, num_vertices, 0.5 * width);
          h = [h; htmp];
      endif
    endif
  endfor

endfunction

function h = plotribbon (hax, sl, anlr_rot_sl, max_vertices, width2)

  total_angle = cumsum (anlr_rot_sl);
  total_angle = [0; total_angle];

  ## 1st streamline segment
  X0 = sl(1,:);
  X1 = sl(2,:);
  R = X1 - X0;
  RE = R / norm (R);

  ## Initial vector KE which is to be transported along the vertice array
  KE = get_normal2 (RE);
  XS10 = - width2 * KE + X0;
  XS20 = width2 * KE + X0;

  ## Apply angular rotation
  cp = cos (anlr_rot_sl(1));
  sp = sin (anlr_rot_sl(1));
  KE = rotation (KE, RE, cp, sp).';

  XS1 = - width2 * KE + X1;
  XS2 = width2 * KE + X1;

  px = zeros (2, max_vertices);
  py = zeros (2, max_vertices);
  pz = zeros (2, max_vertices);
  pc = zeros (2, max_vertices);

  px(:,1) = [XS10(1); XS20(1)];
  py(:,1) = [XS10(2); XS20(2)];
  pz(:,1) = [XS10(3); XS20(3)];
  pc(:,1) = total_angle(1) * [1; 1];

  px(:,2) = [XS1(1); XS2(1)];
  py(:,2) = [XS1(2); XS2(2)];
  pz(:,2) = [XS1(3); XS2(3)];
  pc(:,2) = total_angle(2) * [1; 1];

  for i = 3 : max_vertices

    ## Next streamline segment
    X0 = X1;
    X1 = sl(i,:);
    R = X1 - X0;
    RE = R / norm (R);

    ## Project KE onto RE and get the difference in order to transport
    ## the normal vector KE along the vertex array
    Kp = KE - RE * dot (KE, RE);
    KE = Kp / norm (Kp);

    ## Apply angular rotation to KE
    cp = cos (anlr_rot_sl(i - 1));
    sp = sin (anlr_rot_sl(i - 1));
    KE = rotation (KE, RE, cp, sp).';

    XS1 = - width2 * KE + X1;
    XS2 = width2 * KE + X1;

    px(:,i) = [XS1(1); XS2(1)];
    py(:,i) = [XS1(2); XS2(2)];
    pz(:,i) = [XS1(3); XS2(3)];
    pc(:,i) = total_angle(i) * [1; 1];

  endfor

  h = surface (hax, px, py, pz, pc);

endfunction

## Interpolate speed and divergence onto the streamline vertices and
## return the first chunck of valid samples until a singularity /
## zero is hit or the streamline vertex array "sl" ends
function [lin_spd_sl, anlr_spd_sl, max_vertices] = ...
                               interp_sl (x, y, z, lin_spd, anlr_spd, sl)

  anlr_spd_sl = interp3 (x, y, z, anlr_spd, sl(:,1), sl(:,2), sl(:,3));
  lin_spd_sl = interp3 (x, y, z, lin_spd, sl(:,1), sl(:,2), sl(:,3));

  is_singular_anlr_spd = find (isnan (anlr_spd_sl), 1, "first");
  is_zero_lin_spd = find (lin_spd_sl == 0, 1, "first");

  max_vertices = rows (sl);
  if (! isempty (is_singular_anlr_spd))
    max_vertices = min (max_vertices, is_singular_anlr_spd - 1);
  endif
  if (! isempty (is_zero_lin_spd))
    max_vertices = min (max_vertices, is_zero_lin_spd - 1);
  endif

endfunction

## N normal to X, so that N is in span ([0 0 1], X)
## If not possible then span ([1 0 0], X)
function N = get_normal2 (X)

  if ((X(1) == 0) && (X(2) == 0))
    A = [1, 0, 0];
  else
    A = [0, 0, 1];
  endif

  ## Project A onto X and get the difference
  N = A - X * dot (A, X) / (norm (X)^2);
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
%! [x, y, z] = meshgrid (0:0.2:4, -1:0.2:1, -1:0.2:1);
%! u = - x + 10;
%! v = 10 * z.*x;
%! w = - 10 * y.*x;
%! sx = [0, 0];
%! sy = [0, 0.6];
%! sz = [0, 0];
%! streamribbon (x, y, z, u, v, w, sx, sy, sz);
%! hold on;
%! quiver3 (x, y, z, u, v, w);
%! colormap (jet);
%! shading interp;
%! camlight ("headlight");
%! view (3);
%! axis tight equal off;
%! set (gca, "cameraviewanglemode", "manual");
%! hcb = colorbar;
%! title (hcb, "Angle");
%! title ("Streamribbon");

%!demo
%! clf;
%! t = (0:pi/50:2*pi).';
%! xyz{1} = [cos(t), sin(t), 0*t];
%! twist{1} = ones (numel (t), 1) * pi / (numel (t) - 1);
%! streamribbon (xyz, twist, 0.5);
%! colormap (jet);
%! view (3);
%! camlight ("headlight");
%! axis tight equal off;
%! title ("Moebius Strip");

## Test input validation
%!error <Invalid call> streamribbon ()
%!error <Invalid call> streamribbon (1)
%!error <Invalid call> streamribbon (1,2,3,4,5)
%!error <Invalid call> streamribbon (1,2,3,4,5,6,7,8)
%!error <Invalid call> streamribbon (1,2,3,4,5,6,7,8,9,10,11)
%!error <WIDTH must be a real scalar . 0> streamribbon (1,2,3,1i)
%!error <WIDTH must be a real scalar . 0> streamribbon (1,2,3,0)
%!error <WIDTH must be a real scalar . 0> streamribbon (1,2,3,-1)
%!error <ANLR_ROT must have same length as XYZ> streamribbon ({[1,1,1;2,2,2]},{[1,1,1]})
