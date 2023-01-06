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
## @deftypefn  {} {} streamtube (@var{x}, @var{y}, @var{z}, @var{u}, @var{v}, @var{w}, @var{sx}, @var{sy}, @var{sz})
## @deftypefnx {} {} streamtube (@var{u}, @var{v}, @var{w}, @var{sx}, @var{sy}, @var{sz})
## @deftypefnx {} {} streamtube (@var{xyz}, @var{x}, @var{y}, @var{z}, @var{div})
## @deftypefnx {} {} streamtube (@var{xyz}, @var{div})
## @deftypefnx {} {} streamtube (@var{xyz}, @var{dia})
## @deftypefnx {} {} streamtube (@dots{}, @var{options})
## @deftypefnx {} {} streamtube (@var{hax}, @dots{})
## @deftypefnx {} {@var{h} =} streamtube (@dots{})
## Plot tubes scaled by the divergence along streamlines.
##
## @code{streamtube} draws tubes whose diameter is scaled by the divergence of
## a vector field.  The vector field is given by
## @code{[@var{u}, @var{v}, @var{w}]} and is defined over a rectangular grid
## given by @code{[@var{x}, @var{y}, @var{z}]}.  The tubes start at the
## seed points @code{[@var{sx}, @var{sy}, @var{sz}]} and are plot along
## streamlines.
##
## @code{streamtube} can also be called with a cell array containing
## pre-computed streamline data.  To do this, @var{xyz} must be created with
## the @code{stream3} command.  @var{div} is used to scale the tubes.
## In order to plot tubes scaled by the vector field divergence, @var{div}
## must be calculated with the @code{divergence} command.
##
## A tube diameter of zero corresponds to the smallest scaling value along the
## streamline and the largest tube diameter corresponds to the largest scaling
## value.
##
## It is also possible to draw a tube along an arbitrary array of vertices
## @var{xyz}.  The tube diameter can be specified by the vertex array @var{dia}
## or by a constant.
##
## The input parameter @var{options} is a 2-D vector of the form
## @code{[@var{scale}, @var{n}]}.  The first parameter scales the tube
## diameter (default 1).  The second parameter specifies the number of vertices
## that are used to construct the tube circumference (default 20).
##
## If the first argument @var{hax} is an axes handle, then plot into this axes,
## rather than the current axes returned by @code{gca}.
##
## The optional return value @var{h} is a graphics handle to the plot objects
## created for each tube.
##
## @seealso{stream3, streamline, streamribbon, ostreamtube}
## @end deftypefn

function h = streamtube (varargin)

  [hax, varargin, nargin] = __plt_get_axis_arg__ ("streamtube", varargin{:});

  options = [];
  xyz = [];
  div = [];
  dia = [];
  switch (nargin)
    case 2
      ## "dia" can be a cell array or a constant
      if (iscell (varargin{2}) || numel (varargin{2}) == 1)
        [xyz, dia] = varargin{:};
      else
        [xyz, div] = varargin{:};
        [m, n, p] = size (div);
        [x, y, z] = meshgrid (1:n, 1:m, 1:p);
      endif
    case 3
      if (iscell (varargin{2}))
        [xyz, dia, options] = varargin{:};
      else
        [xyz, div, options] = varargin{:};
        [m, n, p] = size (div);
        [x, y, z] = meshgrid (1:n, 1:m, 1:p);
      endif
    case 5
        [xyz, x, y, z, div] = varargin{:};
    case 6
      if (iscell (varargin{1}))
        [xyz, x, y, z, div, options] = varargin{:};
      else
        [u, v, w, spx, spy, spz] = varargin{:};
        [m, n, p] = size (u);
        [x, y, z] = meshgrid (1:n, 1:m, 1:p);
      endif
    case 7
      [u, v, w, spx, spy, spz, options] = varargin{:};
      [m, n, p] = size (u);
      [x, y, z] = meshgrid (1:n, 1:m, 1:p);
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
        error ("streamtube: OPTIONS must be a 1- or 2-element vector");
    endswitch

    if (! isreal (scale) || scale <= 0)
      error ("streamtube: SCALE must be a real scalar > 0");
    endif
    if (! isreal (num_circum) || num_circum < 3)
      error ("streamtube: number of tube vertices N must be greater than 2");
    endif
    num_circum = fix (num_circum);
  endif

  if (isempty (xyz))
    xyz = stream3 (x, y, z, u, v, w, spx, spy, spz, 0.2);
  endif

  if (isempty (div) && isempty (dia))
    div = divergence (x, y, z, u, v, w);
  endif

  if (! isempty (dia) && iscell (dia))
    for i = 1 : length (xyz)
      if (rows (dia{i}) != rows (xyz{i}))
        error ("streamtube: DIA must have same length then XYZ");
      endif
    endfor
  endif

  if (isempty (hax))
    hax = gca ();
  else
    hax = hax(1);
  endif

  ## Derive final scale factor from the bounding box diagonal
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
  clen = scale * sqrt (dx*dx + dy*dy + dz*dz) / 40;

  h = [];
  for i = 1 : length (xyz)
    sl = xyz{i};
    num_vertices = rows (sl);
    if (! isempty (sl) && num_vertices > 1)
      if (isempty (dia))

        ## Plot a tube based on normalized divergence
        [div_sl, max_vertices] = interp_sl (x, y, z, div, sl);
        if (max_vertices > 1)
          ## Nomalize the divergence along the streamline
          mn = min (div_sl);
          mx = max (div_sl);
          if (mn == mx)
            radius_sl = clen * ones (max_vertices, 1);
          else
            radius_sl = clen * (div_sl - mn) / (mx - mn);
          endif
          htmp = plottube (hax, sl, radius_sl, max_vertices, num_circum);
          h = [h; htmp];
        endif

      else

        ## Plot a tube from external data (vertex array or constant)
        if (iscell (dia))
          radius_sl = 0.5 * scale * dia{i};
        else
          radius_sl = 0.5 * scale * dia * ones (1, num_vertices);
        endif
        htmp = plottube (hax, sl, radius_sl, num_vertices, num_circum);
        h = [h; htmp];

      endif
    endif
  endfor

endfunction

function h = plottube (hax, sl, radius_sl, max_vertices, num_circum)

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
  K = radius_sl(1) * KE;
  XS0 = rotation (K, RE, cp, sp) + repmat (X0.', 1, num_circum);
  K = radius_sl(2) * KE;
  XS = rotation (K, RE, cp, sp) + repmat (X1.', 1, num_circum);

  px = zeros (num_circum, max_vertices);
  py = zeros (num_circum, max_vertices);
  pz = zeros (num_circum, max_vertices);

  px(:,1) = XS0(1,:).';
  py(:,1) = XS0(2,:).';
  pz(:,1) = XS0(3,:).';

  px(:,2) = XS(1,:).';
  py(:,2) = XS(2,:).';
  pz(:,2) = XS(3,:).';

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
    K = radius_sl(i) * KE;

    ## Rotate around RE and collect surface patches
    XS = rotation (K, RE, cp, sp) + repmat (X1.', 1, num_circum);

    px(:,i) = XS(1,:).';
    py(:,i) = XS(2,:).';
    pz(:,i) = XS(3,:).';

  endfor

  h = surface (hax, px, py, pz);

endfunction

## Interpolate onto the streamline vertices and return the first chunck of
## valid samples until a singularity is hit (NaN or +-Inf) or
## the streamline vertex array "sl" ends
function [div_sl_crop, max_vertices] = interp_sl (x, y, z, div, sl)

  div_sl = interp3 (x, y, z, div, sl(:,1), sl(:,2), sl(:,3));
  is_nan = find (isnan (div_sl), 1, "first");
  is_inf = find (isinf (div_sl), 1, "first");

  max_vertices = rows (sl);
  if (! isempty (is_nan))
    max_vertices = min (max_vertices, is_nan - 1);
  endif
  if (! isempty (is_inf))
    max_vertices = min (max_vertices, is_inf - 1);
  endif

  div_sl_crop = div_sl(1 : max_vertices);

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
%! [x, y, z] = meshgrid (-3:0.15:3, -1:0.1:1, -1:0.1:1);
%! u = 2 + 8 * exp (-2.0*x.*x);
%! v = zeros (size (x));
%! w = zeros (size (x));
%! h = streamtube (x, y, z, u, v, w, -3, 0, 0, [5, 60]);
%! set (h, "facecolor", "r", "edgecolor", "none");
%! hold on;
%! camlight ();
%! lighting gouraud;
%! view (3);
%! grid on;
%! quiver3 (x, y, z, u, v, w);
%! axis tight equal;
%! title ("Divergence Plot");

%!demo
%! clf;
%! t = 0:.15:15;
%! xyz{1} = [cos(t)', sin(t)', (t/3)'];
%! dia{1} = cos(t)';
%! streamtube (xyz, dia);
%! grid on;
%! axis tight equal;
%! colormap (jet);
%! shading interp;
%! camlight ();
%! lighting gouraud;
%! view (3);
%! title ("Plot Arbitrary Tube");

## Test input validation
%!error <Invalid call> streamtube ()
%!error <Invalid call> streamtube (1)
%!error <Invalid call> streamtube (1,2,3,4)
%!error <Invalid call> streamtube (1,2,3,4,5,6,7,8)
%!error <Invalid call> streamtube (1,2,3,4,5,6,7,8,9,10,11)
%!error <OPTIONS must be a 1- or 2-element vector> streamtube (1,2,[1,2,3])
%!error <SCALE must be a real scalar . 0> streamtube (1,2,[1i])
%!error <SCALE must be a real scalar . 0> streamtube (1,2,[0])
%!error <SCALE must be a real scalar . 0> streamtube (1,2,[-1])
%!error <N must be greater than 2> streamtube (1,2,[1,1i])
%!error <N must be greater than 2> streamtube (1,2,[1,2])
%!error <DIA must have same length then XYZ> streamtube ({[1,1,1;2,2,2]},{[1,1,1]})
