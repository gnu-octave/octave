## Copyright (C) 1996-2012 John W. Eaton
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
## @deftypefn {Function File} {} surfc (@var{x}, @var{y}, @var{z})
## Plot a surface and contour given matrices @var{x}, and @var{y} from
## @code{meshgrid} and a matrix @var{z} corresponding to the @var{x} and
## @var{y} coordinates of the mesh.  If @var{x} and @var{y} are vectors,
## then a typical vertex is (@var{x}(j), @var{y}(i), @var{z}(i,j)).  Thus,
## columns of @var{z} correspond to different @var{x} values and rows of
## @var{z} correspond to different @var{y} values.
## @seealso{meshgrid, surf, contour}
## @end deftypefn

function h = surfc (varargin)

  newplot ();

  tmp = surface (varargin{:});

  ax = get (tmp, "parent");

  set (tmp, "facecolor", "flat");

  if (! ishold ())
    set (ax, "view", [-37.5, 30],
         "xgrid", "on", "ygrid", "on", "zgrid", "on");
  endif

  drawnow ();
  zmin = get (ax, "zlim")(1);

  # don't pass axis handle and/or string arguments to __contour__()
  stop_idx = nargin;
  for i = 2 : nargin
    if (ischar (varargin{i}))
      stop_idx = i - 1;
      break;
    endif
  endfor

  start_idx = 1;
  if (ishandle (varargin{1}))
    start_idx = 2;
  endif

  if (stop_idx - start_idx == 1 || stop_idx - start_idx == 3)
    #don't pass a color matrix c to __contour__
    stop_idx -= 1;
  endif

  [c, tmp2] = __contour__ (ax, zmin, varargin{start_idx:stop_idx});

  tmp = [tmp; tmp2];

  if (nargout > 0)
    h = tmp;
  endif

endfunction

%!demo
%! clf
%! [~,~,Z]=peaks;
%! surfc(Z);

%!demo
%! clf
%! [~,~,Z]=sombrero;
%! [Fx,Fy] = gradient(Z);
%! surfc(Z,Fx+Fy);
%! shading interp;

%!demo
%! clf
%! [X,Y,Z]=sombrero;
%! [~,Fy] = gradient(Z);
%! surfc(X,Y,Z,Fy);
%! shading interp;
