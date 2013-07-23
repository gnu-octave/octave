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
## @seealso{ezsurfc, meshgrid, surf, contour}
## @end deftypefn

function h = surfc (varargin)

  [hax, varargin, nargin] = __plt_get_axis_arg__ ("surfc", varargin{:});

  if (nargin < 1)
    print_usage ();
  endif

  oldfig = ifelse (isempty (hax), [], get (0, "currentfigure"));
  unwind_protect
    hax = newplot (hax);
    
    htmp = surface (hax, varargin{:});

    set (htmp, "facecolor", "flat");

    if (! ishold ())
      set (hax, "view", [-37.5, 30],
                "xgrid", "on", "ygrid", "on", "zgrid", "on");
    endif

    drawnow ();

    # don't pass string arguments to __contour__()
    stop_idx = find (cellfun ("isclass", varargin, "char"), 1);
    if (isempty (stop_idx))
      stop_idx = nargin;
    else
      stop_idx--;
    endif

    if (stop_idx - 1 == 1 || stop_idx - 1 == 3)
      ## Don't pass a color matrix c to __contour__
      stop_idx -= 1;
    endif

    zmin = get (hax, "zlim")(1);
    [~, htmp2] = __contour__ (hax, zmin, varargin{1:stop_idx});

    htmp = [htmp; htmp2];

  unwind_protect_cleanup
    if (! isempty (oldfig))
      set (0, "currentfigure", oldfig);
    endif
  end_unwind_protect

  if (nargout > 0)
    h = htmp;
  endif

endfunction


%!demo
%! clf;
%! colormap ('default');
%! [~,~,Z] = peaks ();
%! surfc (Z);

%!demo
%! clf;
%! colormap ('default');
%! [~,~,Z] = sombrero ();
%! [Fx,Fy] = gradient (Z);
%! surfc (Z, Fx+Fy);
%! shading interp;

%!demo
%! clf;
%! colormap ('default');
%! [X,Y,Z] = sombrero ();
%! [~,Fy] = gradient (Z);
%! surfc (X,Y,Z,Fy);
%! shading interp;

