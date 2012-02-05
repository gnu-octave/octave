## Copyright (C) 2009-2012 Kai Habel
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
## @deftypefn  {Function File} {} surfl (@var{x}, @var{y}, @var{z})
## @deftypefnx {Function File} {} surfl (@var{z})
## @deftypefnx {Function File} {} surfl (@var{x}, @var{y}, @var{z}, @var{L})
## @deftypefnx {Function File} {} surfl (@var{x}, @var{y}, @var{z}, @var{L}, @var{P})
## @deftypefnx {Function File} {} surfl (@dots{}, "light")
## Plot a lighted surface given matrices @var{x}, and @var{y} from
## @code{meshgrid} and
## a matrix @var{z} corresponding to the @var{x} and @var{y} coordinates of
## the mesh.  If @var{x} and @var{y} are vectors, then a typical vertex
## is (@var{x}(j), @var{y}(i), @var{z}(i,j)).  Thus, columns of @var{z}
## correspond to different @var{x} values and rows of @var{z} correspond
## to different @var{y} values.
##
## The light direction can be specified using @var{L}.  It can be
## given as 2-element vector [azimuth, elevation] in degrees or as 3-element
## vector [lx, ly, lz].
## The default value is rotated 45° counter-clockwise from the current view.
##
## The material properties of the surface can specified using a 4-element vector
## @var{P} = [@var{AM} @var{D} @var{SP} @var{exp}] which defaults to
## @var{p} = [0.55 0.6 0.4 10].
## @table @asis
## @item "AM" strength of ambient light
##
## @item "D" strength of diffuse reflection
##
## @item "SP" strength of specular reflection
##
## @item "EXP" specular exponent
## @end table
##
## The default lighting mode "cdata", changes the cdata property to give the
## impression
## of a lighted surface.  Please note: the alternative "light" mode, which
## creates a light
## object to illuminate the surface is not implemented (yet).
##
## Example:
##
## @example
## @group
## colormap (bone (64));
## surfl (peaks);
## shading interp;
## @end group
## @end example
## @seealso{surf, diffuse, specular, surface}
## @end deftypefn

## Author: Kai Habel <kai.habel@gmx.de>

function retval = surfl (varargin)

  [h, varargin] = __plt_get_axis_arg__ ("surfl", varargin{:});

  oldh = gca ();
  unwind_protect
    axes (h);
    newplot ();

    ## Check for lighting type.
    use_cdata = true;
    if (ischar (varargin{end}))
      lstr = varargin{end};
      if (strncmp (tolower (lstr), "light", 5))
        warning ("light method not supported (yet), using cdata method instead");
        ## This can be implemented when light objects are supported.
        use_cdata = false;
      elseif (strncmp (tolower (lstr), "cdata", 5))
        use_cdata = true;
      else
        error ("surfl: unknown lighting method");
      endif
      varargin(end) = [];
    endif

    ## Check for reflection properties argument.
    ##
    ## r = [ambient light strength,
    ##      diffuse reflection strength,
    ##      specular reflection strength,
    ##      specular shine]
    if (length (varargin{end}) == 4 && isnumeric (varargin{end}))
      r = varargin{end};
      varargin(end) = [];
    else
      ## Default values.
      r = [0.55, 0.6, 0.4, 10];
    endif

    ## Check for light vector (lv) argument.
    have_lv = false;
    if (isnumeric (varargin{end}))
      len = numel (varargin{end});
      lastarg = varargin{end};
      if (len == 3)
        lv = lastarg;
        varargin(end) = [];
        have_lv = true;
      elseif (len == 2)
        [lv(1), lv(2), lv(3)] = sph2cart ((lastarg(1) - 90) * pi/180, lastarg(2) * pi/180, 1.0);
        varargin(end) = [];
        have_lv = true;
      endif
    endif

    tmp = surface (varargin{:});
    if (! ishold ())
      set (h, "view", [-37.5, 30],
           "xgrid", "on", "ygrid", "on", "zgrid", "on", "clim", [0 1]);
    endif

    ## Get view vector (vv).
    a = axis;
    [az, el] = view;
    [vv(1), vv(2), vv(3)] = sph2cart ((az - 90) * pi/180.0, el * pi/180.0, 1.0);
    vv /= norm (vv);

    if (!have_lv)
      ## Calculate light vector (lv) from view vector.
      Phi = 45.0 / 180.0 * pi;
      R = [cos(Phi), -sin(Phi), 0;
           sin(Phi),  cos(Phi), 0;
           0,          0,         1];
      lv = (R * vv.').';
    endif

    vn = get (tmp, "vertexnormals");
    dar = get (h, "plotboxaspectratio");
    vn(:,:,1) *= dar(1);
    vn(:,:,2) *= dar(2);
    vn(:,:,3) *= dar(3);

    ## Normalize vn.
    vn = vn ./ repmat (sqrt (sumsq (vn, 3)), [1, 1, 3]);
    [nr, nc] = size(get(tmp, "zdata"));

    ## Ambient, diffuse, and specular term.
    cdata = (r(1) * ones (nr, nc)
             + r(2) * diffuse  (vn(:,:,1), vn(:,:,2), vn(:,:,3), lv)
             + r(3) * specular (vn(:,:,1), vn(:,:,2), vn(:,:,3), lv, vv, r(4)));

    set (tmp, "cdata", cdata ./ sum (r(1:3)));

  unwind_protect_cleanup
    axes (oldh);
  end_unwind_protect

  if (nargout > 0)
    retval = tmp;
  endif

endfunction

%!demo
%! clf
%! [X,Y,Z]=sombrero;
%! colormap(copper);
%! surfl(X,Y,Z);
%! shading interp;

%!demo
%! clf
%! [X,Y,Z]=sombrero;
%! colormap(copper);
%! [az, el] = view;
%! surfl(X,Y,Z,[az+225,el],[0.2 0.6 0.4 25]);
%! shading interp;

