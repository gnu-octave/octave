## Copyright (C) 2013 Mike Miller
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
## @deftypefn  {Function File} {} waterfall (@var{x}, @var{y}, @var{z})
## @deftypefnx {Function File} {} waterfall (@var{z})
## @deftypefnx {Function File} {@var{h} =} waterfall (@dots{})
## Plot a waterfall plot given matrices @var{x}, and @var{y} from
## @code{meshgrid} and a matrix @var{z} corresponding to the @var{x} and
## @var{y} coordinates of the mesh.  If @var{x} and @var{y} are vectors,
## then a typical vertex is (@var{x}(j), @var{y}(i), @var{z}(i,j)).  Thus,
## columns of @var{z} correspond to different @var{x} values and rows of
## @var{z} correspond to different @var{y} values.
##
## The optional return value @var{h} is a graphics handle to the created
## surface object.
## @seealso{meshgrid, meshz, surf}
## @end deftypefn

## Author: Mike Miller <mtmiller@ieee.org>

function h = waterfall (varargin)

  tmp = meshz (varargin{:});

  set (tmp, "meshstyle", "row");

  ## The gnuplot toolkit does nothing with the meshstyle property currently.
  toolkit = get (ancestor (tmp, "figure"), "__graphics_toolkit__");
  if (strcmp (toolkit, "gnuplot"))
    warning ("waterfall: may not render correctly using toolkit '%s'", toolkit);
  endif

  if (nargout > 0)
    h = tmp;
  endif

endfunction


%!demo
%! clf;
%! colormap ('default');
%! [~,~,Z] = peaks ();
%! waterfall (Z);

