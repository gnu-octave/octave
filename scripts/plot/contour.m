## Copyright (C) 1993-2012 Shai Ayal
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
## @deftypefn  {Function File} {} contour (@var{z})
## @deftypefnx {Function File} {} contour (@var{z}, @var{vn})
## @deftypefnx {Function File} {} contour (@var{x}, @var{y}, @var{z})
## @deftypefnx {Function File} {} contour (@var{x}, @var{y}, @var{z}, @var{vn})
## @deftypefnx {Function File} {} contour (@dots{}, @var{style})
## @deftypefnx {Function File} {} contour (@var{h}, @dots{})
## @deftypefnx {Function File} {[@var{c}, @var{h}] =} contour (@dots{})
## Plot level curves (contour lines) of the matrix @var{z}, using the
## contour matrix @var{c} computed by @code{contourc} from the same
## arguments; see the latter for their interpretation.  The set of
## contour levels, @var{c}, is only returned if requested.  For example:
##
## @example
## @group
## x = 0:2;
## y = x;
## z = x' * y;
## contour (x, y, z, 2:3)
## @end group
## @end example
##
## The style to use for the plot can be defined with a line style @var{style}
## in a similar manner to the line styles used with the @code{plot} command.
## Any markers defined by @var{style} are ignored.
##
## The optional input and output argument @var{h} allows an axis handle to
## be passed to @code{contour} and the handles to the contour objects to be
## returned.
## @seealso{contourc, patch, plot}
## @end deftypefn

## Author: Shai Ayal <shaiay@users.sourceforge.net>

function [c, h] = contour (varargin)

  [xh, varargin] = __plt_get_axis_arg__ ("contour", varargin{:});

  oldh = gca ();
  unwind_protect
    axes (xh);
    newplot ();
    [ctmp, htmp] = __contour__ (xh, "none", varargin{:});
  unwind_protect_cleanup
    axes (oldh);
  end_unwind_protect

  if (nargout > 0)
    c = ctmp;
    h = htmp;
  endif

endfunction

%!demo
%! clf ()
%! [x, y, z] = peaks ();
%! contour (x, y, z);

%!demo
%! clf ()
%! [theta, r] = meshgrid (linspace (0, 2*pi, 64), linspace(0,1,64));
%! [X, Y] = pol2cart (theta, r);
%! Z = sin(2*theta).*(1-r);
%! contour(X, Y, abs(Z), 10)

%!demo
%! clf ()
%! x = linspace (-2, 2);
%! [x, y] = meshgrid (x);
%! z = sqrt (x.^2 + y.^2) ./ (x.^2 + y.^2+1);
%! contourf (x, y, z, [0.4, 0.4])
%! title ("The hole should be filled with the background color")

