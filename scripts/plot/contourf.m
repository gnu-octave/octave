## Copyright (C) 2007-2012 Kai Habel
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
## @deftypefn  {Function File} {[@var{c}, @var{h}] =} contourf (@var{x}, @var{y}, @var{z}, @var{lvl})
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
## If @var{x} and @var{y} are omitted they are taken as the row/column
## index of @var{z}.  @var{n} is a scalar denoting the number of lines
## to compute.  Alternatively @var{lvl} is a vector containing the
## contour levels.  If only one value (e.g., lvl0) is wanted, set
## @var{lvl} to [lvl0, lvl0].  If both @var{n} or @var{lvl} are omitted
## a default value of 10 contour level is assumed.
##
## If provided, the filled contours are added to the axes object
## @var{ax} instead of the current axis.
##
## The following example plots filled contours of the @code{peaks}
## function.
##
## @example
## @group
## [x, y, z] = peaks (50);
## contourf (x, y, z, -7:9)
## @end group
## @end example
## @seealso{contour, contourc, patch}
## @end deftypefn

## Author: Kai Habel <kai.habel@gmx.de>
## Author: Shai Ayal <shaiay@users.sourceforge.net>

function [c, h] = contourf (varargin)

  [xh, varargin] = __plt_get_axis_arg__ ("contour", varargin{:});

  oldh = gca ();
  unwind_protect
    axes (xh);
    newplot ();
    [ctmp, htmp] = __contour__ (xh, "none", "fill", "on",
                                "linecolor", "black", varargin{:});
  unwind_protect_cleanup
    axes (oldh);
  end_unwind_protect

  if (nargout > 0)
    c = ctmp;
    h = htmp;
  endif
endfunction

%!demo
%! clf
%! [x, y, z] = peaks (50);
%! contourf (x, y, z, -7:9)

%!demo
%! clf
%! [theta, r] = meshgrid (linspace (0, 2*pi, 64), linspace(0,1,64));
%! [X, Y] = pol2cart (theta, r);
%! Z = sin(2*theta).*(1-r);
%! contourf(X, Y, abs(Z), 10)
