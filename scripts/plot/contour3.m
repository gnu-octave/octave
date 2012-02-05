## Copyright (C) 2007-2012 David BAteman
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
## @deftypefn  {Function File} {} contour3 (@var{z})
## @deftypefnx {Function File} {} contour3 (@var{z}, @var{vn})
## @deftypefnx {Function File} {} contour3 (@var{x}, @var{y}, @var{z})
## @deftypefnx {Function File} {} contour3 (@var{x}, @var{y}, @var{z}, @var{vn})
## @deftypefnx {Function File} {} contour3 (@dots{}, @var{style})
## @deftypefnx {Function File} {} contour3 (@var{h}, @dots{})
## @deftypefnx {Function File} {[@var{c}, @var{h}] =} contour3 (@dots{})
## Plot level curves (contour lines) of the matrix @var{z}, using the
## contour matrix @var{c} computed by @code{contourc} from the same
## arguments; see the latter for their interpretation.  The contours are
## plotted at the Z level corresponding to their contour.  The set of
## contour levels, @var{c}, is only returned if requested.  For example:
##
## @example
## @group
## contour3 (peaks (19));
## hold on
## surface (peaks (19), "facecolor", "none", "EdgeColor", "black");
## colormap hot;
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

function [c, h] = contour3 (varargin)

  [xh, varargin, nargin] = __plt_get_axis_arg__ ("contour3", varargin{:});

  oldh = gca ();
  unwind_protect
    axes (xh);
    newplot ();
    [ctmp, htmp] = __contour__ (xh, "auto", varargin{:});
  unwind_protect_cleanup
    axes (oldh);
  end_unwind_protect

  if (! ishold ())
    set (xh, "view", [-37.5, 30],
         "xgrid", "on", "ygrid", "on", "zgrid", "on");
  endif

  if (nargout > 0)
    c = ctmp;
    h = htmp;
  endif

endfunction

%!demo
%! clf
%! contour3 (peaks (19));
%! hold on
%! surface (peaks (19), "facecolor", "none", "edgecolor", "black")
%! colormap hot
%! axis tight
%! zlim auto
%! hold off
%! box off
