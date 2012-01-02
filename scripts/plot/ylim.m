## Copyright (C) 2007-2012 David Bateman
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
## @deftypefn  {Function File} {@var{yl} =} ylim ()
## @deftypefnx {Function File} {} ylim (@var{yl})
## @deftypefnx {Function File} {@var{m} =} ylim ('mode')
## @deftypefnx {Function File} {} ylim (@var{m})
## @deftypefnx {Function File} {} ylim (@var{h}, @dots{})
## Get or set the limits of the y-axis of the current plot.  Called without
## arguments @code{ylim} returns the y-axis limits of the current plot.
## If passed a two element vector @var{yl}, the limits of the y-axis are set
## to this value.
##
## The current mode for calculation of the y-axis can be returned with a
## call @code{ylim ('mode')}, and can be either 'auto' or 'manual'.  The
## current plotting mode can be set by passing either 'auto' or 'manual'
## as the argument.
##
## If passed a handle as the first argument, then operate on this handle
## rather than the current axes handle.
## @seealso{xlim, zlim, set, get, gca}
## @end deftypefn

function retval = ylim (varargin)
  ret = __axes_limits__ ("ylim", varargin{:});

  if (! isempty (ret))
    retval = ret;
  endif
endfunction

%!demo
%! clf ();
%! line ();
%! ylim ([0.2, 0.8]);
%! title ("ylim is [0.2, 0.8]");
%! assert (ylim (), [0.2, 0.8]);

%!demo
%! clf ();
%! line ();
%! ylim ('auto');
%! title ("ylim is auto");
%! assert (ylim ("mode"), "auto");

%!demo
%! clf ();
%! plot3 ([0,1], [0,1], [0,1]);
%! ylim ([0.2, 0.8]);
%! title ("ylim is [0.2, 0.8]");
%! assert (ylim (), [0.2, 0.8]);

%!demo
%! clf ();
%! plot3 ([0,1], [0,1], [0,1]);
%! ylim ('auto');
%! title ("ylim is auto");
%! assert (ylim ("mode"), "auto");

%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   limy = [0, 1.1];
%!   plot3 ([0,1], [0,1], [0,1]);
%!   ylim (limy);
%!   assert (get (gca, "ylim"), limy, eps);
%!   assert (ylim ("mode"), "manual");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   plot3 ([0,1], [0,1.1], [0, 1]);
%!   assert (get (gca, "ylim"), [0, 1.4], eps);
%!   assert (ylim ("mode"), "auto");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect
