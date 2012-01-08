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
## @c List other forms of function in documentation index
## @findex ylim
## @findex zlim
##
## @deftypefn  {Function File} {@var{xl} =} xlim ()
## @deftypefnx {Function File} {} xlim (@var{xl})
## @deftypefnx {Function File} {@var{m} =} xlim ('mode')
## @deftypefnx {Function File} {} xlim (@var{m})
## @deftypefnx {Function File} {} xlim (@var{h}, @dots{})
## Get or set the limits of the x-axis of the current plot.  Called without
## arguments @code{xlim} returns the x-axis limits of the current plot.
## If passed a two element vector @var{xl}, the limits of the x-axis are set
## to this value.
##
## The current mode for calculation of the x-axis can be returned with a
## call @code{xlim ('mode')}, and can be either 'auto' or 'manual'.  The
## current plotting mode can be set by passing either 'auto' or 'manual'
## as the argument.
##
## If passed a handle as the first argument, then operate on this handle
## rather than the current axes handle.
## @seealso{ylim, zlim, set, get, gca}
## @end deftypefn

function retval = xlim (varargin)
  ret = __axes_limits__ ("xlim", varargin{:});

  if (! isempty (ret))
    retval = ret;
  endif
endfunction

%!demo
%! clf ();
%! line ();
%! xlim ([0.2, 0.8]);
%! title ("xlim is [0.2, 0.8]");
%! assert (xlim (), [0.2, 0.8]);

%!demo
%! clf ();
%! line ();
%! xlim ('auto');
%! title ("xlim is auto");
%! assert (xlim ("mode"), "auto");

%!demo
%! clf ();
%! plot3 ([0,1], [0,1], [0,1]);
%! xlim ([0.2, 0.8]);
%! title ("xlim is [0.2, 0.8]");
%! assert (xlim (), [0.2, 0.8]);

%!demo
%! clf ();
%! plot3 ([0,1], [0,1], [0,1]);
%! xlim ('auto');
%! title ("xlim is auto");
%! assert (xlim ("mode"), "auto");


%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   plot3 ([0,1], [0,1], [0,1]);
%!   xlim ([0, 1.1]);
%!   assert (get (gca, "xlim"), [0, 1.1], eps);
%!   assert (xlim ("mode"), "manual");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = plot3 ([0,1.1], [0,1], [0, 1]);
%!   assert (get (gca, "xlim"), [0, 1.4], eps);
%!   assert (xlim ("mode"), "auto");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect
