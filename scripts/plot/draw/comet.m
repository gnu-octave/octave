########################################################################
##
## Copyright (C) 2008-2023 The Octave Project Developers
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
## @deftypefn  {} {} comet (@var{y})
## @deftypefnx {} {} comet (@var{x}, @var{y})
## @deftypefnx {} {} comet (@var{x}, @var{y}, @var{p})
## @deftypefnx {} {} comet (@var{hax}, @dots{})
## Produce a simple comet style animation along the trajectory provided by
## the input coordinate vectors (@var{x}, @var{y}).
##
## If @var{x} is not specified it defaults to the indices of @var{y}.
##
## The speed of the comet may be controlled by @var{p}, which represents the
## time each point is displayed before moving to the next one.  The default for
## @var{p} is @code{5 / numel (@var{y})}.
##
## If the first argument @var{hax} is an axes handle, then plot into this axes,
## rather than the current axes returned by @code{gca}.
## @seealso{comet3}
## @end deftypefn

function comet (varargin)

  [hax, varargin, nargin] = __plt_get_axis_arg__ ("comet", varargin{:});

  if (nargin == 0 || nargin > 3)
    print_usage ();
  endif

  if (nargin == 1)
    y = varargin{1};
    x = 1:numel (y);
    p = 5 / numel (y);
  elseif (nargin == 2)
    x = varargin{1};
    y = varargin{2};
    p = 5 / numel (y);
  elseif (nargin == 3)
    x = varargin{1};
    y = varargin{2};
    p = varargin{3};
  endif

  oldfig = [];
  if (! isempty (hax))
    oldfig = get (0, "currentfigure");
  endif
  unwind_protect
    hax = newplot (hax);
    limits = [min(x), max(x), min(y), max(y)];
    num = numel (y);
    dn = round (num/10);

    hl = plot (x(1), y(1), "color", "r", "marker", "none",
               x(1), y(1), "color", "g", "marker", "none",
               x(1), y(1), "color", "b", "marker", "o");
    axis (limits);  # set manual limits to speed up plotting

    ## Initialize the timer
    t = p;
    timerid = tic ();

    for n = 2:(num+dn)
      m = n - dn;
      m = max ([m, 1]);
      k = min ([n, num]);
      set (hl(1), "xdata", x(1:m), "ydata", y(1:m));
      set (hl(2), "xdata", x(m:k), "ydata", y(m:k));
      set (hl(3), "xdata", x(k),   "ydata", y(k));

      pause (t - toc (timerid));
      t += p;
    endfor

  unwind_protect_cleanup
    if (! isempty (oldfig))
      set (0, "currentfigure", oldfig);
    endif
  end_unwind_protect

endfunction


%!demo
%! clf;
%! title ("comet() animation");
%! hold on;
%! t = 0:.1:2*pi;
%! x = cos (2*t) .* (cos (t).^2);
%! y = sin (2*t) .* (sin (t).^2);
%! comet (x, y, .05);
%! hold off;
