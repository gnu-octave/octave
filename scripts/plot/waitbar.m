## Copyright (C) 2011 John W. Eaton
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
## @deftypefn  {Function File} {@var{h} =} waitbar (@var{frac}, @var{msg})
## @deftypefnx {Function File} {@var{h} =} waitbar (@var{frac}, @var{h}, @var{msg})
## Craete a waitbar and display an optional message.
## @end deftypefn

## Author: jwe

function h = waitbar (varargin)

  msg = " ";

  h = 0;

  if (nargin > 0)

    frac = varargin{1};
    varargin(1) = [];

    if (! (isnumeric (frac) && isscalar (frac) && frac >= 0 && frac <= 1))
      error ("waitbar: frac must be in between 0 and 1");
    endif

    if (numel (varargin) > 0 && ishandle (varargin{1}))
      h = varargin{1};
      varargin(1) = [];
      ## FIXME -- also check that H is really a waitbar?
      if (! isfigure (h))
        error ("handle must be a waitbar object");
      endif
    endif

    if (numel (varargin) > 0)
      msg = varargin{1};
      varargin(1) = [];
      if (! ischar (msg))
        error ("waitbar: msg must be a character string");
      endif
    endif

    if (rem (numel (varargin), 2) != 0)
      error ("waitbar: invalid number of property-value pairs");
    endif

    if (h)
      p = findobj (h, "type", "patch");
      if (p)
        delete (p);
      endif
      ax = findobj (h, "type", "axes");
    else
      h = __go_figure__ (Inf, "position", [250, 500, 400, 100],
                         "numbertitle", "off",
                         "handlevisibility", "callback",
                         varargin{:});

      ax = axes ("parent", h, "xtick", [], "ytick", [],
                 "xlim", [0, 1], "ylim", [0, 1],
                 "xlimmode", "manual", "ylimmode", "manual",
                 "position", [0.1, 0.3, 0.8, 0.2]);
    endif

    patch (ax, [0, frac, frac, 0], [0, 0, 1, 1], [0, 0.35, 0.75]);
    title (ax, msg);
    drawnow ();

  else
    print_usage ();
  endif

endfunction

%!demo
%! h = 0;
%! for i = 0:0.01:1
%!   if (h)
%!     waitbar (i, h, sprintf ("%.2f%%", 100*i));
%!   else
%!     h = waitbar (i, sprintf ("%.2f%%", 100*i));
%!   endif
%! endfor
