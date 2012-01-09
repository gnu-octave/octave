## Copyright (C) 2010-2012 Ben Abbott
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
## @deftypefn {Function File} {} daspect (@var{data_aspect_ratio})
## Set the data aspect ratio of the current axes.  The aspect ratio is
## a normalized 3-element vector representing the span of the x, y, and
## z-axes limits.
##
## @deftypefnx {Function File} {@var{data_aspect_ratio} =} daspect ( )
## Return the data aspect ratio of the current axes.
##
## @deftypefnx {Function File} {} daspect (@var{mode})
## Set the data aspect ratio mode of the current axes.
##
## @deftypefnx {Function File} {@var{data_aspect_ratio_mode} =} daspect ("mode")
## Return the data aspect ratio mode of the current axes.
##
## @deftypefnx {Function File} {} daspect (@var{hax}, @dots{})
## Use the axes, with handle @var{hax}, instead of the current axes.
##
## @seealso{axis, pbaspect, xlim, ylim, zlim}
## @end deftypefn

## Author: Ben Abbott <bpabbott@mac.com>
## Created: 2010-01-26

function varargout = daspect (varargin)

  hax = gca ();

  if (nargin > 0)
    if (isscalar (varargin{1}) && ishandle (varargin{1}))
      hax = varargin{1};
      varargin = varargin(2:end);
    endif
  endif
  if (numel (varargin) > 0)
    if (numel (varargin) == 1)
      if (ischar (varargin{1})
          && any (strcmpi (varargin{1}, {"mode", "manual", "auto"})))
        switch (varargin{1})
        case "mode"
          if (nargout < 2)
            varargout{1} = get (hax, "dataaspectratiomode");
            return
          else
            error ("daspect: only one output is allowed");
          endif
        case "manual"
          set (hax, "dataaspectratiomode", "manual");
        case "auto"
          set (hax, "dataaspectratiomode", "auto");
        endswitch
      elseif (isreal (varargin{1}) && numel (varargin{1}) == 2)
        set (hax, "dataaspectratio", [varargin{1}, 1]);
      elseif (isreal (varargin{1}) && numel (varargin{1}) == 3)
        set (hax, "dataaspectratio", varargin{1});
      else
        error ("daspect: invalid input");
      endif
    elseif (numel (varargin) > 1)
      error ("daspect: too many inputs");
    endif
  elseif (nargout == 0)
    print_usage ();
  endif

  if (nargout == 1)
    varargout{1} = get (hax, "dataaspectratio");
  elseif (nargout > 1)
    error ("daspect: only one output is allowed");
  endif

endfunction

%!demo
%! x = 0:0.01:4;
%! clf
%! plot (x, cos (x), x, sin (x))
%! axis square
%! daspect ([1 1 1])
%! title ("square plot-box with axis limits [0, 4, -2, 2]")

%!demo
%! x = 0:0.01:4;
%! clf
%! plot (x, cos (x), x, sin (x))
%! axis ([0 4 -1 1])
%! daspect ([2 1 1])
%! title ("square plot-box with axis limits [0, 4, -1, 1]")

%!demo
%! x = 0:0.01:4;
%! clf
%! plot (x, cos (x), x, sin (x))
%! daspect ([1 2 1])
%! pbaspect ([2 1 1])
%! title ("2x1 plot box with axis limits [0, 4, -2, 2]")

%!demo
%! x = 0:0.01:4;
%! clf
%! plot (x, cos (x), x, sin (x))
%! axis square
%! set (gca, "activepositionproperty", "position")
%! daspect ([1 1 1])
%! title ("square plot-box with axis limits [0, 4, -2, 2]")

%!demo
%! x = 0:0.01:4;
%! clf
%! plot (x, cos (x), x, sin (x))
%! axis ([0 4 -1 1])
%! set (gca, "activepositionproperty", "position")
%! daspect ([2 1 1])
%! title ("square plot-box with axis limits [0, 4, -1, 1]")

