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
## @deftypefn {Function File} {} pbaspect (@var{plot_box_aspect_ratio})
## Set the plot box aspect ratio of the current axes.  The aspect ratio
## is a normalized 3-element vector representing the rendered lengths of
## the x, y, and z-axes.
##
## @deftypefnx {Function File} {@var{plot_box_aspect_ratio} =} pbaspect ( )
## Return the plot box aspect ratio of the current axes.
##
## @deftypefnx {Function File} {} pbaspect (@var{mode})
## Set the plot box aspect ratio mode of the current axes.
##
## @deftypefnx {Function File} {@var{plot_box_aspect_ratio_mode} =} pbaspect ("mode")
## Return the plot box aspect ratio mode of the current axes.
##
## @deftypefnx {Function File} {} pbaspect (@var{hax}, @dots{})
## Use the axes, with handle @var{hax}, instead of the current axes.
##
## @seealso{axis, daspect, xlim, ylim, zlim}
## @end deftypefn

## Author: Ben Abbott <bpabbott@mac.com>
## Created: 2010-01-26

function varargout = pbaspect (varargin)

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
            varargout{1} = get (hax, "plotboxaspectratiomode");
            return
          else
            error ("pbaspect: only one output is allowed");
          endif
        case "manual"
          set (hax, "plotboxaspectratiomode", "manual");
        case "auto"
          set (hax, "plotboxaspectratiomode", "auto");
        endswitch
      elseif (isreal (varargin{1}) && numel (varargin{1}) == 2)
        set (hax, "plotboxaspectratio", [varargin{1}, 1]);
      elseif (isreal (varargin{1}) && numel (varargin{1}) == 3)
        set (hax, "plotboxaspectratio", varargin{1});
      else
        error ("pbaspect: invalid input");
      endif
    elseif (numel (varargin) > 1)
      error ("pbaspect: too many inputs");
    endif
  elseif (nargout == 0)
    print_usage ();
  endif

  if (nargout == 1)
    varargout{1} = get (hax, "plotboxaspectratio");
  elseif (nargout > 1)
    error ("pbaspect: only one output is allowed");
  endif

endfunction

%!demo
%! x = 0:0.01:4;
%! clf
%! plot (x, cos (x), x, sin (x))
%! pbaspect ([1 1 1])
%! title ("plot box should be square")

%!demo
%! x = 0:0.01:4;
%! clf
%! plot (x, cos (x), x, sin (x))
%! pbaspect ([2 1 1])
%! title ("plot box aspect ratio should be 2x1")

%!demo
%! x = 0:0.01:4;
%! clf
%! plot (x, cos (x), x, sin (x))
%! daspect ([1 1 1])
%! pbaspect ([2 1 1])
%! title ("plot box should be 2x1, and axes [0 4 -1 1]")

