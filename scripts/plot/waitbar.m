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
## @deftypefn  {Function File} {} waitbar (@var{frac})
## @deftypefn  {Function File} {} waitbar (@var{frac}, @var{msg})
## @deftypefnx {Function File} {} waitbar (@var{frac}, @var{h}, @dots{})
## @deftypefnx {Function File} {} waitbar (@dots{}, "FigureProperty", "Value", @dots{})
## @deftypefnx {Function File} {@var{h} = } waitbar (@dots{})
## Create a waitbar filled to fraction @var{frac} and display an optional
## message @var{msg}.  The waitbar fraction must be in the range [0, 1].  If
## the optional input @var{h} is specified then update the waitbar in the
## specified figure handle.  Otherwise, a new waitbar is created.
##
## The display of the waitbar window can be configured by passing 
## property/value pairs to the function.
## @end deftypefn

## Author: jwe

function retval = waitbar (varargin)

  persistent curr_waitbar;

  if (nargin < 1)
    print_usage ();
  endif

  frac = varargin{1};
  varargin(1) = [];

  if (! (isnumeric (frac) && isscalar (frac) && frac >= 0 && frac <= 1))
    error ("waitbar: FRAC must be between 0 and 1");
  endif

  msg = false;

  if (isempty (varargin) && ! isempty (curr_waitbar))
    h = curr_waitbar;
  else
    h = false;
  endif

  if (! isempty (varargin) && ishandle (varargin{1}))
    h = varargin{1};
    varargin(1) = [];
    ## FIXME -- also check that H is really a waitbar?
    if (! isfigure (h))
      error ("waitbar: H must be a handle to a waitbar object");
    endif
  endif

  if (! isempty (varargin))
    msg = varargin{1};
    varargin(1) = [];
    if (! ischar (msg))
      error ("waitbar: MSG must be a character string");
    endif
  endif

  if (rem (numel (varargin), 2) != 0)
    error ("waitbar: invalid number of property-value pairs");
  endif

  if (h)
    p = findobj (h, "type", "patch");
    set (p, "xdata", [0; frac; frac; 0]);
    ax = findobj (h, "type", "axes");
    if (ischar (msg))
      th = get (ax, "title");
      curr_msg = get (th, "string");
      if (! strcmp (msg, curr_msg))
        set (th, "string", msg);
      endif
    endif
  else
    h = __go_figure__ (Inf, "position", [250, 500, 400, 100],
                       "numbertitle", "off",
                       "toolbar", "none", "menubar", "none",
                       "handlevisibility", "callback",
                       varargin{:});

    ax = axes ("parent", h, "xtick", [], "ytick", [],
               "xlim", [0, 1], "ylim", [0, 1],
               "xlimmode", "manual", "ylimmode", "manual",
               "position", [0.1, 0.3, 0.8, 0.2]);
    patch (ax, [0; frac; frac; 0], [0; 0; 1; 1], [0, 0.35, 0.75]);

    if (ischar (msg))
      title (ax, msg);
    endif
  endif

  drawnow ();

  if (nargout > 0)
    retval = h;
  endif

  ## If there were no errors, update current waitbar.
  curr_waitbar = h;

endfunction

%!demo
%! h = waitbar (0, "0.00%");
%! for i = 0:0.01:1
%!   waitbar (i, h, sprintf ("%.2f%%", 100*i));
%! endfor
%! close (h);

%!demo
%! h = waitbar (0, "please wait...");
%! for i = 0:0.01:1
%!   waitbar (i, h);
%! endfor
%! close (h);

%!demo
%! h = waitbar (0, "please don't be impatient...");
%! for i = 0:0.01:1
%!   waitbar (i);
%! endfor
%! close (h);

%% Test input validation
%!error <FRAC must be between 0 and 1> waitbar (-0.5)
%!error <FRAC must be between 0 and 1> waitbar (1.5)
%!error <MSG must be a character string> waitbar (0.5, 1)
%!error <invalid number of property-value pairs> waitbar (0.5, "msg", "Name")

