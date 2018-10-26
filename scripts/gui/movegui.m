## Copyright (C) 2018 Guillaume Flandin
##
## This file is part of Octave.
##
## Octave is free software: you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
## your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the filename COPYING.  If not, see
## <https://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn  {Command} {} movegui
## @deftypefnx  {Command} {} movegui (@var{h})
## @deftypefnx  {Command} {} movegui (@var{pos})
## @deftypefnx  {Command} {} movegui (@var{h}, @var{pos})
## @deftypefnx  {Command} {} movegui (@var{h}, @var{event})
## @deftypefnx  {Command} {} movegui (@var{h}, @var{event}, @var{pos})
## Move a figure specified by figure handle @var{h} to a position on the screen
## defined by @var{pos}.
##
## @var{h} is a figure handle, or a handle to a graphics object.  In the latter
## case, its parent figure will be used.  If unspecified, @var{h} will be set to
## the handle of the relevant figure if a callback is being executed, otherwise
## it will be set to the handle of the current figure.
##
## @var{pos} is a numeric or char array.  If a numeric array, @var{pos} is a
## two-value array specifying the horizontal and vertical offsets of the figure
## with respect to the screen.  A positive value indicates the offset between the
## left (resp. bottom) of the screen and the left (resp. bottom) of the figure.
## A negative value indicates the offset between the right (resp. top) of the
## screen and the right (resp. top) of the figure.
##
## Possible values for @var{pos} as a char array are
##
## @table @code
## @item north
## Top center of the screen.
## @item south
## Bottom center of the screen.
## @item east
## Right center of the screen.
## @item west
## Left center of the screen.
## @item northeast
## Top right of the screen.
## @item northwest
## Top left of the screen.
## @item southeast
## Bottom right of the screen.
## @item southwest
## Bottom left of the screen.
## @item center
## Center of the screen.
## @item onscreen
## The figure will be minimally moved to be entirely visible on the screen, with
## a 30 pixel extra padding from the sides of the screen.
## This is the default value if none is provided.
## @end table
##
## @var{event} contains event data that will be ignored.  This construct
## facilitates a call to movegui from a callback.
##
## @end deftypefn

function movegui (varargin)

  if (nargin > 3)
    print_usage ();
  endif

  ## Default values for input arguments
  h = [];
  pos = "onscreen";

  ## Get input arguments
  if (nargin == 3)
    h = varargin{1};
    pos = varargin{3};
  elseif (nargin == 2)
    h = varargin{1};
    pos = varargin{2};
  elseif (nargin == 1)
    if (ishghandle (varargin{1}) && isscalar (varargin{1}))
      h = varargin{1};
    else
      pos = varargin{1};
    endif
  endif

  ## Check figure handle
  if (isempty (h))
    h = gcbf ();
    if (isempty (h))
      h = gcf ();
    endif
  endif
  if (ishghandle (h))
    h = ancestor (h, "figure");
  else
    error ("movegui: invalid figure handle");
  endif

  ## Get current position
  units = get (h, "Units");
  set (h, "Units", "pixels");
  fpos = get (h, "Position"); # OuterPosition seems unreliable
  set (h, "Units", units);

  ## Get screen size
  units_groot = get (groot (), "Units");
  screen_size = get (groot (), "ScreenSize");
  set (groot (), "Units", units_groot);

  ## Set default figure and screen border sizes [left, top, right, bottom]
  f = [0 90 0 30];
  s = [0  0 0 30];

  ## Make sure figure is not larger than screen
  fpos(1) = max (fpos(1), 1);
  fpos(2) = max (fpos(2), 1);
  fpos(3) = min (fpos(3), screen_size(3));
  fpos(4) = min (fpos(4), screen_size(4));

  ## Standard figure coordinates
  ## left, middle, right
  x = [s(1)+f(1), (screen_size(3)-fpos(3))/2, screen_size(3)-fpos(3)-s(3)-f(3)];
  ## bottom, middle top
  y = [s(4)+f(4), (screen_size(4)-fpos(4))/2, screen_size(4)-fpos(4)-s(2)-f(2)];

  ## Compute new position
  if (isnumeric (pos) && isreal (pos) && numel (pos) == 2)
    fpos(1) = ifelse (pos(1) >= 0, pos(1), pos(1) + x(3));
    fpos(2) = ifelse (pos(2) >= 0, pos(2), pos(2) + y(3));
  elseif (ischar (pos))
    switch (tolower (pos))
      case "north"
        fpos(1:2) = [x(2) y(3)];
      case "south"
        fpos(1:2) = [x(2) y(1)];
      case "east"
        fpos(1:2) = [x(3) y(2)];
      case "west"
        fpos(1:2) = [x(1) y(2)];
      case "northeast"
        fpos(1:2) = [x(3) y(3)];
      case "northwest"
        fpos(1:2) = [x(1) y(3)];
      case "southeast"
        fpos(1:2) = [x(3) y(1)];
      case "southwest"
        fpos(1:2) = [x(1) y(1)];
      case "center"
        fpos(1:2) = [x(2) y(2)];
      case "onscreen"
        if (fpos(1) > x(3))
          fpos(1) = x(3) - 30;
        endif
        if (fpos(2) > y(3))
          fpos(2) = y(3) - 30;
        endif
        fpos(1) = max(fpos(1), 30);
        fpos(2) = max(fpos(2), 30);
      otherwise
        error ("movegui: invalid position");
    endswitch
  elseif (nargin == 2 && ! isempty (gcbo))
    ## Ignore event data (from callback)
    movegui (h);
    return;
  else
    error ("movegui: invalid position");
  endif

  ## Move figure
  units = get (h, "Units");
  set (h, "Units", "pixels");
  set (h, "Position", fpos);
  set (h, "Units", units);

endfunction

%!test
%! unwind_protect
%!   h = figure ("Visible", "off");
%!   pos = {[10 10], [10 -10], [-10 10], [-10 -10], [10 10]',...
%!     "north", "east", "south", "west", ...
%!     "northwest", "northeast", "southeast", "southwest", ...
%!     "center", "onscreen"};
%!   for i = 1:numel (pos)
%!     movegui (h, pos{i});
%!     movegui (pos{i});
%!     movegui (h, struct ("evt", []), pos{i});
%!   endfor
%!   movegui ();
%!   movegui (h);
%! unwind_protect_cleanup
%!   close (h);
%! end_unwind_protect
