## Copyright (C) 2007 David Bateman
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
## @deftypefn {Function File} {} colorbar (@var{s})
## @deftypefnx {Function File} {} colorbar ('peer', @var{h}, @dots{})
## Adds a colorbar to the current axes. Valid values for @var{s} are
##
## @table @asis
## @item 'EastOutside'
## Place the colorbar outside the plot to the right. This is the default.
## @item 'East'
## Place the colorbar inside the plot to the right.
## @item 'WestOutside'
## Place the colorbar outside the plot to the left.
## @item 'West'
## Place the colorbar inside the plot to the left.
## @item 'NorthOutside'
## Place the colorbar above the plot.
## @item 'North'
## Place the colorbar at the top of the plot.
## @item 'SouthOutside'
## Place the colorbar under the plot.
## @item 'South'
## Place the colorbar at the bottom of the plot.
## @item 'Off', 'None'
## Remove any existing colorbar from the plot.
## @end table
##
## If the argument 'peer' is given, then the following argument is treated a
## the axes handle on which to add the colorbar.
## @end deftypefn


## PKG_ADD: mark_as_command colorbar

function colorbar (varargin)
  
  if (nargin > 0 && strcmpi(varargin{1}, "peer"))
    if (nargin > 1)
      ax = varargin{2};
      if (!isscalar (ax) || ! ishandle (ax)
	  || strcmp (get (ax, "type"), "axes"))
	error ("colorbar: expecting an axes handle following 'peer'");
      endif
    else
      error ("colorbar: misisng axes handle after 'peer'");
    endif
  else
    ax = gca ();
  endif

  pos = "eastoutside";
  for i = 1 : length (varargin)
    arg = varargin {i};
    if (length(arg) < 1)
      pos = "eastoutside";
    elseif (ischar (arg))
      arg = tolower (arg);
      if (strcmp (arg, "off") || strcmp (arg, "none"))
	pos = "none";
      elseif (strcmp (arg, "north") || strcmp (arg, "south")
	      || strcmp (arg, "east") || strcmp (arg, "west")
	      || strcmp (arg, "northoutside") || strcmp (arg, "southoutside")
	      || strcmp (arg, "eastoutside") || strcmp (arg, "westoutside"))
	pos = arg;
      else
	error ("colorbar: unrecognized position argument");
      endif
    else
      error ("colorbar: expecting string arguments");
    endif
  endfor

  set (ax, "__colorbar__", pos);

endfunction


%!demo
%! hold off;
%! close all;
%! n = 64; x = kron (1:n,ones(n,1)); x = abs(x - x.'); 
%! imagesc(x)
%! colorbar();

%!demo
%! hold off;
%! n = 64; x = kron (1:n,ones(n,1)); x = abs(x - x.'); 
%! imagesc(x)
%! colorbar("westoutside");

%!demo
%! hold off;
%! n = 64; x = kron (1:n,ones(n,1)); x = abs(x - x.'); 
%! imagesc(x)
%! colorbar("northoutside");

%!demo
%! hold off;
%! n = 64; x = kron (1:n,ones(n,1)); x = abs(x - x.'); 
%! imagesc(x)
%! colorbar("southoutside");

%!demo
%! hold off;
%! subplot(2,2,1)
%! contour(peaks())
%! colorbar("east");
%! subplot(2,2,2)
%! contour(peaks())
%! colorbar("west");
%! subplot(2,2,3)
%! contour(peaks())
%! colorbar("north");
%! subplot(2,2,4)
%! contour(peaks())
%! colorbar("south");

%!demo
%! hold off;
%! n = 64; x = kron (1:n,ones(n,1)); x = abs(x - x.'); 
%! subplot(2,2,1)
%! imagesc(x)
%! colorbar();
%! subplot(2,2,2)
%! imagesc(x)
%! colorbar("westoutside");
%! subplot(2,2,3)
%! imagesc(x)
%! colorbar("northoutside");
%! subplot(2,2,4)
%! imagesc(x)
%! colorbar("southoutside");

