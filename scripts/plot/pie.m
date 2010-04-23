## Copyright (C) 2007, 2008, 2009 David Bateman
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
## @deftypefn {Function File} {} pie (@var{y})
## @deftypefnx {Function File} {} pie (@var{y}, @var{explode})
## @deftypefnx {Function File} {} pie (@dots{}, @var{labels})
## @deftypefnx {Function File} {} pie (@var{h}, @dots{});
## @deftypefnx {Function File} {@var{h} =} pie (@dots{});
## Produce a pie chart. 
##
## Called with a single vector argument, produces a pie chart of the
## elements in @var{x}, with the size of the slice determined by percentage
## size of the values of @var{x}.
##
## The variable @var{explode} is a vector of the same length as @var{x} that
## if non zero 'explodes' the slice from the pie chart.
##
## If given @var{labels} is a cell array of strings of the same length as
## @var{x}, giving the labels of each of the slices of the pie chart. 
##
## The optional return value @var{h} provides a handle to the patch object.
##
## @seealso{bar, stem}
## @end deftypefn

## Very roughly based on pie.m from octave-forge whose author was
## Daniel Heiserer <Daniel.heiserer@physik.tu-muenchen.de>

function retval = pie (varargin)

  [h, varargin] = __plt_get_axis_arg__ ("pie", varargin{:});

  if (nargin < 1)
    print_usage ();
  else
    oldh = gca ();
    unwind_protect
      axes (h);
      newplot ();
      tmp = __pie__ (h, varargin{:});
    unwind_protect_cleanup
      axes (oldh);
    end_unwind_protect
  endif

  if (nargout > 0)
    retval = tmp;
  endif

endfunction

function hlist = __pie__ (varargin)

  h = varargin{1};
  x = abs (varargin{2});
  iarg = 3;

  if (! isvector (x))
    error ("pie: expecting vector argument");
  endif

  len = length (x);

  have_explode = false;
  have_labels = false;

  while (iarg <= nargin)
    arg = varargin{iarg++};
    if (iscell (arg))
      labels = arg;
      have_labels = true;
      if (numel (x) != numel (labels))
        error ("pie: mismatch in number of labels and data");
      endif
    elseif (isnumeric (arg))
      explode = arg;
      have_explode = true;
      if (! size_equal (x, explode))
        error ("pie: mismatch in number of elements in explode and data");
      endif
    endif
  endwhile

  if (! have_explode)
    explode = zeros (size (x));
  endif

  if (! have_labels)
    xp = round (100 * x ./ sum (x)); 
    for i = 1:len
      labels{i} = sprintf ("%d%%", xp(i));
    endfor
  endif

  hlist = [];
  refinement = 90;
  phi = 0:refinement:360;
  xphi = cumsum (x / sum (x) * 360);
  for i = 1:len 
    if (i == 1)
      xn = 0 : 360 / refinement : xphi(i);
    else
      xn = xphi(i-1) : 360 / refinement : xphi(i);
    endif

    if (xn(end) != xphi(i))
      xn = [xn, xphi(i)];
    endif

    xn2 = (xn(1) + xn(end)) / 2;
    if (explode (i))
      xoff = - 0.1 * sind (xn2);
      yoff = 0.1 * cosd (xn2);
    else
      xoff = 0;
      yoff = 0;
    endif
    xt = - 1.2 * sind (xn2);
    yt = 1.2 * cosd (xn2);
    if (xt > 0)
      align = "left";
    else
      align = "right";
    endif

    hlist = [hlist; patch(xoff + [0, - sind(xn)], yoff + [0, cosd(xn)], i);
             text(xt, yt, labels{i}, "horizontalalignment", align)];
  endfor

  if (len == 1)
    set (h, "clim", [1, 2]);
  else
    set (h, "clim", [1, len]);
  endif

  axis ([-1.5, 1.5, -1.5, 1.5], "square");

endfunction

%!demo
%! pie ([3, 2, 1], [0, 0, 1]);
%! colormap([1,0,0;0,1,0;0,0,1;1,1,0;1,0,1;0,1,1]);

%!demo
%! pie ([3, 2, 1], [0, 0, 1], {"Cheddar", "Swiss", "Camembert"});
%! colormap([1,0,0;0,1,0;0,0,1;1,1,0;1,0,1;0,1,1]);
%! axis ([-2,2,-2,2]);
