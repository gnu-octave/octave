## Copyright (C) 2007-2012 David Bateman
## Copyright (C) 2010 Kai Habel
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
## @deftypefn  {Function File} {} pie3 (@var{x})
## @deftypefnx {Function File} {} pie3 (@var{x}, @var{explode})
## @deftypefnx {Function File} {} pie3 (@dots{}, @var{labels})
## @deftypefnx {Function File} {} pie3 (@var{h}, @dots{});
## @deftypefnx {Function File} {@var{h} =} pie3 (@dots{});
## Draw a 3-D pie chart.
##
## Called with a single vector argument, produces a 3-D pie chart of the
## elements in @var{x}, with the size of the slice determined by percentage
## size of the values of @var{x}.
##
## The variable @var{explode} is a vector of the same length as @var{x} that
## if non zero 'explodes' the slice from the pie chart.
##
## If given @var{labels} is a cell array of strings of the same length as
## @var{x}, giving the labels of each of the slices of the pie chart.
##
## The optional return value @var{h} is a list of graphics handles to the patch,
## surface, and text objects generating the plot.
##
## @seealso{pie, bar, stem}
## @end deftypefn

## Very roughly based on pie.m from octave-forge whose author was
## Daniel Heiserer <Daniel.heiserer@physik.tu-muenchen.de>

function retval = pie3 (varargin)

  [h, varargin] = __plt_get_axis_arg__ ("pie", varargin{:});

  if (nargin < 1)
    print_usage ();
  else
    oldh = gca ();
    unwind_protect
      axes (h);
      newplot ();
      tmp = __pie__ ("pie3", h, varargin{:});
    unwind_protect_cleanup
      axes (oldh);
    end_unwind_protect
  endif

  if (nargout > 0)
    retval = tmp;
  endif

endfunction


%!demo
%! clf
%! pie3 ([5:-1:1], [0, 0, 1, 0, 0]);
%! colormap ([1,0,0;0,1,0;0,0,1;1,1,0;1,0,1;0,1,1]);

%!demo
%! clf
%! pie3 ([3, 2, 1], [0, 0, 1], {"Cheddar", "Swiss", "Camembert"});
%! colormap ([1,0,0;0,1,0;0,0,1;1,1,0;1,0,1;0,1,1]);
%! axis ([-2,2,-2,2]);

%!demo
%! clf
%! pie3 ([0.17, 0.34, 0.41], {"Cheddar", "Swiss", "Camembert"});
%! colormap ([1,0,0;0,1,0;0,0,1;1,1,0;1,0,1;0,1,1]);
%! axis ([-2,2,-2,2]);
%! title ("missing slice");

