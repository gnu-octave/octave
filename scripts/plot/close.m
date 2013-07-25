## Copyright (C) 2002-2012 John W. Eaton
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
## @deftypefn  {Command} {} close
## @deftypefnx {Command} {} close (@var{h})
## @deftypefnx {Command} {} close all
## @deftypefnx {Command} {} close all hidden
## Close figure window(s).
##
## @code{close} operates by calling the function specified by the
## @code{"closerequestfcn"} property for each figure.  By default, the function
## @code{closereq} is used.
##
## When called with no arguments, close the current figure.  This is equivalent
## to @code{close (gcf)}.  If the input is a graphic handle @var{h} or vector
## of graphics handles then close each figure in @var{h}.
##
## If the argument "all" is given then all figures with visible handles
## (HandleVisibility = "on") are closed.
##
## If the argument "all hidden" is given then all figures, including hidden
## ones, are closed.
##
## Implementation Note: @code{close} calls a function to dispose of the figure.
## It is possible that the function will delay or abort removing the figure.
## To remove a figure without calling any callback functions use @code{delete}.
##
## @seealso{closereq, delete}
## @end deftypefn

## Author: jwe
## 2010-05-02   PBig    allow empty argument

function retval = close (arg1, arg2)

  figs = [];

  if (nargin > 2)
    print_usage ();
  elseif (nargin == 0)
    ## Close current figure.
    ## Can't use gcf because it opens a new plot window if one does not exist.
    figs = get (0, "currentfigure");
    if (figs == 0)  # don't close root figure
      figs = [];
    endif
  elseif (nargin == 1)
    if (ischar (arg1) && strcmpi (arg1, "all"))
      figs = (get (0, "children"))';
      figs = figs(isfigure (figs));
    elseif (isfigure (arg1))
      figs = arg1;
    elseif (isempty (arg1))
      figs = [];
    else
      error ('close: expecting argument to be "all" or a figure handle');
    endif
  elseif (   ischar (arg1) && strcmpi (arg1, "all")
          && ischar (arg2) && strcmpi (arg2, "hidden"))
    figs = (allchild (0))';
    figs = figs(isfigure (figs));
  endif

  for h = figs
    __go_execute_callback__ (h, "closerequestfcn");
  endfor

  if (nargout > 0)
    retval = 1;
  endif

endfunction


%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   close (hf);
%!   objs = findobj ("type", "figure");
%!   assert (isempty (intersect (objs, hf)));
%! unwind_protect_cleanup
%!   if (isfigure (hf))
%!     close (hf);
%!   endif
%! end_unwind_protect

