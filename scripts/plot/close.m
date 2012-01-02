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
## @deftypefnx {Command} {} close (@var{n})
## @deftypefnx {Command} {} close all
## @deftypefnx {Command} {} close all hidden
## Close figure window(s) by calling the function specified by the
## @code{"closerequestfcn"} property for each figure.  By default, the
## function @code{closereq} is used.
## @seealso{closereq}
## @end deftypefn

## Author: jwe
## 2010-05-02   PBig    allow empty argument

function retval = close (arg1, arg2)

  figs = [];

  if (nargin == 0)
    ## Close current figure.  Don't use gcf because that will open a new
    ## plot window if one doesn't exist.
    figs = get (0, "currentfigure");
    if (! isempty (figs) && figs == 0)
      figs = [];
    endif
  elseif (nargin == 1)
    if (ischar (arg1) && strcmpi (arg1, "all"))
      close_all_figures (false);
    elseif (isfigure (arg1))
      figs = arg1;
    elseif (isempty(arg1))
      figs = [];
    else
      error ("close: expecting argument to be \"all\" or a figure handle");
    endif
  elseif (nargin == 2
          && ischar (arg1) && strcmpi (arg1, "all")
          && ischar (arg2) && strcmpi (arg2, "hidden"))
    close_all_figures (true);
  else
    print_usage ();
  endif

  for h = figs
    __go_execute_callback__ (h, "closerequestfcn");
  endfor

  if (nargout > 0)
    retval = 1;
  endif

endfunction

function close_all_figures (close_hidden_figs)

  while (! isempty (fig = get (0, "currentfigure")))
    ## handlevisibility = get (fig, "handlevisibility")
    ## if (close_hidden_figs || ! strcmpi (handlevisibility, "off"))
    close (fig);
    ## endif
  endwhile

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
