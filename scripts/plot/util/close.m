########################################################################
##
## Copyright (C) 2002-2023 The Octave Project Developers
##
## See the file COPYRIGHT.md in the top-level directory of this
## distribution or <https://octave.org/copyright/>.
##
## This file is part of Octave.
##
## Octave is free software: you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <https://www.gnu.org/licenses/>.
##
########################################################################

## -*- texinfo -*-
## @deftypefn  {} {} close
## @deftypefnx {} {} close (@var{h})
## @deftypefnx {} {} close @var{figname}
## @deftypefnx {} {} close all
## @deftypefnx {} {} close all hidden
## @deftypefnx {} {} close all force
## @deftypefnx {} {@var{status} =} close (@dots{})
## Close figure window(s).
##
## When called with no arguments, close the current figure.  This is equivalent
## to @code{close (gcf)}.  If the input @var{h} is a graphic handle, or vector
## of graphics handles, then close each figure in @var{h}.  The figure to
## close may also be specified by name @var{figname} which is matched against
## the @qcode{"Name"} property of all figures.
##
## If the argument @qcode{"all"} is given then all figures with visible handles
## (HandleVisibility = @qcode{"on"}) are closed.
##
## If the additional argument @qcode{"hidden"} is given then all figures,
## including hidden ones, are closed.
##
## If the additional argument @qcode{"force"} is given then figures are closed
## even when @qcode{"closerequestfcn"} has been altered to prevent closing the
## window.
##
## If the optional output @var{status} is requested then Octave returns 1 if
## the figure windows were closed successfully.
##
## Implementation Note: @code{close} operates by making the handle @var{h} the
## current figure, and then calling the function specified by the
## @qcode{"closerequestfcn"} property of the figure.  By default, the function
## @code{closereq} is used.  It is possible that the function invoked will
## delay or abort removing the figure.  To remove a figure without executing
## any callback functions use @code{delete}.  When writing a callback function
## to close a window do not use @code{close} to avoid recursion.
##
## @seealso{closereq, delete}
## @end deftypefn

function status = close (arg1, arg2)

  figs = [];

  if (nargin == 0)
    ## Close current figure.
    ## Can't use gcf because it opens a new plot window if one does not exist.
    figs = get (0, "currentfigure");
    if (figs == 0)  # don't call close on root object
      figs = [];
    endif
  elseif (nargin == 1)
    if (ischar (arg1))
      if (strcmpi (arg1, "all"))
        figs = get (0, "children");
        figs = figs(isfigure (figs));
      else
        figs = findall ("-depth", 1, "name", arg1, "type", "figure");
      endif
    elseif (any (isfigure (arg1)))
      figs = arg1(isfigure (arg1));
    elseif (isempty (arg1))
      figs = [];  # Silently accept null argument for Matlab compatibility
    else
      error ('close: first argument must be "all", a figure handle, or a figure name');
    endif
  elseif (ischar (arg2)
          && (strcmpi (arg2, "hidden") || strcmpi (arg2, "force")))
    if (ischar (arg1))
      if (strcmpi (arg1, "all"))
        figs = allchild (0);
        figs = figs(isfigure (figs));
      else
        figs = findall ("-depth", 1, "name", arg1, "type", "figure");
      endif
    elseif (any (isfigure (arg1)))
      figs = arg1(isfigure (arg1));
    elseif (isempty (arg1))
      figs = [];  # Silently accept null argument for Matlab compatibility
    else
      error ('close: first argument must be "all", a figure handle, or a figure name');
    endif
    if (strcmpi (arg2, "force"))
      delete (figs);
      return;
    endif
  else
    error ('close: second argument must be "hidden" or "force"');
  endif

  ## Save and restore current figure
  cf = get (0, "currentfigure");

  for hfig = figs(:).'
    set (0, "currentfigure", hfig);  # make figure current
    __go_execute_callback__ (hfig, "closerequestfcn");
  endfor

  if (isfigure (cf))
    set (0, "currentfigure", cf);
  endif

  if (nargout > 0)
    status = 1;
  endif

endfunction


%!test
%! ## Test closing gcf
%! hf = figure ("visible", "off");
%! unwind_protect
%!   close ();
%!   assert (! isfigure (hf));
%! unwind_protect_cleanup
%!   if (isfigure (hf))
%!     delete (hf);
%!   endif
%! end_unwind_protect

%!test
%! ## Test closing specified numeric figure handle
%! hf = figure ("visible", "off");
%! unwind_protect
%!   close (hf);
%!   assert (! isfigure (hf));
%! unwind_protect_cleanup
%!   if (isfigure (hf))
%!     delete (hf);
%!   endif
%! end_unwind_protect

%!test
%! ## Test closing specified named figure handle
%! hf = figure ("visible", "off", "name", "__foobar__");
%! unwind_protect
%!   close __foobar__;
%!   assert (! isfigure (hf));
%! unwind_protect_cleanup
%!   if (isfigure (hf))
%!     delete (hf);
%!   endif
%! end_unwind_protect

%!test
%! ## Test forcing the close of a figure
%! hf = figure ("visible", "off", "closerequestfcn", []);
%! unwind_protect
%!   close (hf);
%!   assert (isfigure (hf));    # figure not deleted
%!   close (hf, "force");
%!   assert (! isfigure (hf));  # figure finally deleted
%! unwind_protect_cleanup
%!   if (isfigure (hf))
%!     delete (hf);
%!   endif
%! end_unwind_protect

## Test input validation
%!error <first argument must be "all", a figure handle> close ({"all"})
%!error <first argument must be "all", a figure handle> close (-1)
%!error <second argument must be "hidden"> close all hid
%!error <second argument must be "hidden"> close all for
