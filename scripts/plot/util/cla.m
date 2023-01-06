########################################################################
##
## Copyright (C) 2008-2023 The Octave Project Developers
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
## @deftypefn  {} {} cla
## @deftypefnx {} {} cla reset
## @deftypefnx {} {} cla (@var{hax})
## @deftypefnx {} {} cla (@var{hax}, "reset")
## Clear the current or specified (@var{hax}) axes object.
##
## @code{cla} operates by deleting child graphic objects with visible
## handles (@code{HandleVisibility} = @qcode{"on"}).  This typically clears the
## axes of any visual objects, but leaves in place axes limits, tick marks and
## labels, camera view, etc.  In addition, the automatic coloring and styling
## of lines is reset by changing the axes properties @code{ColorOrderIndex},
## @code{LinestyleOrderIndex} to 1.
##
## If the optional argument @qcode{"reset"} is specified, delete all child
## objects, including those with hidden handles, and reset all axes properties
## to their defaults.  However, the following properties are not reset:
## @code{Position}, @code{Units}.
##
## If the first argument @var{hax} is an axes handle, then operate on
## this axes rather than the current axes returned by @code{gca}.
## @seealso{clf, delete, reset}
## @end deftypefn

function cla (hax, do_reset = false)

  if (nargin == 0)
    hax = gca ();
  elseif (nargin == 1)
    if (isscalar (hax) && isaxes (hax))
      ## Normal case : cla (hax) without reset
    elseif (ischar (hax) && strcmpi (hax, "reset"))
      hax = gca ();
      do_reset = true;
    else
      print_usage ();
    endif
  else
    if (isscalar (hax) && isaxes (hax)
        && ischar (do_reset) && strcmpi (do_reset, "reset"))
      do_reset = true;
    else
      print_usage ();
    endif
  endif

  if (! do_reset)
    delete (get (hax, "children"));
    set (hax, "colororderindex", 1, "linestyleorderindex", 1);
  else
    delete (allchild (hax));
    reset (hax);
  endif

endfunction


%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   plot (1:10);
%!   assert (! isempty (get (gca, "children")));
%!   cla ();
%!   assert (isempty (get (gca, "children")));
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   hax = gca ();
%!   plot (hax, 1:10);
%!   assert (get (hax, "colororderindex"), 2);
%!   set (hax, "ticklabelinterpreter", "none");
%!   cla (hax);
%!   kids = get (hax, "children");
%!   assert (numel (kids), 0);
%!   assert (get (hax, "colororderindex"), 1);
%!   assert (get (hax, "ticklabelinterpreter"), "none");
%!
%!   hp = plot (hax, 1:10, "handlevisibility", "off");
%!   cla (hax);
%!   assert (ishghandle (hp), true);
%!
%!   cla (hax, "reset");
%!   kids = get (hax, "children");
%!   assert (numel (kids), 0);
%!   assert (ishghandle (hp), false);
%!   assert (get (hax, "ticklabelinterpreter"), "tex");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect
