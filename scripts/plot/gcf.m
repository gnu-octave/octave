## Copyright (C) 2005-2012 John W. Eaton
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
## @deftypefn {Function File} {} gcf ()
## Return the current figure handle.  If a figure does not exist, create
## one and return its handle.  The handle may then be used to examine or
## set properties of the figure.  For example,
##
## @example
## @group
## fplot (@@sin, [-10, 10]);
## fig = gcf ();
## set (fig, "visible", "off");
## @end group
## @end example
##
## @noindent
## plots a sine wave, finds the handle of the current figure, and then
## makes that figure invisible.  Setting the visible property of the
## figure to @code{"on"} will cause it to be displayed again.
## @seealso{get, set}
## @end deftypefn

## Author: jwe, Bill Denney

function h = gcf ()

  if (nargin == 0)
    h = get (0, "currentfigure");
    if (isempty (h) || h == 0)
      ## We only have a root figure object, so create a new figure
      ## object and make it the current figure.
      h = figure (1);
    endif
  else
    print_usage ();
  endif

endfunction

%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   assert (gcf, hf);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect
