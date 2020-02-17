########################################################################
##
## Copyright (C) 2017-2020 The Octave Project Developers
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

## Put graphics properties here that should be removed, or for which some
## values (radio strings only) should not be accepted, in a given future
## version.  Don't forget to add a note in the NEWS file.

%!function testprop (h, prop, removal_version, val = [])
%!  if (compare_versions (version (), removal_version, ">="))
%!    if (isempty (val) && isprop (h, prop))
%!      error ("Please remove %s property %s", get (h, "type"), prop);
%!    elseif (! isempty (val) && any (strcmp (val, set (h, prop))))
%!      error ("Please remove '%s' from allowed values for %s property %s",
%!             val, get (h, "type"), prop);
%!    endif
%!  endif
%!endfunction

## text/uicontrol/uipanel/uibuttongroup/uitable  "oblique" value for
## "fontangle" property was deprecated in 5.0, remove from version 7:
##   * remove "oblique" options in graphics.in.h, QtHandlesUtils.cc,
##     and ft-text-renderer.cc
##   * remove warnings from update_fontangle in graphics.in.h
%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   ht = text ();
%!   testprop (ht, "fontangle", "7.0", "oblique");
%!   hui = uicontrol ();
%!   testprop (hui, "fontangle", "7.0", "oblique");
%!   hui = uipanel ();
%!   testprop (hui, "fontangle", "7.0", "oblique");
%!   hui = uibuttongroup ();
%!   testprop (hui, "fontangle", "7.0", "oblique");
%!   hui = uitable ();
%!   testprop (hui, "fontangle", "7.0", "oblique");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect
