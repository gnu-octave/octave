## Copyright (C) 2017-2018 Pantxo Diribarne
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

## patch/surface "normalmode" deprecated in 4.2, remove from version 5.
%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   hp = patch ();
%!   testprop (hp, "normalmode", "5.0");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

%! hf = figure ("visible", "off");
%! unwind_protect
%!   hs = surface ();
%!   testprop (hs, "normalmode", "5.0");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## axes, "zero" value for "x/yaxislocation" deprecated in 4.2, remove
## from version 5.
%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   ha = axes ();
%!   testprop (ha, "xaxislocation", "5.0", "zero");
%!   testprop (ha, "yaxislocation", "5.0", "zero");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## annotation rectangle "edgecolor" deprecated in 4.4, remove from version 6.
%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   ha = annotation ("rectangle");
%!   testprop (ha, "edgecolor", "6.0");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## figure "doublebuffer, mincolormap, wvisual, wvisualmode, xdisplay,
## xvisual, xvisualmode" deprecated in 4.4, remove from version 6.
%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   testprop (hf, "doublebuffer", "6.0");
%!   testprop (hf, "mincolormap", "6.0");
%!   testprop (hf, "wvisual", "6.0");
%!   testprop (hf, "wvisualmode", "6.0");
%!   testprop (hf, "xdisplay", "6.0");
%!   testprop (hf, "xvisual", "6.0");
%!   testprop (hf, "xvisualmode", "6.0");
%! unwind_protect_cleanup
%!   close (hf)
%! end_unwind_protect

## axes "drawmode" deprecated in 4.4, remove from version 6.
%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   hax = axes ();
%!   testprop (hax, "drawmode", "6.0");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## text/uicontrol/uipanel/uibuttongroup  "demi" and "light" values for
## "fontweight" property are deprecated in 4.4, remove from version 6:
##   * remove "demi" and "light" options in graphics.in.h,
##   QtHandlesUtils.cc and ft-text-renderer.cc
##   * remove warnings from update_fontweight in graphics.in.h
%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   ht = text ();
%!   testprop (ht, "fontweight", "6.0", "demi");
%!   testprop (ht, "fontweight", "6.0", "light");
%!   hui = uicontrol ();
%!   testprop (hui, "fontweight", "6.0", "demi");
%!   testprop (hui, "fontweight", "6.0", "light");
%!   hui = uipanel ();
%!   testprop (hui, "fontweight", "6.0", "demi");
%!   testprop (hui, "fontweight", "6.0", "light");
%!   hui = uibuttongroup ();
%!   testprop (hui, "fontweight", "6.0", "demi");
%!   testprop (hui, "fontweight", "6.0", "light");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect
