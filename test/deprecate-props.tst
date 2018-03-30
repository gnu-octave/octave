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

%!function testprop (h, prop, vrs, val = [])
%!  vrsmax = strsplit (vrs, ".");
%!  vrscur = strsplit (version (), ".");
%!  if (num2str (vrsmax{1}) < num2str (vrscur{1}) ||
%!      num2str (vrsmax{2}) < num2str (vrscur{2}))
%!    if (isempty (val) && isprop (h, prop))
%!      error ("Please remove %s property %s", get (h, "type"), prop);
%!    elseif (! isempty (val) && any (strcmp (val, set (h, prop))))
%!      error ("Please remove '%s' from allowed values for %s property %s",
%!             val, get (h, "type"), prop);
%!    endif
%!  endif
%!endfunction

## annotation rectangle "edgecolor" deprecated in 4.4, remove from 4.7.+
%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   ha = annotation ("rectangle");
%!   testprop (ha, "edgecolor", "4.6");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## figure "doublebuffer, mincolormap, wvisual, wvisualmode, xdisplay,
## xvisual, xvisualmode" deprecated in 4.4, remove from 4.7.+
%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   testprop (hf, "doublebuffer", "4.6");
%!   testprop (hf, "mincolormap", "4.6");
%!   testprop (hf, "wvisual", "4.6");
%!   testprop (hf, "wvisualmode", "4.6");
%!   testprop (hf, "xdisplay", "4.6");
%!   testprop (hf, "xvisual", "4.6");
%!   testprop (hf, "xvisualmode", "4.6");
%! unwind_protect_cleanup
%!   close (hf)
%! end_unwind_protect

## axes "drawmode" deprecated in 4.4, remove from 4.7.+
%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   hax = axes ();
%!   testprop (hax, "drawmode", "4.6");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## text/uicontrol/uipanel/uibuttongroup  "demi" and "light" values for
## "fontweight" property are deprecated in 4.4, remove from 4.7.+:
##   * remove "demi" and "light" options in graphics.in.h,
##   QtHandlesUtils.cc and ft-text-renderer.cc
##   * remove warnings from update_fontweight in graphics.in.h
%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   ht = text ();
%!   testprop (ht, "fontweight", "4.6", "demi");
%!   testprop (ht, "fontweight", "4.6", "light");
%!   hui = uicontrol ();
%!   testprop (hui, "fontweight", "4.6", "demi");
%!   testprop (hui, "fontweight", "4.6", "light");
%!   hui = uipanel ();
%!   testprop (hui, "fontweight", "4.6", "demi");
%!   testprop (hui, "fontweight", "4.6", "light");
%!   hui = uibuttongroup ();
%!   testprop (hui, "fontweight", "4.6", "demi");
%!   testprop (hui, "fontweight", "4.6", "light");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect
