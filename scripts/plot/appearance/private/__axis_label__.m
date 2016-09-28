## Copyright (C) 1996-2016 John W. Eaton
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
## @deftypefn {} {} __axis_label__ (@var{caller}, @var{h}, @var{txt}, @dots{})
## Undocumented internal function.
## @end deftypefn

## Author: jwe

function retval = __axis_label__ (hax, caller, txt, varargin)

  h = get (hax, caller);

  set (h, "fontangle", get (hax, "fontangle"),
          "fontname", get (hax, "fontname"),
          "fontunits", get (hax, "fontunits"),   # must precede fontsize
          "fontsize", get (hax, "LabelFontSizeMultiplier") *
                      get (hax, "fontsize"),
          "fontweight", get (hax, "fontweight"),
          "string", txt,
          varargin{:});

  ## FIXME: It would be better to delete only the listener that [xyz]label
  ##        installed.  But this didn't work, so instead it deletes all
  ##        listener's on the [xyz]color property.
  dellistener (hax, [caller(1) "color"]);
  addlistener (hax, [caller(1) "color"], {@cb_color, h, caller(1)});

  if (nargout > 0)
    retval = h;
  endif

endfunction

## Callback to update label color when axes color is changed
function cb_color (hax, ~, hlabel, axis2label)
  set (hlabel, "color", get (hax, [axis2label "color"]));
endfunction

