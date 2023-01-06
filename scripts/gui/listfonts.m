########################################################################
##
## Copyright (C) 2020-2023 The Octave Project Developers
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
## @deftypefn  {} {fonts =} listfonts ()
## @deftypefnx {} {fonts =} listfonts (@var{h})
## List system fonts.
##
## If a handle to a graphics object @var{h} is provided, also include the
## font from the object's @qcode{"FontName"} property in the list.
##
## Programming Note: On systems that don't use FontConfig natively (all but
## Linux), the font cache is built when Octave is installed.  You will need to
## run @code{system ("fc-cache -fv")} manually after installing new fonts.
##
## @seealso{uisetfont, text, axes, uicontrol}
## @end deftypefn

function fonts = listfonts (h)

  if (nargin == 1 && (! ishghandle (h) || ! isprop (h, "fontname")))
    error (['listfonts: H must be a handle to a graphics object ', ...
            'with a "fontname" property']);
  endif

  persistent sysfonts = get_fonts ();
  fonts = sysfonts;

  if (nargin == 1 && ! isempty (h))
    font = get (h, "fontname");
    if (! strcmp (font, "*"))
      fonts = unique ([fonts font]);
    endif
  endif

endfunction

function fonts = get_fonts ()

  fontfiles = __get_system_fonts__ ();

  fonts = unique ({fontfiles.family, "FreeSans"});

endfunction


## Test input validation
%!error listfonts (0, 0)
%!error <H must be a handle to a graphics object with a "fontname" property>
%! s = listfonts (0);
%!error <H must be a handle to a graphics object>
%! s = listfonts (struct ());
