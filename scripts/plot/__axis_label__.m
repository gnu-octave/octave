## Copyright (C) 1996, 1997 John W. Eaton
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2, or (at your option)
## any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, write to the Free
## Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301, USA.

## Undocumented internal function.

## Author: jwe

function retval = __axis_label__ (caller, txt, varargin)

  if (ischar (txt))
    ## FIXME -- should be able to use text instead of __go_text__.
    ca = gca ();

    if (strcmp (caller, "ylabel"))
      rot = 90;
    else
      rot = 0;
    endif

    h = __go_text__ (ca, "string", txt, "rotation", rot, varargin{:});
    set (ca, caller, h);
    if (nargout > 0)
      retval = h;
    endif
  else
    error ("%s: expecting first argument to be character string");
  endif

endfunction
