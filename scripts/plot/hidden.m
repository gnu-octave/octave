## Copyright (C) 2007-2012 Michael Goffioul
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
## @deftypefn  {Function File} {} hidden (@var{mode})
## @deftypefnx {Function File} {} hidden ()
## Manipulation the mesh hidden line removal.  Called with no argument
## the hidden line removal is toggled.  The argument @var{mode} can be either
## 'on' or 'off' and the set of the hidden line removal is set accordingly.
## @seealso{mesh, meshc, surf}
## @end deftypefn

function retval = hidden (mode)

  if (nargin == 0)
    mode = "swap";
  elseif (nargin == 1);
    if (ischar (mode))
      mode = tolower (mode);
      if (! strcmp (mode, "on") && ! strcmp (mode, "off"))
        error ("hidden: MODE expected to be 'on' or 'off'");
      endif
    else
      error ("hidden: expecting MODE to be a string");
    endif
  else
    print_usage ();
  endif

  for h = get (gca (), "children");
    htype = lower (get (h, "type"));
    if (strcmp (htype, "surface"))
      fc = get (h, "facecolor");
      if ((! ischar (fc) && is_white (fc))
          || (ischar (fc) && strcmpi (fc, "none")))
        switch (mode)
        case "on"
          set (h, "facecolor", "w");
        case "off"
          set (h, "facecolor", "none");
        case "swap"
          if (ischar (fc))
            set (h, "facecolor", "w");
            mode = "on";
          else
            set (h, "facecolor", "none");
            mode = "off";
          endif
        endswitch
      endif
    endif
  endfor

  if (nargout > 0)
    retval = mode;
  endif

endfunction

function retval = is_white (color)
  retval = all (color == 1);
endfunction
