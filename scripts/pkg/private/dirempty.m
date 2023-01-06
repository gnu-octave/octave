########################################################################
##
## Copyright (C) 2005-2023 The Octave Project Developers
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
## @deftypefn {} {@var{emp} =} dirempty (@var{nm}, @var{ign})
## Undocumented internal function.
## @end deftypefn

function emp = dirempty (nm, ign)

  if (isfolder (nm))
    if (nargin < 2)
      ign = {".", ".."};
    else
      ign = [{".", ".."}, ign];
    endif
    l = dir (nm);
    for i = 1:length (l)
      found = false;
      for j = 1:length (ign)
        if (strcmp (l(i).name, ign{j}))
          found = true;
          break;
        endif
      endfor
      if (! found)
        emp = false;
        return;
      endif
    endfor
    emp = true;
  else
    emp = true;
  endif

endfunction
