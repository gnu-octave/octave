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
## @deftypefn {} {@var{retval} =} __tickangle__ (@var{caller}, @var{hax}, @var{angle})
## Undocumented internal function.
## @seealso{xtickangle, ytickangle, ztickangle}
## @end deftypefn

function retval = __tickangle__ (caller, hax, angle)

  ax = tolower (caller(1));
  switch (nargin)
    case 1
      retval = get (gca (), [ax, "ticklabelrotation"]);
      return;

    case 2
      if (isaxes (hax))
        retval = get (hax, [ax, "ticklabelrotation"]);
        return;
      else
        angle = hax;
        hax = [];
      endif

    case 3
      if (! isaxes (hax))
        error ([caller, ": HAX must be a handle to an axes object"]);
      endif

  endswitch

  if (! (isscalar (angle) && isnumeric (angle) && isfinite (angle)))
    error ([caller ": ANGLE must be a finite, numeric, scalar value"]);
  elseif (nargout > 0)
    error ([caller ": function called with output query and input set value"]);
  endif

  if (isempty (hax))
    hax = gca ();
  endif

  set (hax, [ax, "ticklabelrotation"], angle);

endfunction
