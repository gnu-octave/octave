########################################################################
##
## Copyright (C) 2010-2023 The Octave Project Developers
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
## @deftypefn {} {[@var{hplots}, @var{strings}] =} __getlegenddata__ (@var{hlegend})
## Undocumented internal function.
## @end deftypefn

function [hplots, text_strings] = __getlegenddata__ (hlegend)

  hplots = [];
  text_strings = {};
  ca = getappdata (hlegend, "__axes_handle__");
  if (numel (ca) == 1)
    kids = get (ca, "children");
  else
    kids = cell2mat (get (ca, "children"));
  endif

  for i = numel (kids):-1:1
    typ = get (kids(i), "type");
    if (any (strcmp (typ, {"line", "patch", "surface", "hggroup"})))
      dname = get (kids(i), "DisplayName");
      if (! isempty (dname))
        hplots(end+1) = kids(i);
        text_strings(end+1) = dname;
      endif
    endif
  endfor

endfunction


## No test needed for internal helper function.
%!assert (1)
