## Copyright (C) 2010-2012 David Bateman
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
## @deftypefn {Function File} {[@var{hplots}, @var{strings}]} = __getlegenddata__ (@var{h})
## Undocumented internal function.
## @end deftypefn

function [hplots, text_strings] = __getlegenddata__ (hlegend)
  hplots = [];
  text_strings = {};
  ca = getfield (get (hlegend, "userdata"), "handle");
  kids = [];
  for i = 1:numel (ca)
    kids = [kids; get(ca(i), "children")];
  endfor

  for i = numel (kids):-1:1
    typ = get (kids(i), "type");
    if (strcmp (typ, "line") || strcmp (typ, "surface")
        || strcmp (typ, "patch") || strcmp (typ, "hggroup"))

      if (strcmp (typ, "hggroup"))
        hgkids = get (kids(i), "children");
        for j = 1 : length (hgkids)
          hgobj = get (hgkids (j));
          if (isfield (hgobj, "displayname") && ! isempty (hgobj.displayname))
            hplots = [hplots, hgkids(j)];
            text_strings = {text_strings{:}, hgobj.displayname};
            break;
          endif
        endfor
      else
        if (! isempty (get (kids (i), "displayname")))
          hplots = [hplots, kids(i)];
          text_strings = {text_strings{:}, get(kids (i), "displayname")};
        endif
      endif

    endif
  endfor

endfunction
