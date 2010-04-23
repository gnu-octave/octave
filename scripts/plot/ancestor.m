## Copyright (C) 2007, 2008, 2009 Michael Goffioul
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
## @deftypefn {Function File} {@var{parent} =} ancestor (@var{h}, @var{type})
## @deftypefnx {Function File} {@var{parent} =} ancestor (@var{h}, @var{type}, 'toplevel')
## Return the first ancestor of handle object @var{h} whose type matches
## @var{type}, where @var{type} is a character string.  If @var{type} is a
## cell array of strings, return the first parent whose type matches
## any of the given type strings.
##
## If the handle object @var{h} is of type @var{type}, return @var{h}.
##
## If @code{"toplevel"} is given as a 3rd argument, return the highest
## parent in the object hierarchy that matches the condition, instead
## of the first (nearest) one.
## @seealso{get, set}
## @end deftypefn

function p = ancestor (h, type, toplevel)

  if (nargin == 2 || nargin == 3)
    p = [];
    if (ischar (type))
      type = { type };
    endif
    if (iscellstr (type))
      look_first = true;
      if (nargin == 3)
        if (ischar (toplevel) && strcmpi (toplevel, "toplevel"))
          look_first = false;
        else
          error ("ancestor: third argument must be \"toplevel\"");
        endif
      endif
      while (true)
        if (isempty (h) || ! ishandle (h))
          break;
        endif
        if (any (strcmpi (get (h, "type"), type)))
          p = h;
          if (look_first)
            break;
          endif
        endif
                h = get (h, "Parent");
      endwhile
    else
      error ("ancestor: second argument must be a string or cell array of strings");
    endif
  else
    print_usage ();
  endif

endfunction
