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
## @deftypefn  {Function File} {@var{parent} =} ancestor (@var{h}, @var{type})
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
    p = cell (numel (h), 1);
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
      h = num2cell (h);
      for nh = 1:numel(h)
        while (true)
          if (isempty (h{nh}) || ! ishandle (h{nh}))
            break;
          endif
          if (any (strcmpi (get (h{nh}, "type"), type)))
            p{nh} = h{nh};
            if (look_first)
              break;
            endif
          endif
          h{nh} = get (h{nh}, "Parent");
        endwhile
      endfor
      if (nh == 1)
        p = p{1};
      endif
    else
      error ("ancestor: second argument must be a string or cell array of strings");
    endif
  else
    print_usage ();
  endif

endfunction

%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   l = line;
%!   assert (ancestor (l, "axes"), gca);
%!   assert (ancestor (l, "figure"), hf);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect
