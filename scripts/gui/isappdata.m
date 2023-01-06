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
## @deftypefn {} {@var{valid} =} isappdata (@var{h}, @var{name})
## Return true if the named application data, @var{name}, exists for the
## graphics object with handle @var{h}.
##
## @var{h} may also be a vector of graphics handles.
## @seealso{getappdata, setappdata, rmappdata, guidata, get, set, getpref,
## setpref}
## @end deftypefn

function valid = isappdata (h, name)

  if (nargin < 1)
    print_usage ();
  endif

  if (! all (ishghandle (h(:))))
    error ("isappdata: H must be a scalar or vector of graphic handles");
  elseif (! ischar (name))
    error ("isappdata: NAME must be a string");
  endif

  valid = false (size (h));
  for i = 1:numel (h)
    appdata = get (h(i), "__appdata__");
    if (isfield (appdata, name))
      valid(i) = true;
    endif
  endfor

endfunction


%!test
%! unwind_protect
%!   setappdata (0, "%hello%", "world");
%!   assert (isappdata (0, "%hello%"), true);
%!   assert (isappdata ([0 0], "%hello%"), [true, true]);
%!   assert (isappdata (0, "%foobar%"), false);
%! unwind_protect_cleanup
%!   rmappdata (0, "%hello%");
%! end_unwind_protect

## Test input validation
%!error <Invalid call> isappdata ()
%!error <H must be a scalar .* graphic handle> isappdata (-1, "hello")
%!error <NAME must be a string> isappdata (0, 1)
