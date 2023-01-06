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
## @deftypefn  {} {@var{value} =} getappdata (@var{h}, @var{name})
## @deftypefnx {} {@var{appdata} =} getappdata (@var{h})
## Return the @var{value} of the application data @var{name} for the graphics
## object with handle @var{h}.
##
## @var{h} may also be a vector of graphics handles.  If no second argument
## @var{name} is given then @code{getappdata} returns a structure,
## @var{appdata}, whose fields correspond to the appdata properties.
##
## @seealso{setappdata, isappdata, rmappdata, guidata, get, set, getpref,
## setpref}
## @end deftypefn

function value = getappdata (h, name)

  if (nargin < 1)
    print_usage ();
  endif

  if (! all (ishghandle (h(:))))
    error ("getappdata: H must be a scalar or vector of graphic handles");
  endif

  if (nargin == 2)
    if (! ischar (name))
      error ("getappdata: NAME must be a string");
    endif

    value = cell (numel (h), 1);
    for i = 1:numel (h)
      try
        value{i} = (get (h(i), "__appdata__")).(name);
      end_try_catch
    endfor

    if (i == 1)
      value = value{1};
    endif

  else  # nargin == 1
    if (numel (h) != 1)
      error ("getappdata: Only one handle H may be used when fetching appdata");
    endif

    value = get (h, "__appdata__");
    if (isempty (value))
      value = struct ();
    endif
  endif

endfunction


%!test
%! unwind_protect
%!   setappdata (0, "%data1%", ones (3), "%data2%", "hello world");
%!   assert (getappdata (0, "%data1%"), ones (3));
%!   assert (getappdata (0, "%data2%"), "hello world");
%!   appdata = getappdata (0);
%!   name1 = "%data1%";  name2 = "%data2%";
%!   assert (appdata.(name1), ones (3));
%!   assert (appdata.(name2), "hello world");
%! unwind_protect_cleanup
%!   rmappdata (0, "%data1%", "%data2%");
%! end_unwind_protect

## Test input validation
%!error <Invalid call> getappdata ()
%!error <H must be a scalar .* graphic handle> getappdata (-1, "hello")
%!error <NAME must be a string> getappdata (0, 1)
%!error <Only one handle H may be used when fetching appdata> getappdata ([0 0])
