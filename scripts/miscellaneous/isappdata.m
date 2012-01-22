## Copyright (C) 2010-2012 Ben Abbott
##
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2 of the License, or
## (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn {Function File} {@var{V} =} isappdata (@var{h}, @var{name})
## Return true if the named application data, @var{name}, exists for the
## object with handle @var{h}.
## @seealso{getappdata, setappdata, rmappdata}
## @end deftypefn

## Author: Ben Abbott <bpabbott@mac.com>
## Created: 2010-07-15

function res = isappdata (h, name)

  if (! (all (ishandle (h)) && ischar (name)))
    error ("isappdata: invalid input");
  endif

  for nh = 1:numel(h)
    data = get (h(nh));
    if (isfield (data, "__appdata__") && isfield (data.__appdata__, name))
      res(nh) = true;
    else
      res(nh) = false;
    endif
  endfor

endfunction

%!test
%! setappdata (0, "hello", "world")
%! assert (isappdata (0, "hello"), true)
%!assert (isappdata (0, "foobar"), false)

