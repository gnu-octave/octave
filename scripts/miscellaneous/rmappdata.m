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
## @deftypefn {Function File} {} rmappdata (@var{h}, @var{name})
## Delete the named application data for the object(s) with
## handle(s) @var{h}.
## @end deftypefn

## Author: Ben Abbott <bpabbott@mac.com>
## Created: 2010-07-15

function rmappdata (h, varargin)

  if (! (all (ishandle (h)) && iscellstr (varargin)))
    error ("rmappdata: invalid input");
  endif

  for nh = 1:numel(h)
    appdata = get (h(nh), "__appdata__");
    appdata = rmfield (appdata, varargin);
    set (h(nh), "__appdata__", appdata);
  endfor

endfunction

%!test
%! setappdata (0, "hello", "world")
%! rmappdata (0, "hello")
%! assert (isappdata (0, "hello"), false)

