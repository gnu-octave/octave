## Copyright (C) 2010 Ben Abbott
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
## @deftypefn {Function File} {@var{value} =} getappdata (@var{h}, @var{name})
## Returns the @var{value} for named application data for the object(s) with
## handle(s) @var{h}.
## @end deftypefn

## Author: Ben Abbott <bpabbott@mac.com>
## Created: 2010-07-15

function val = getappdata (h, name)

  if (! (all (ishandle (h)) && ischar (name)))
    error ("getappdata: invalid input.")
  endif

  appdata(numel(h)) = struct();
  for nh = 1:numel(h)
    appdata(nh) = get (h(nh), "__appdata__");
  end
  if (nh > 1)
    val = {appdata.(name)};
  else
    val = appdata.(name);
  endif

endfunction

