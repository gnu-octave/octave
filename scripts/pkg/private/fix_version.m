## Copyright (C) 2005-2012 Søren Hauberg
## Copyright (C) 2010 VZLU Prague, a.s.
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
## @deftypefn {Function File} {@var{out} =} fix_version (@var{v})
## Undocumented internal function.
## @end deftypefn

## Make sure the version string v is a valid x.y.z version string
## Examples: "0.1" => "0.1.0", "monkey" => error(...).
function out = fix_version (v)
  dots = find (v == ".");
  if (length (dots) == 1)
    major = str2num (v(1:dots-1));
    minor = str2num (v(dots+1:end));
    if (length (major) != 0 && length (minor) != 0)
      out = sprintf ("%d.%d.0", major, minor);
      return;
    endif
  elseif (length (dots) == 2)
    major = str2num (v(1:dots(1)-1));
    minor = str2num (v(dots(1)+1:dots(2)-1));
    rev = str2num (v(dots(2)+1:end));
    if (length (major) != 0 && length (minor) != 0 && length (rev) != 0)
      out = sprintf ("%d.%d.%d", major, minor, rev);
      return;
    endif
  endif
  error ("bad version string: %s", v);
endfunction

