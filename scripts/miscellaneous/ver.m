## Copyright (C) 2005, 2006, 2007 William Poetra Yoga Hadisoeseno
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
## @deftypefn {Function File} {} ver ()
## Display a header containing the current Octave version
## number, license string and operating system.
## @seealso{license, version}
## @end deftypefn

## Author: William Poetra Yoga Hadisoeseno <williampoetra@gmail.com>

function ver ()

  if (nargin > 0)
    print_usage ();
  endif

  octave_license = license ();

  [unm, status] = uname ();

  if (status < 0)
    os_string = "unknown";
  else
    os_string = sprintf ("%s %s %s %s", unm.sysname, unm.release,
			 unm.version, unm.machine);
  endif

  hbar(1:70) = "-";
  ver_line1 = "GNU Octave Version ";
  ver_line2 = "GNU Octave License: ";
  ver_line3 = "Operating System: ";

  ver_desc = sprintf ("%s\n%s%s\n%s%s\n%s%s\n%s\n", hbar, ver_line1, version,
                      ver_line2, octave_license, ver_line3, os_string, hbar);

  puts (ver_desc);

endfunction
