# Copyright (C) 1996 John W. Eaton
#
# This file is part of Octave.
#
# Octave is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation; either version 2, or (at your option) any
# later version.
#
# Octave is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
# for more details.
#
# You should have received a copy of the GNU General Public License
# along with Octave; see the file COPYING.  If not, write to the Free
# Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

function retval = is_leap_year (year)

# usage: is_leap_year (year)
#
# Return 1 if the given year is a leap year; otherwise, return 0.
# With no arguments, use the current year.

  if (nargin > 1)
    usage ("is_leap_year (year)");
  endif

  if (nargin == 0)
    t = clock ();
    year = t (1);
  endif

  retval = ((rem (year, 4) == 0 & rem (year, 100) != 0) ...
	    | rem (year, 400) == 0);

endfunction
