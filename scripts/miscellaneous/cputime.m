# Copyright (C) 1995 John W. Eaton
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
# Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

function retval = cputime ()

# usage: cputime
#
# Get CPU time statistics.

  if (nargin != 0)
    warning ("cputime: ignoring extra arguments");
  endif

  resource_stats = getrusage ();

  usr = resource_stats.ru_utime;
  sys = resource_stats.ru_stime;

  usr_time = usr.tv_sec + usr.tv_usec / 1e6;
  sys_time = sys.tv_sec + sys.tv_usec / 1e6;

  retval = zeros (1, 3);

  retval (1) = usr_time + sys_time;
  retval (2) = usr_time;
  retval (3) = sys_time;

endfunction
