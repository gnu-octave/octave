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

function [total, user, system] = cputime ()

# usage: cputime
#
# Get CPU time statistics.

  if (nargin != 0)
    warning ("cputime: ignoring extra arguments");
  endif

  resource_stats = getrusage ();

  usr = resource_stats.utime;
  sys = resource_stats.stime;

  user = usr.sec + usr.usec / 1e6;
  system = sys.sec + sys.usec / 1e6;
  total = user + system;

endfunction
