# Copyright (C) 1994 John W. Eaton
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

function bug_report ()

# usage: bug_report
#
# Have Octave create a bug report template file, invoke your favorite
# editor, and submit the report to the bug-octave mailing list when
# you are finished editing.

  if (nargin != 0)
    disp ("bug_report: ignoring extra arguments");
  endif

  printf ("Please enter a one-line description of your bug report.\n\n");
  fflush (stdout);

  subject = "";
  subject = input ("Subject: ", "s");

# XXX FIXME XXX -- really need a better system command, one that will
# automatically send output from the command to stdout...

  if (length (subject) > 0)
    system (sprintf ("octave-bug -s \"%s\" > /dev/tty", subject));
  else
    system ("octave-bug > /dev/tty");
  endif

endfunction
