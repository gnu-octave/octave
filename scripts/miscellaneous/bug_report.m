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

function bug_report ()

# usage: bug_report
#
# Have Octave create a bug report template file, invoke your favorite
# editor, and submit the report to the bug-octave mailing list when
# you are finished editing.

  if (nargin != 0)
    warning ("bug_report: ignoring extra arguments");
  endif

  printf ("Please enter a one-line description of your bug report.\n\n");
  fflush (stdout);

  subject = "";
  subject = input ("Subject: ", "s");

# XXX FIXME XXX -- really need a better system command, one that will
# automatically send output from the command to stdout...

  prefs = octave_tmp_file_name ();

  if (! isempty (prefs))
    fopen (prefs, "w");
    dump_prefs (prefs);
    fclose (prefs);
  endif

  cmd = "octave-bug";

  if (length (subject) > 0)
    cmd = sprintf ("%s -s \"%s\"", cmd, subject);
  endif

  if (! isempty (prefs))
    cmd = sprintf ("%s %s", cmd, prefs);
  endif

  system (sprintf ("%s > /dev/tty", cmd));

  if (! isempty (prefs))
    system (sprintf ("rm -f %s", prefs));
  endif

endfunction
