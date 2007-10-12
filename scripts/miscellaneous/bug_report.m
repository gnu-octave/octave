## Copyright (C) 1996, 1997 John W. Eaton
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
## @deftypefn {Function File} {} bug_report ()
## Have Octave create a bug report template file, invoke your favorite
## editor, and submit the report to the bug-octave mailing list when
## you are finished editing.
## @end deftypefn

## Author: jwe

function bug_report ()

  if (nargin != 0)
    warning ("bug_report: ignoring extra arguments");
  endif

  printf ("Please enter a one-line description of your bug report.\n\n");
  fflush (stdout);

  subject = "";
  subject = input ("Subject: ", "s");

  unwind_protect

    prefs = tmpnam ();

    if (! isempty (prefs))
      fid = fopen (prefs, "wt");
      if (fid > 0)
        dump_prefs (fid);
        fclose (fid);
      endif
    endif

    cmd = strcat ("octave-bug-", OCTAVE_VERSION);

    if (length (subject) > 0)
      cmd = sprintf ("%s -s \"%s\"", cmd, subject);
    endif

    if (! isempty (prefs))
      cmd = sprintf ("%s \"%s\"", cmd, prefs);
    endif

    system (cmd);

  unwind_protect_cleanup

    if (! isempty (prefs))
      unlink (prefs);
    endif

  end_unwind_protect

endfunction
