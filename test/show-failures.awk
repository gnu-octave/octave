#! /usr/bin/awk -f

########################################################################
##
## Copyright (C) 2016-2023 The Octave Project Developers
##
## See the file COPYRIGHT.md in the top-level directory of this
## distribution or <https://octave.org/copyright/>.
##
## This file is part of Octave.
##
## Octave is free software: you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by the
## Free Software Foundation, either version 3 of the License, or (at
## your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but WITHOUT
## ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
## FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
## for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <https://www.gnu.org/licenses/>.
##
########################################################################

BEGIN {
  buf = "";
  limit_output = 0;
  line = 0;
  line_limit = 32;
}
## Make the decision to print or not when we get to the next file
/^>>>>> |^Files with no tests:/ {
  print buf;

  buf = "";
  line = 0;
  limit_output = 0;
}
## The first line of the output from a failing test
/^\*\*\*\*\* / {
  line = 0;
  limit_output = 0;
}
/^!!!!! known (bug|failure)|^----- skipped test/ {
  limit_output = 1;
}
## Accumulate all lines that display the test code and up to line_limit
## lines of output from failed test.
{
  if (buf == "")
    {
      buf = $0;
    }
  else
    {
      if (limit_output)
        {
          if (line < line_limit)
            buf = buf "\n" $0;
          else if (line == line_limit)
            buf = buf "\n[skipping remaining output]";

          line++;
        }
      else
        buf = buf "\n" $0;
    }
}
