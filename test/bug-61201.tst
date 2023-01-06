########################################################################
##
## Copyright (C) 2010-2023 The Octave Project Developers
##
## See the file COPYRIGHT.md in the top-level directory of this
## distribution or <https://octave.org/copyright/>.
##
## This file is part of Octave.
##
## Octave is free software: you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <https://www.gnu.org/licenses/>.
##
########################################################################

%!function lineno = test_conditional_lines (type)
%!  lineno = -42;
%!  warning ("error", "Octave:possible-matlab-short-circuit-operator", "local");
%!  try
%!    switch (type)
%!      case "while"
%!        k = 0;
%!        while (k < 2 & true)  ## line 8
%!          k++;
%!        endwhile
%!      case "until"
%!        k = 0;
%!        do
%!          k++;
%!        until (k > 2 & true);  ## line 15, but see below...
%!      case "if"
%!        k = 2;
%!        if (k < 2 & true)  ## line 18
%!        endif
%!      case "elseif"
%!        k = 2;
%!        if (false)
%!        elseif (k < 2 & true)  ## line 23
%!        endif
%!    endswitch
%!  catch (ex)
%!    lineno = ex.stack(1).line;
%!  end_try_catch
%!endfunction

%!assert<*61201> (test_conditional_lines ("while"), 8)

%!assert<*61201> (test_conditional_lines ("if"), 18)

%!assert<*61201> (test_conditional_lines ("elseif"), 23)

## Because the DO-UNTIL statement is not part of Matlab it is not
## eligible for Matlab-style short-circuit behavior in Octave.
%!assert<*61201> (test_conditional_lines ("until"), -42)
