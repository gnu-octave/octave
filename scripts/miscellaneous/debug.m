## Copyright (C) 2008-2011 David Bateman
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
## @deftypefn {Function File} {} debug ()
## Summary of the debugging commands.  The debugging commands that are
## available in Octave are
##
## @table @code
## @item keyboard
## Force entry into debug mode.
##
## @item dbstop
## Add a breakpoint.
##
## @item dbclear
## Remove a breakpoint.
##
## @item dbstatus
## List all breakpoints.
##
## @item dbcont
## Continue execution from the debug prompt.
##
## @item dbstack
## Print a backtrace of the execution stack.
##
## @item dbstep
## Execute one or more lines and re-enter debug mode
##
## @item dbtype
## List the function where execution is currently stopped, enumerating
## the lines.
##
## @item dbup
## The workspace up the execution stack.
##
## @item dbdown
## The workspace down the execution stack.
##
## @item dbquit
## Quit debugging mode and return to the main prompt.
##
## @item debug_on_error
## Flag whether to enter debug mode in case Octave encounters an error.
##
## @item debug_on_warning
## Flag whether to enter debug mode in case Octave encounters a warning.
##
## @item debug_on_interrupt
## Flag whether to enter debug mode in case Octave encounters an interupt.
## 
## @end table
##
## @noindent
## when Octave encounters a breakpoint or other reason to enter debug
## mode, the prompt changes to @code{"debug>"}.  The workspace of the function
## where the breakpoint was encountered becomes available and any Octave
## command that works within that workspace may be executed.
##
## @seealso{dbstop, dbclear, dbstatus, dbcont, dbstack, dbstep, dbtype,
## dbup, dbdown, dbquit, debug_on_error, debug_on_warning,
## debug_on_interrupt}
## @end deftypefn

function debug ()
  help ("debug");
endfunction
