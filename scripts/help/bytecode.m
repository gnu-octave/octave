########################################################################
##
## Copyright (C) 2023-2024 The Octave Project Developers
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

## -*- texinfo -*-
## @deftypefn {} {} bytecode ()
## Summary of commands related to Octave's bytecode interpreter.
##
## As of Octave 9, the bytecode interpreter is considered @emph{experimental}.
## The user is encouraged to test it with that in mind.  All bytecode
## functions, being experimental, may be renamed in future.
##
## To switch on the bytecode interpreter, type: @code{__vm_enable__ (1)}
##
## To switch it off, type: @code{__vm_enable__ (0)}
##
## To always use it, add @code{__vm_enable__ (1)}
## to your Octave startup file (@file{.octaverc} or similar).
##
## For more information on each command and available options use
## @code{help CMD}.
##
## Other useful bytecode commands available in Octave are:
##
## @table @code
## @item __vm_compile__
## Compile a specified function to bytecode.
##
## @item __vm_profile__
## Profile the code running in the bytecode interpreter.
##
## @end table
##
## There are also several private functions whose names also begin with
## @code{__vm_}.  These are intended for developer use.
##
## @c FIXME: Use seealso macro when functions are no longer experimental.
## See also: __vm_enable__, __vm_compile__, __vm_profile__.
## @end deftypefn

function bytecode ()
  help ("bytecode");
endfunction


## Mark file as being tested.  No real test needed for a documentation .m file
%!assert (1)
