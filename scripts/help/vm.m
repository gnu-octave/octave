########################################################################
##
## Copyright (C) 2023 The Octave Project Developers
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
## @deftypefn {} {} vm ()
## Summary of commands related to Octave's virtual machine (VM) evaluator.
##
## As of Octave 9, the VM evaluator is considered @emph{experimental}.
## The user is encouraged to test the VM with that in mind. All VM functions,
## being experimental, may be renamed in future. Currently they are
## all named @code{__vm_X__}.
##
## To switch on the VM, type: @code{__vm_enable__ (1)}
##
## To switch off the VM, type: @code{__vm_enable__ (0)}
##
## To always use the VM, add @code{__vm_enable__ (1)}
## to your Octave startup file (.octaverc or similar).
##
## For more information on each command and available options use
## @code{help CMD}.
##
## The other VM commands available in Octave are:
##
## @table @code
## @item __vm_compile__
## Compile a specified function to bytecode.
##
## @item __vm_profile__
## Profile the code running in the VM.
##
## @end table
##
## There are also several private VM functions whose names also begin with
## @code{__vm_}.  These are meant for developer use.
##
## @noindent
## @end deftypefn

function vm ()
  help ("vm");
endfunction

## Mark file as being tested.  No real test needed for a documentation .m file
%!assert (1)
