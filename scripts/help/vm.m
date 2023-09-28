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
## For more information on each command and available options use
## @code{help CMD}.
##
## The VM commands available in Octave are
##
## @table @code
## @item __vm_enable__
## The main VM user function.  Switch on or off the VM as a whole.
##
## @item __vm_compile__
## Another VM user function.  Compiles a specified function to bytecode.
##
## @item __vm_clear_cache__
## Internal function.  Clears the cache of already-processed code.
##
## @item __vm_is_executing__
## Internal function.  Returns true if the VM is executing.
##
## @item __vm_print_trace__
## Internal function.  Print a debug trace from the VM.
##
## @item __vm_profile__
## Internal function.  Profile the code running in the VM.
##
## @end table
##
## @noindent
##
## @seealso{__vm_enable__, __vm_compile__, __vm_clear_cache__,
## __vm_is_executing__, __vm_print_trace__, __vm_profile__}
## @end deftypefn

function vm ()
  help ("vm");
endfunction

## Mark file as being tested.  No real test needed for a documentation .m file
%!assert (1)
