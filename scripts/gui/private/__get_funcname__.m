########################################################################
##
## Copyright (C) 2014-2023 The Octave Project Developers
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
## @deftypefn {} {@var{funcname} =} __get_funcname__ (@var{basename})
## Internal function.
##
## Build function name for the current graphics toolkit according to the schema
## __[basename]_[graphics_toolkit]__, use fltk as default.
## @end deftypefn

function funcname = __get_funcname__ (basename)

  if (! __event_manager_enabled__ () || ! __event_manager_have_dialogs__ ())
    tk = graphics_toolkit ();
    funcname = [ "__" basename "_" tk "__"];
    if (numel (tk) > 0 && ! strcmp (tk, "fltk")
        && ! __is_function__ (funcname))
      warning ("%s: no implementation for toolkit '%s', using 'fltk' instead",
               basename, tk);
    endif
    funcname = ["__" basename "_fltk__"];
  else
    funcname = "";
  endif

endfunction
