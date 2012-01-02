## Copyright (C) 2008-2012 Ben Abbott
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
## @deftypefn {Function File} {} __finish__ ()
## Undocumented internal function.
## @end deftypefn

## Check for the existence of the function/script, @file{finish}, in the
## path or current working directory and execute it.  This function is
## intended to be excecuted upon a clean exit form Octave.  This is
## accomplished in the system script @file{startup/octaverc} by use of
## the built-in function @code{onexit}.

function __finish__ ()

  if (exist ("finish", "file"))
    ## No arg list here since finish might be a script.
    finish;
  endif

endfunction

## No test needed for internal helper function.
%!assert (1)
