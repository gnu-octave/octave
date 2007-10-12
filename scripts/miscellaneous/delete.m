## Copyright (C) 2004, 2005, 2006, 2007 John W. Eaton
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
## @deftypefn {Function File} {} delete (@var{file})
## @deftypefnx {Function File} {} delete (@var{h})
## Delete the named file or figure handle.
## @end deftypefn

## PKG_ADD: mark_as_command delete

## Author: jwe

function delete (arg)

  if (nargin == 1)
    if (ischar (arg))
      unlink (arg);
    elseif (ishandle (arg))
      ## Delete a graphics object.
      __go_delete__ (arg);
    else
      error ("delete: expecting argument to be a filename or graphics handle");
    endif
  else
    print_usage ();
  endif

endfunction
