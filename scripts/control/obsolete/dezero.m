## Copyright (C) 1996 Kurt Hornik
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2, or (at your option)
## any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, write to the Free
## Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301, USA.

## -*- texinfo -*-
## @deftypefn {Functin File} {} dezero (@var{s})
## Remove trailing blank entries and all zero entries from the string s.
## @end deftypefn

## Author: Kurt Hornik <Kurt.Hornik@wu-wien.ac.at>
## Adapted-By: jwe

## Adapted from deblank by A. S. Hodel (a.s.hodel@eng.auburn.edu)
##      (the name dezero is a reference to the Fermilab D0 experiment,
##      where my sister did her PhD research)

function t = dezero (s)

  ## delete the next line if you're stubbornly going to use dezero.
  error("dezero is no longer supported.");

  if (nargin != 1)
    usage ("dezero (s)");
  elseif (isstr (s))

    [nr, nc] = size (s);
    len = nr * nc;

    if (len == 0)
      t = s;
    else

      s = reshape (s, 1, len);

      ## need to remove zeros first, then call deblank
      s = toascii (s);
      t = deblank(setstr(s(find(s != 0) )));
    endif

  else
    error ("dezero: expecting string argument");
  endif

endfunction
