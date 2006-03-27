## Copyright (C) 1996, 1997 John W. Eaton
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
## @deftypefn {Function File} {} toc ()
## See tic.
## @end deftypefn

## Author: jwe

function secs = toc ()

  if (nargin != 0)
    warning ("toc: ignoring extra arguments");
  endif

  global __tic_toc_timestamp__ = -1;

  if (__tic_toc_timestamp__ < 0)
    warning ("toc called before timer set");
    secs0 = [];
  else
    secs0 = etime (clock (), __tic_toc_timestamp__);
    if (nargout != 0)
      secs = secs0;
    else
      printf ("Elapsed time is %f seconds.\n", secs0);
    endif  
  endif

endfunction
