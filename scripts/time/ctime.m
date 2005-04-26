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
## @deftypefn {Function File} {} ctime (@var{t})
## Convert a value returned from @code{time} (or any other nonnegative
## integer), to the local time and return a string of the same form as
## @code{asctime}.  The function @code{ctime (time)} is equivalent to
## @code{asctime (localtime (time))}.  For example,
##
## @example
## @group
## ctime (time ())
##      @result{} "Mon Feb 17 01:15:06 1997\n"
## @end group
## @end example
## @end deftypefn

## Author: jwe

function retval = ctime (t)

  if (nargin == 1)
    retval = asctime (localtime (t));
  else
    usage ("ctime (TIME)");
  endif

endfunction
