## Copyright (C) 2004 John W. Eaton
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
## @deftypefn {Function File} {} computer ()
## Print or return a string of the form @var{cpu}-@var{vendor}-@var{os}
## that identifies the kind of computer Octave is running on.  If invoked
## with an output argument, the value is returned instead of printed.  For
## example,
##
## @example
## @group
## computer ()
##      @print{} i586-pc-linux-gnu
##
## x = computer ()
##      @result{} x = "i586-pc-linux-gnu"
## @end group
## @end example
## @end deftypefn

function retval = computer ()

  if (nargin != 0)
    warning ("computer: ignoring extra arguments");
  endif

  msg = octave_config_info ("canonical_host_type");

  if (strcmp (msg, "unknown"))
    msg = "Hi Dave, I'm a HAL-9000";
  endif

  if (nargout == 0)
    printf ("%s\n", msg);
  else
    retval = msg;
  endif

endfunction
