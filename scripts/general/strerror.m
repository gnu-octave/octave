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
## Software Foundation, 59 Temple Place - Suite 330, Boston, MA
## 02111-1307, USA.

## usage: msg = strerror (name, err)
##
## Return the text of an error message for error number `err' from
## function "name".

## Author: jwe

function msg = strerror (name, err)

  if (nargin != 2)
    usage ("strerror (name, err)");
  endif

  if (! isstr (name))
    error ("strerror: first argument must be a string");
  endif

  if (! is_scalar (err))
    error ("strerror: second argument must be a scalar");
  endif

  if (strcmp (name, "fsolve"))

    if (err == -2)
      msg = "input error\n";
    elseif (err == -1)
      msg = "error encountered in user-supplied function\n";
    elseif (err == 1)
      msg = "solution converged to requested tolerance\n";
    elseif (err == 4)
      msg = "iteration limit exceeded\n";
    elseif (err == 3)
      msg = "iteration is not making good progress\n";
    else
      error ("strerror: unrecognized error code for fsolve");
    endif

  else

    error ("strerror: unrecognized function name");

  endif

endfunction
