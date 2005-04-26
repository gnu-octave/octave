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
## @deftypefn {Function File} {} strerror (@var{name}, @var{num})
## Return the text of an error message for function @var{name}
## corresponding to the error number @var{num}.  This function is intended
## to be used to print useful error messages for those functions that
## return numeric error codes.
## @end deftypefn

## Author: jwe

function msg = strerror (name, err)

  if (nargin != 2)
    usage ("strerror (name, err)");
  endif

  if (! isstr (name))
    error ("strerror: first argument must be a string");
  endif

  if (! isscalar (err))
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
