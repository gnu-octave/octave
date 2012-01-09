## Copyright (C) 1995-2012 John W. Eaton
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
## @deftypefn {Function File} {} strerror (@var{name}, @var{num})
## Return the text of an error message for function @var{name}
## corresponding to the error number @var{num}.  This function is intended
## to be used to print useful error messages for those functions that
## return numeric error codes.
## @end deftypefn

## Author: jwe

function msg = strerror (name, num)

  persistent warned = false;
  if (! warned)
    warned = true;
    warning ("Octave:deprecated-function",
             "strerror is obsolete and will be removed from a future version of Octave.");
  endif

  if (nargin != 2)
    print_usage ();
  endif

  if (! ischar (name))
    error ("strerror: first argument must be a string");
  endif

  if (! isscalar (num))
    error ("strerror: second argument must be a scalar");
  endif

  if (strcmp (name, "fsolve"))

    if (num == -2)
      msg = "input error\n";
    elseif (num == -1)
      msg = "error encountered in user-supplied function\n";
    elseif (num == 1)
      msg = "solution converged to requested tolerance\n";
    elseif (num == 3)
      msg = "iteration is not making good progress\n";
    elseif (num == 4)
      msg = "iteration limit exceeded\n";
    else
      error ("strerror: unrecognized error code for fsolve");
    endif

  else

    error ("strerror: unrecognized function NAME");

  endif

endfunction
