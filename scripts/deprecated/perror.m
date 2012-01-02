## Copyright (C) 1993-2012 John W. Eaton
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
## @deftypefn {Function File} {} perror (@var{funcname}, @var{num})
## Print the error message for function @var{funcname} corresponding to the
## error number @var{num}.  This function is intended to be used to print
## useful error messages for those functions that return numeric error
## codes.
## @seealso{strerror}
## @end deftypefn

## Author: jwe

function perror (funcname, num)

  persistent warned = false;
  if (! warned)
    warned = true;
    warning ("Octave:deprecated-function",
             "perror is obsolete and will be removed from a future version of Octave.");
  endif

  if (nargin != 2)
    print_usage ();
  else
    printf (strerror (funcname, num));
  endif

endfunction
