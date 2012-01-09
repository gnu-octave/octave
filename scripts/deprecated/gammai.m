## Copyright (C) 1994-2012 John W. Eaton
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
## @deftypefn {Function File} {} gammai (@var{a}, @var{x})
## This function is provided for compatibility with older versions of
## Octave.  New programs should use @code{gammainc} instead.
##
## @code{gammai (@var{a}, @var{x})} is the same as
## @code{gammainc (@var{x}, @var{a})}.
## @end deftypefn

## Author: jwe
## Created: 30 Jan 1998

## Deprecated in version 3.4

function retval = gammai (a, x)
  persistent warned = false;
  if (! warned)
    warned = true;
    warning ("Octave:deprecated-function",
             "gammai is obsolete and will be removed from a future version of Octave; please use gammainc instead");
  endif

  if (nargin == 2)
    retval = gammainc (x, a);
  else
    print_usage ();
  endif

endfunction
