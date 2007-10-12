## Copyright (C) 1998 John W. Eaton
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
## @deftypefn {Function File} {} randperm (@var{n})
## Return a row vector containing a random permutation of the
## integers from 1 to @var{n}.
## @end deftypefn

## Author: "James R. Van Zandt" <jrv@vanzandt.mv.com>
## Adapted-By: jwe

function retval = randperm (n)

  if (nargin == 1 && isscalar (n) && floor (n) == n)
    if (n >= 0)
      [junk, retval] = sort (rand (1, n));
    else
      error ("randperm: argument must be non-negative");
    endif
  else
    print_usage ();
  endif

endfunction
