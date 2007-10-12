## Copyright (C) 1996, 1997 John W. Eaton
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
## @deftypefn {Function File} {} nargchk (@var{nargin_min}, @var{nargin_max}, @var{n})
## If @var{n} is in the range @var{nargin_min} through @var{nargin_max}
## inclusive, return the empty matrix.  Otherwise, return a message
## indicating whether @var{n} is too large or too small.
##
## This is useful for checking to see that the number of arguments supplied
## to a function is within an acceptable range.
## @end deftypefn

## Author: jwe

function retval = nargchk (nargin_min, nargin_max, n)

  if (nargin != 3)
    print_usage ();
  endif

  if (nargin_min > nargin_max)
    error  ("nargchk: nargin_min > nargin_max");
  endif

  if (n < nargin_min)
    retval = "nargchk: N is less than nargin_min";
  elseif (n > nargin_max)
    retval = "nargchk: N is greater than nargin_max";
  else
    retval = [];
  endif

endfunction
