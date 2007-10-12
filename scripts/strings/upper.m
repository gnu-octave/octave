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
## @deftypefn {Function File} {} upper (@var{s})
## Transform all letters in the character string (or cell array of
## character strings) @var{s} to upper case.
## @seealso{lower, tolower, toupper}
## @end deftypefn

## Author: jwe

function retval = upper (s)

  if (nargin != 1)
    print_usage ();
  endif

  if (ischar (s))
    retval = toupper (s);
  elseif (iscellstr (s))
    retval = cellfun (@toupper, s, "UniformOutput", false);
  else
    error ("upper: `s' must be a string or cell array of strings");
  endif

endfunction
