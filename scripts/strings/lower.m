## Copyright (C) 1998 John W. Eaton
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
## @deftypefn {Function File} {} lower (@var{s})
## Transform all letters in the character string (or cell array of
## character strings) @var{s} to lower case.
## @seealso{upper, tolower, toupper}
## @end deftypefn

## Author: jwe

function retval = lower (s)

  if (nargin != 1)
    usage ("lower (s)");
  endif

  if (ischar (s))
    retval = tolower (s);
  elseif (iscellstr (s))
    retval = cellfun (@tolower, s, "UniformOutput", false);
  else
    error ("lower: `s' must be a string or cell array of strings");
  endif

endfunction
