## Copyright (C) 2000 Bill Lash
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
## @deftypefn {Function File} {} strcmpi (@var{s1}, @var{s2})
## Ignoring case, return 1 if the character strings @var{s1} and @var{s2}
## are the same, and 0 otherwise.
##
## If either @var{s1} or @var{s2} is a cell array of strings, then an array
## of the same size is returned, containing the values described above for
## every member of the cell array. The other argument may also be a cell
## array of strings (of the same size or with only one element), char matrix
## or character string.
##
## @strong{Caution:} For compatibility with @sc{Matlab}, Octave's strcmpi
## function returns 1 if the character strings are equal, and 0 otherwise.
## This is just the opposite of the corresponding C library function.
## @seealso{strcmp, strncmp, strncmpi}
## @end deftypefn

## Author: Bill Lash <lash@tellabs.com>
## Adapted-by: jwe

function retval = strcmpi (s1, s2)

  if (nargin == 2)
    if ((ischar(s1) || iscellstr(s1)) && (ischar(s2) || iscellstr(s2)))
      ## Note that we don't use tolower here because we need to be able
      ## to handle cell arrays of strings.
      retval = strcmp (lower (s1), lower (s2));
    else
      retval = false;
    endif
  else
    print_usage ();
  endif

endfunction
