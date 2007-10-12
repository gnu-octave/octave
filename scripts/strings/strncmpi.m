## Copyright (C) 2000, 2006, 2007 Bill Lash
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
## @deftypefn {Function File} {} strncmpi (@var{s1}, @var{s2}, @var{n})
## Ignoring case, return 1 if the first @var{n} characters of character
## strings @var{s1} and @var{s2} are the same, and 0 otherwise.
##
## If either @var{s1} or @var{s2} is a cell array of strings, then an array
## of the same size is returned, containing the values described above for
## every member of the cell array. The other argument may also be a cell
## array of strings (of the same size or with only one element), char matrix
## or character string.
##
## @strong{Caution:} For compatibility with @sc{Matlab}, Octave's strncmpi
## function returns 1 if the character strings are equal, and 0 otherwise.
## This is just the opposite of the corresponding C library function.
## @seealso{strcmp, strcmpi, strncmp}
## @end deftypefn

function retval = strncmpi (s1, s2, n)

  if (nargin == 3)
    ## Note that we don't use tolower here because we need to be able to
    ## handle cell arrays of strings.
    retval = strncmp (lower (s1), lower (s2), n);
  else
    print_usage ();
  endif

endfunction
