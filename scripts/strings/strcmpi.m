## Copyright (C) 2000  Bill Lash
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
## Compare two strings, ignoring case, returning 1 if
## they are the same, and 0 otherwise.
##
## Note: For compatibility with Matlab, Octave's strcmpi function
## returns 1 if the strings are equal, and 0 otherwise.  This is
## just the opposite of the corresponding C library function.
## @end deftypefn

## Author: Bill Lash <lash@tellabs.com>
## Adapted-by: jwe

function status = strcmpi(s1, s2)

  if (nargin == 2)
    status = (ischar (s1) && ischar(s2) && strcmp (upper (s1), upper (s2)));
  else
    usage ("strcmpi (s, t)");
  endif

endfunction

