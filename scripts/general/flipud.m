## Copyright (C) 1996, 1997 John W. Eaton
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
## @deftypefn {Function File} {} flipud (@var{x})
## Return a copy of @var{x} with the order of the rows reversed.  For
## example,
##
## @example
## @group
## flipud ([1, 2; 3, 4])
##      @result{}  3  4
##          1  2
## @end group
## @end example
##
## Due to the difficulty of defining which axis about which to flip the 
## matrix @code{flipud} only work with 2-d arrays.  To flip N-d arrays
## use @code{flipdim} instead.
## @end deftypefn
##
## @seealso{fliplr, flipdim, rot90 and rotdim}

## Author: jwe

function y = flipud (x)

  if (nargin != 1)
    usage ("flipud (x)");
  endif

  if (ndims (x) > 2)
    error ("flipud: Only works with 2-d arrays")
  endif

  nr = rows (x);
  y = x (nr:-1:1, :);

endfunction
