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
## Software Foundation, 59 Temple Place - Suite 330, Boston, MA
## 02111-1307, USA.

## -*- texinfo -*-
## @deftypefn {Function File} {} fliplr (@var{x})
## Return a copy of @var{x} with the order of the columns reversed.  For
## example,
##
## @example
## @group
## fliplr ([1, 2; 3, 4])
##      @result{}  2  1
##          4  3
## @end group
## @end example
##
## Note that @code{fliplr} only workw with 2-D arrays.  To flip N-d arrays
## use @code{flipdim} instead.
## @end deftypefn
##
## @seealso{flipud, flipdim, rot90 and rotdim}

## Author: jwe

function y = fliplr (x)

  if (nargin != 1)
    usage ("fliplr (x)");
  endif

  if (ndims (x) > 2)
    error ("fliplr: Only works with 2-D arrays")
  endif

  nc = columns (x);
  y = x (:, nc:-1:1);

endfunction
