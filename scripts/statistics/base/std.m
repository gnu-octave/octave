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
## @deftypefn {Function File} {} std (@var{x})
## If @var{x} is a vector, compute the standard deviation of the elements
## of @var{x}.
## @iftex
## @tex
## $$
## {\rm std} (x) = \sigma (x) = \sqrt{{\sum_{i=1}^N (x_i - \bar{x}) \over N - 1}}
## $$
## @end tex
## @end iftex
## @ifinfo
##
## @example
## @group
## std (x) = sqrt (sumsq (x - mean (x)) / (n - 1))
## @end group
## @end example
## @end ifinfo
## If @var{x} is a matrix, compute the standard deviation for
## each column and return them in a row vector.
## @end deftypefn
## @seealso{mean and median}

## Author: jwe

function retval = std (a)

  if (nargin != 1)
    usage ("std (a)");
  endif

  nr = rows (a);
  nc = columns (a);
  if (nc == 1 && nr == 1)
    retval = 0;
  elseif (nc == 1 || nr == 1)
    n = length (a);
    retval = sqrt (sumsq (a - mean (a)) / (n - 1));
  elseif (nr > 1 && nc > 0)
    retval = sqrt (sumsq (a - ones (nr, 1) * mean (a)) / (nr - 1));
  else
    error ("std: invalid matrix argument");
  endif

endfunction
