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
## @deftypefn {Function File} {} median (@var{x})
## If @var{x} is a vector, compute the median value of the elements of
## @var{x}.
## @iftex
## @tex
## $$
## {\rm median} (x) =
##   \cases{x(\lceil N/2\rceil), & $N$ odd;\cr
##           (x(N/2)+x(N/2+1))/2, & $N$ even.}
## $$
## @end tex
## @end iftex
## @ifinfo
##
## @example
## @group
##             x(ceil(N/2)),             N odd
## median(x) =
##             (x(N/2) + x((N/2)+1))/2,  N even
## @end group
## @end example
## @end ifinfo
## If @var{x} is a matrix, compute the median value for each
## column and return them in a row vector.
## @seealso{std, mean}
## @end deftypefn

## Author: jwe

function retval = median (a, dim)

  if (nargin != 1 && nargin != 2)
    usage ("median (a, dim)");
  endif
  if (nargin < 2)
    dim = min (find (size (a) > 1));
    if (isempty (dim))
      dim = 1;
    endif
  endif

  sz = size (a);
  s = sort (a, dim);
  if (numel (a) > 0)
    if (numel (a) == sz(dim))
      if (rem (sz(dim), 2) == 0)
	i = sz(dim) / 2;
	retval = (s(i) + s(i+1)) / 2;
      else
	i = ceil (sz(dim) /2);
	retval = s(i);
      endif
    else
      idx = cell ();
      nd = length (sz);
      for i = 1:nd
	idx{i} = 1:sz(i);
      endfor
      if (rem (sz(dim), 2) == 0)
	i = sz(dim) / 2;
	idx{dim} = i;
	retval = s(idx{:});
	idx{dim} = i+1;
	retval = (retval + s(idx{:})) / 2;
      else
	idx{dim} = ceil (sz(dim) / 2);
	retval = s(idx{:});
      endif
    endif
  else
    error ("median: invalid matrix argument");
  endif

endfunction
