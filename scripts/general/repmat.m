## Copyright (C) 2000 Paul Kienzle
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
## @deftypefn {Function File} {} repmat (@var{A}, @var{m}, @var{n})
## @deftypefnx {Function File} {} repmat (@var{A}, [@var{m} @var{n}])
## Form a block matrix of size @var{m} by @var{n}, with a copy of matrix
## @var{A} as each element.  If @var{n} is not specified, form an 
## @var{m} by @var{m} block matrix.
## @end deftypefn

## Author: Paul Kienzle <pkienzle@kienzle.powernet.co.uk>
## Created: July 2000

function x = repmat (a, m, n)

  if (nargin < 2 || nargin > 3)
    usage ("repmat (a, m, n)");
  endif

  if (nargin == 3)
    if (!isscalar (m) && !isscalar (n))
      error ("repmat: with 3 arguments m and n must be scalar");
    endif
    idx = [m, n];
  else 
    if (isscalar (m))
      idx = [m, m];
      n = m;
    elseif (isvector (m) && length (m) > 1)
      # Ensure that we have a row vector
      idx = m(:).';
    else
      error ("repmat: invalid dimensional argument");
    endif
  endif

  if (numel (a) == 1)
    if (isstr (a))
      x = setstr (toascii (a) * ones (idx));
    else
      x = a * ones(idx);
    endif
  elseif (ndims (a) == 2 && length (idx) < 3)
    if (isstr (a))
      x = setstr (kron (ones (idx), toascii (a)));
    else
      x = kron (ones (idx), a);
    endif
  else
    aidx = size(a);
    if (length(aidx) > length(idx))
      idx = [idx, ones(1,length(aidx)-length(idx))];
    elseif (length(aidx) < length(idx))
      aidx = [aidx, ones(1,length(idx)-length(aidx))];
    endif
    cidx = cell ();
    for i=1:length(aidx)
      cidx{i} = kron (ones (1, idx(i)), 1:aidx(i));
    endfor
    x = a (cidx{:});
  endif

endfunction
