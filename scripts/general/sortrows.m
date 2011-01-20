## Copyright (C) 2000-2011 Daniel Calvelo
## Copyright (C) 2009 Jaroslav Hajek
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
## @deftypefn {Function File} {} sortrows (@var{A}, @var{c})
## Sort the rows of the matrix @var{A} according to the order of the
## columns specified in @var{c}.  If @var{c} is omitted, a
## lexicographical sort is used.  By default ascending order is used
## however if elements of @var{c} are negative then the corresponding
## column is sorted in descending order.
## @end deftypefn

## Author: Daniel Calvelo, Paul Kienzle
## Adapted-by: jwe

function [s, i] = sortrows (A, c)

  default_mode = "ascend";
  other_mode = "descend";

  if (issparse (A))
    ## FIXME -- eliminate this case once __sort_rows_idx__ is fixed to
    ## handle sparse matrices.
    if (nargin == 1)
      i = sort_rows_idx_generic (default_mode, other_mode, A);
    else
      i = sort_rows_idx_generic (default_mode, other_mode, A, c);
    endif
  elseif (nargin == 1)
    i = __sort_rows_idx__ (A, default_mode);
  elseif (all (c > 0))
    i = __sort_rows_idx__ (A(:,c), default_mode);
  elseif (all (c < 0))
    i = __sort_rows_idx__ (A(:,-c), other_mode);
  else
    ## Otherwise, fall back to the old algorithm.
    i = sort_rows_idx_generic (default_mode, other_mode, A, c);
  endif

  ## Only bother to compute s if needed.
  if (isargout (1))
    s = A(i,:);
  endif

endfunction

function i = sort_rows_idx_generic (default_mode, other_mode, m, c)

  if (nargin == 3)
    indices = [1:size(m,2)]';
    mode(1:size(m,2)) = {default_mode};
  else
    for ii = 1:length (c);
      if (c(ii) < 0)
        mode{ii} = other_mode;
      else
        mode{ii} = default_mode;
      endif
    endfor
    indices = abs(c(:));
  endif

  ## Since sort is 'stable' the order of identical elements will be
  ## preserved, so by traversing the sort indices in reverse order we
  ## will make sure that identical elements in index i are subsorted by
  ## index j.
  indices = flipud (indices);
  mode = flipud (mode');
  i = [1:size(m,1)]';
  for ii = 1:length (indices);
    [trash, idx] = sort (m(i, indices(ii)), mode{ii});
    i = i(idx);
  endfor

endfunction


%!test
%! m = [1, 1; 1, 2; 3, 6; 2, 7];
%! c = [1, -2];
%! [x, idx] = sortrows (m, c);
%! [sx, sidx] = sortrows (sparse (m), c);
%! assert (x, [1, 2; 1, 1; 2, 7; 3, 6]);
%! assert (idx, [2; 1; 4; 3]);
%! assert (issparse (sx));
%! assert (x, full (sx));
%! assert (idx, sidx);
