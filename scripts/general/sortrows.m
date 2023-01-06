########################################################################
##
## Copyright (C) 2000-2023 The Octave Project Developers
##
## See the file COPYRIGHT.md in the top-level directory of this
## distribution or <https://octave.org/copyright/>.
##
## This file is part of Octave.
##
## Octave is free software: you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <https://www.gnu.org/licenses/>.
##
########################################################################

## -*- texinfo -*-
## @deftypefn  {} {[@var{s}, @var{i}] =} sortrows (@var{A})
## @deftypefnx {} {[@var{s}, @var{i}] =} sortrows (@var{A}, @var{c})
## Sort the rows of the matrix @var{A} according to the order of the columns
## specified in @var{c}.
##
## By default (@var{c} omitted, or a particular column unspecified in @var{c})
## an ascending sort order is used.  However, if elements of @var{c} are
## negative then the corresponding column is sorted in descending order.  If
## the elements of @var{A} are strings then a lexicographical sort is used.
##
## Example: sort by column 2 in descending order, then 3 in ascending order
##
## @example
## @group
## x = [ 7, 1, 4;
##       8, 3, 5;
##       9, 3, 6 ];
## sortrows (x, [-2, 3])
##    @result{} 8  3  5
##       9  3  6
##       7  1  4
## @end group
## @end example
##
## @seealso{sort}
## @end deftypefn

function [s, i] = sortrows (A, c)

  if (nargin < 1)
    print_usage ();
  endif

  if (nargin == 2)
    if (! (isnumeric (c) && isvector (c)))
      error ("sortrows: C must be a numeric vector");
    elseif (any (c == 0) || any (abs (c) > columns (A)))
      error ("sortrows: all elements of C must be in the range [1, columns (A)]");
    endif
  endif

  default_mode = "ascend";
  reverse_mode = "descend";

  if (issparse (A) || iscell (A))
    ## FIXME: Eliminate this case once __sort_rows_idx__ is fixed to
    ##        handle sparse matrices.
    if (nargin == 1)
      i = sort_rows_idx_generic (default_mode, reverse_mode, A);
    else
      i = sort_rows_idx_generic (default_mode, reverse_mode, A, c);
    endif
  elseif (nargin == 1)
    i = __sort_rows_idx__ (A, default_mode);
  elseif (all (c > 0))
    i = __sort_rows_idx__ (A(:,c), default_mode);
  elseif (all (c < 0))
    i = __sort_rows_idx__ (A(:,-c), reverse_mode);
  else
    ## Otherwise, fall back to the old algorithm.
    i = sort_rows_idx_generic (default_mode, reverse_mode, A, c);
  endif

  ## Only bother to compute s if needed.
  if (isargout (1))
    s = A(i,:);
  endif

endfunction

function i = sort_rows_idx_generic (default_mode, reverse_mode, m, c)

  if (nargin == 3)
    indices = [1:columns(m)]';
    mode(1:columns(m)) = {default_mode};
  else
    for j = 1:length (c)
      if (c(j) < 0)
        mode{j} = reverse_mode;
      else
        mode{j} = default_mode;
      endif
    endfor
    indices = abs (c(:));
  endif

  ## Since sort is 'stable' the order of identical elements will be
  ## preserved, so by traversing the sort indices in reverse order we
  ## will make sure that identical elements in index i are subsorted by
  ## index j.
  indices = flipud (indices);
  mode = flipud (mode');
  i = [1:rows(m)]';
  for j = 1:length (indices)
    M = m(i, indices(j));
    if (iscell (M) && ! iscellstr (M))
      M = cell2mat (M);
    endif
    [~, idx] = sort (M, mode{j});
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

%!test
%! m = [1, 0, 0, 4];
%! c = 1;
%! [x, idx] = sortrows (m, c);
%! [sx, sidx] = sortrows (sparse (m), c);
%! assert (x, m);
%! assert (idx, 1);
%! assert (issparse (sx));
%! assert (x, full (sx));
%! assert (idx, sidx);

%!test <*42523>
%! C = {1, 2, "filename1";
%!      3, 4, "filename2";
%!      5, 6, "filename3"};
%! C2 = sortrows (C, -1);
%! assert (C2, flipud (C));

## Test input validation
%!error <Invalid call> sortrows ()
%!error sortrows (1, "ascend")
%!error sortrows (1, ones (2,2))
%!error sortrows (1, 0)
%!error sortrows (1, 2)
