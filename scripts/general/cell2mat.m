## Copyright (C) 2005, 2006, 2007, 2008 Laurent Mazet
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
## @deftypefn {Function File} {@var{m} =} cell2mat (@var{c})
## Convert the cell array @var{c} into a matrix by concatenating all
## elements of @var{c} into a hyperrectangle.  Elements of @var{c} must
## be numeric, logical or char, and @code{cat} must be able to
## concatenate them together.
## @seealso{mat2cell, num2cell}
## @end deftypefn

function m = cell2mat (c)

  if (nargin != 1)
    print_usage ();
  endif

  if (! iscell (c))
    error ("cell2mat: c is not a cell array");
  endif
  
  nb = numel (c);

  ## We only want numeric, logical, and char matrices.
  valid = cellfun (@isnumeric, c);
  valid |= cellfun (@islogical, c);
  valid |= cellfun (@ischar, c);

  if (! all (valid))
    error ("cell2mat: elements must be numeric, char or logical");
  endif

  if (nb == 0)
    m = [];
  elseif (ndims (c) == 2)
    ## 2d case optimized
    [nr, nc] = size (c);
    if (nc > nr)
      c1 = cell (nr, 1);
      for i = 1 : nr
	c1{i} = [c{i,:}];
      endfor
      m = vertcat (c1 {:});
    else
      c1 = cell (nc, 1);
      for i = 1 : nc
	c1{i} = vertcat (c{:,i});
      endfor
      m = [c1{:}];
    endif
  else
    ## n dimensions case
    for k = ndims (c):-1:2,
      sz = size (c);
      sz(k) = 1;
      c1 = cell (sz);
      n1 = prod (sz);
      for i = 1:n1
        c1{i} = cat (k, c{i:n1:end});
      endfor
      c = c1;
    endfor
    m = cat (1, c1{:});
  endif

endfunction

## Tests
%!shared C, D, E, F
%! C = {[1], [2 3 4]; [5; 9], [6 7 8; 10 11 12]};
%! D = C; D(:,:,2) = C;
%! E = [1 2 3 4; 5 6 7 8; 9 10 11 12];
%! F = E; F(:,:,2) = E;
%!assert (cell2mat (C), E);
%!assert (cell2mat (D), F);
%!test
%! m = rand (10) + i * rand (10);
%! c = mat2cell (m, [1 2 3 4], [4 3 2 1]);
%! assert (cell2mat (c), m)
%!test
%! m = int8 (256*rand (4, 5, 6, 7, 8));
%! c = mat2cell (m, [1 2 1], [1 2 2], [3 1 1 1], [4 1 2], [3 1 4]);
%! assert (cell2mat (c), m)
## Demos
%!demo
%! C = {[1], [2 3 4]; [5; 9], [6 7 8; 10 11 12]};
%! cell2mat (C)
