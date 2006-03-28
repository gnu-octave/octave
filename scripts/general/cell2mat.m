## Copyright (C) 2005 Laurent Mazet
##
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2 of the License, or
## (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, write to the Free
## Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301, USA.

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
    usage ("m = cell2mat (c)");
  endif

  if (! iscell (c))
    error ("cell2mat: c is not a cell array");
  endif
  
  nb = numel (c);

  if (nb == 0)
    m = [];
  elseif (nb == 1)
    elt = c{1};
    if (isnumeric (elt) || ischar (elt) || islogical (elt))
      m = elt;
    elseif (iscell (elt))
      m = cell2mat (elt);
    else
      error ("cell2mat: all elements of cell array must be numeric, logical or char");
    endif
  else
    ## n dimensions case
    for k = ndims (c):-1:2,
      sz = size (c);
      sz(end) = 1;
      c1 = cell (sz);
      for i = 1:(prod (sz))
        c1{i} = cat (k, c{i:(prod (sz)):end});
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
## Demos
%!demo
%! C = {[1], [2 3 4]; [5; 9], [6 7 8; 10 11 12]};
%! cell2mat (C)
