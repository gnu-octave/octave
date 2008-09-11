## Copyright (C) 2005, 2006, 2007 Laurent Mazet
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
  elseif (ndims (c) == 2)
    nr = rows (c);
    c1 = cell (nr, 1);
    for i = 1 : nr
      c1{i} = [c{i : nr : end}];
    endfor
    ## This is faster than "c = cat(1, c{:})"
    m = [cellfun(@(x) x.', c1, "UniformOutput", false){:}].';
  else
   nd = ndims (c);
   for k = nd : -1 : 2
      sz = size (c);
      if (k > ndims (c) || sz(end) == 1)
	continue;
      endif
      sz(end) = 1;
      c1 = cell (sz);
      sz = prod (sz);
      if (k == 2)
        for i = 1 : sz
	  c1{i} = [c{i : sz : end}];
        endfor
      else
        ## This is faster than
        ##   for i = 1:sz, c1{i} = cat (k, c{i:(prod (sz)):end}); endfor
	idx = [1, k, (3 : (k - 1)), 2, ((k + 1): nd)];
        c = cellfun(@(x) permute (x, idx), c, "UniformOutput", false);
        for i = 1: sz
	  c1{i} = ipermute ([c{i : sz : end}], idx);
        endfor
      endif
      c = c1;
    endfor
    if (numel (c) > 1)
      idx = [2, 1, 3 : nd];
      m = ipermute([cellfun(@(x) permute (x, idx), c, "UniformOutput", false){:}], idx);
    else
      m = c{1};
    endif
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
