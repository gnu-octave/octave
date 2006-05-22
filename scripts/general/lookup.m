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
## Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301, USA.

## -*- texinfo -*-
## @deftypefn {Function File} {@var{idx} =} lookup (@var{table}, @var{y})
## Lookup values in a sorted table.  Usually used as a prelude to
## interpolation.
##
## If table is strictly increasing and @code{idx = lookup (table, y)}, then
## @code{table(idx(i)) <= y(i) < table(idx(i+1))} for all @code{y(i)}
## within the table.  If @code{y(i)} is before the table, then 
## @code{idx(i)} is 0. If @code{y(i)} is after the table then
## @code{idx(i)} is @code{table(n)}.
##
## If the table is strictly decreasing, then the tests are reversed.
## There are no guarantees for tables which are non-monotonic or are not
## strictly monotonic.
##
## To get an index value which lies within an interval of the table,
## use:
##
## @example
## idx = lookup (table(2:length(table)-1), y) + 1
## @end example
##
## @noindent
## This expression puts values before the table into the first
## interval, and values after the table into the last interval.
## @end deftypefn

## Changed from binary search to sort.
## Thanks to Kai Habel <kai.habel@gmx.de> for the suggestion.

## TODO: sort-based lookup is significantly slower given a large table
## TODO: and small lookup vector.  This shouldn't be a problem since
## TODO: interpolation (the reason for the table lookup in the first
## TODO: place) usually involves subsampling of an existing table.  The
## TODO: other use of interpolation, looking up values one at a time, is
## TODO: unfortunately significantly slower for large tables.  
## TODO:    sort is order O((lt+lx)*log(lt+lx)) 
## TODO:    search is order O(lx*log(lt))
## TODO: Clearly, search is asymptotically better than sort, but sort
## TODO: is compiled and search is not.  Could support both, or recode
## TODO: search in C++, or assume things are good enough as they stand.

function idx = lookup (table, xi)
  if (nargin == 2)
    if (isempty (table))
      idx = zeros (size (xi));
    elseif (isvector (table))
      [nr, nc] = size (xi);
      lt = length (table);
      if (table(1) > table(lt))
	## decreasing table
	[v, p] = sort ([xi(:); table(:)]);
	idx(p) = cumsum (p > nr*nc);
	idx = lt - idx(1:nr*nc);
      else
	## increasing table
	[v, p] = sort ([table(:); xi(:) ]);
	idx(p) = cumsum (p <= lt);
	idx = idx(lt+1:lt+nr*nc);
      endif
      idx = reshape (idx, nr, nc);
    else
      error ("lookup: table must be a vector");
    endif
  else
    print_usage ();
  endif
endfunction
  
%!assert (lookup(1:3, 0.5), 0)     # value before table
%!assert (lookup(1:3, 3.5), 3)     # value after table error
%!assert (lookup(1:3, 1.5), 1)     # value within table error
%!assert (lookup(1:3, [3,2,1]), [3,2,1])
%!assert (lookup([1:4]', [1.2, 3.5]'), [1, 3]');
%!assert (lookup([1:4], [1.2, 3.5]'), [1, 3]');
%!assert (lookup([1:4]', [1.2, 3.5]), [1, 3]);
%!assert (lookup([1:4], [1.2, 3.5]), [1, 3]);
%!assert (lookup(1:3, [3, 2, 1]), [3, 2, 1]);
%!assert (lookup([3:-1:1], [3.5, 3, 1.2, 2.5, 2.5]), [0, 1, 2, 1, 1])
%!assert (isempty(lookup([1:3], [])))
%!assert (isempty(lookup([1:3]', [])))
%!assert (lookup(1:3, [1, 2; 3, 0.5]), [1, 2; 3, 0]);
