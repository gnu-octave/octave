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
## @deftypefn {Function File} {} ismember (@var{A}, @var{S})
## Return a matrix the same shape as @var{A} which has 1 if
## @code{A(i,j)} is in @var{S} or 0 if it isn't.
## @end deftypefn
## @seealso{unique, union, intersect, setxor, setdiff}

## Author: Paul Kienzle
## Adapted-by: jwe

function c = ismember (a, S)

  if (nargin != 2)
    usage ("ismember (A, S)");
  endif

  [ra, ca] = size (a);
  if (isempty (a) || isempty (S))
    c = zeros (ra, ca);
  else
    if (iscell (a) && ! iscell (S))
      tmp{1} = S;
      S = tmp;
    endif
    if (! iscell (a) && iscell (S))
      tmp{1} = a;
      a = tmp;
    endif
    S = unique (S(:));
    lt = length (S);
    if (lt == 1)
      if (iscell (a) || iscell (S))
        c = cellfun ("length", a) == cellfun ("length", S);
        idx = find (c);
        c(idx) = all (char (a(idx)) == repmat (char (S), length (idx), 1), 2);
      else
        c = (a == S);
      endif
    elseif (prod (size (a)) == 1)
      if (iscell (a) || iscell (S))
        c = cellfun ("length", a) == cellfun ("length", S);
        idx = find (c);
        c(idx) = all (repmat (char (a), length (idx), 1) == char (S(idx)), 2);
        c = any(c);
      else
        c = any (a == S);
      endif
    else
      ## Magic:  the following code determines for each a, the index i
      ## such that S(i)<= a < S(i+1).  It does this by sorting the a
      ## into S and remembering the source index where each element came
      ## from.  Since all the a's originally came after all the S's, if 
      ## the source index is less than the length of S, then the element
      ## came from S.  We can then do a cumulative sum on the indices to
      ## figure out which element of S each a comes after.
      ## E.g., S=[2 4 6], a=[1 2 3 4 5 6 7]
      ##    unsorted [S a]  = [ 2 4 6 1 2 3 4 5 6 7 ]
      ##    sorted [ S a ]  = [ 1 2 2 3 4 4 5 6 6 7 ] 
      ##    source index p  = [ 4 1 5 6 2 7 8 3 9 10 ]
      ##    boolean p<=l(S) = [ 0 1 0 0 1 0 0 1 0 0 ]
      ##    cumsum(p<=l(S)) = [ 0 1 1 1 2 2 2 3 3 3 ]
      ## Note that this leaves a(1) coming after S(0) which doesn't
      ## exist.  So arbitrarily, we will dump all elements less than
      ## S(1) into the interval after S(1).  We do this by dropping S(1)
      ## from the sort!  E.g., S=[2 4 6], a=[1 2 3 4 5 6 7]
      ##    unsorted [S(2:3) a] =[4 6 1 2 3 4 5 6 7 ]
      ##    sorted [S(2:3) a] = [ 1 2 3 4 4 5 6 6 7 ]
      ##    source index p    = [ 3 4 5 1 6 7 2 8 9 ]
      ##    boolean p<=l(S)-1 = [ 0 0 0 1 0 0 1 0 0 ]
      ##    cumsum(p<=l(S)-1) = [ 0 0 0 1 1 1 2 2 2 ]
      ## Now we can use Octave's lvalue indexing to "invert" the sort,
      ## and assign all these indices back to the appropriate A and S,
      ## giving S_idx = [ -- 1 2], a_idx = [ 0 0 0 1 1 2 2 ].  Add 1 to
      ## a_idx, and we know which interval S(i) contains a.  It is
      ## easy to now check membership by comparing S(a_idx) == a.  This
      ## magic works because S starts out sorted, and because sort
      ## preserves the relative order of identical elements.
      [v, p] = sort ([S(2:lt); a(:)]);
      idx(p) = cumsum (p <= lt-1) + 1;
      idx = idx(lt:lt+ra*ca-1);
      if (iscell (a) || iscell (S))
        c = (cellfun ("length", a)
	     == reshape (cellfun ("length", S(idx)), size (a)));
        idx2 = find (c);
        c(idx2) = all (char (a(idx2)) == char (S(idx)(idx2)), 2);
      else
        c = (a == reshape (S (idx), size (a)));
      endif
    endif
  endif

endfunction
  