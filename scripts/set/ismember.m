## Copyright (C) 2000, 2005, 2006, 2007 Paul Kienzle
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
## @deftypefn {Function File} [@var{bool}, @var{index}] = ismember (@var{a}, @var{s})
## Return a matrix @var{bool} the same shape as @var{a} which has 1 if
## @code{a(i,j)} is in @var{s} or 0 if it isn't. If a second output argument
## is requested, the indexes into @var{s} of the matching elements is
## also returned.
## @seealso{unique, union, intersection, setxor, setdiff}
## @end deftypefn

## Author: Paul Kienzle
## Adapted-by: jwe

function [c, index] = ismember (a, s)

  if (nargin != 2)
    print_usage ();
  endif

  ## Convert char matrices to cell arrays.
  if (ischar (a))
    a = cellstr (a);
  endif
  if (ischar (s))
    s = cellstr (s);
  endif
  
  ## Input checking.
  if (! isa (a, class (s)))
    error ("ismember: both input arguments must be the same type");
  endif

  if (iscell (a) && ! iscellstr (a))
    error ("ismember: cell arrays may only contain strings");
  endif

  if (! isnumeric(a) && ! iscell (a))
    error ("ismember: input arguments must be arrays, cell arrays, or strings");
  endif
  
  ## Do the actual work.
  if (isempty (a) || isempty (s))
    c = zeros (size (a), "logical");
  else
    if (numel (s) == 1)
      if (iscell (a))
        c = strcmp (a, s);
      else
	## Both A and S are matrices.
        c = (a == s);
      endif
      index = double (c);
    elseif (numel (a) == 1)
      if (iscell (a))
        f = find (strcmp (a, s), 1);
      else
	## Both A and S are matrices.
        f = find (a == s, 1);
      endif
      c = ! isempty (f);
      index = f;
      if (isempty (index))
	index = 0;
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
      lt = length (s);
      [s, sidx] = sort (s);
      [v, p] = sort ([s(2:lt); a(:)]);
      idx(p) = cumsum (p <= lt-1) + 1;
      idx = idx(lt:end);
      if (iscell (a) || iscell (s))
        c = (cellfun ("length", a)
             == reshape (cellfun ("length", s(idx)), size (a)));
        idx2 = find (c);
        c(idx2) = all (char (a(idx2)) == char (s(idx)(idx2)), 2);
        index = zeros (size (c));
        index(c) = sidx(idx(c));
      else
	## Both A and S are matrices.
         c = (a == reshape (s (idx), size (a)));
        index = zeros (size (c));
        index(c) = sidx(idx(c));
      endif
    endif
  endif

endfunction

%!assert (ismember ({''}, {'abc', 'def'}), false);
%!assert (ismember ('abc', {'abc', 'def'}), true);
%!assert (isempty (ismember ([], [1, 2])), true);
%!xtest assert (ismember ('', {'abc', 'def'}), false);
%!fail ('ismember ([], {1, 2})', 'error:.*');
%!fail ('ismember ({[]}, {1, 2})', 'error:.*');
%!assert (ismember ({'foo', 'bar'}, {'foobar'}), logical ([0, 0]))
%!assert (ismember ({'foo'}, {'foobar'}), false)
%!assert (ismember ({'bar'}, {'foobar'}), false)
%!assert (ismember ({'bar'}, {'foobar', 'bar'}), true)
%!assert (ismember ({'foo', 'bar'}, {'foobar', 'bar'}), logical ([0, 1]))
%!assert (ismember ({'xfb', 'f', 'b'}, {'fb', 'b'}), logical ([0, 0, 1]))
