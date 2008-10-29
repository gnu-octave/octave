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
## @deftypefn {Function File} {[@var{tf}, @var{a_idx}] =} ismember (@var{A}, @var{S}) 
## @deftypefnx {Function File} {[@var{tf}, @var{a_idx}] =} ismember (@var{A}, @var{S}, "rows")
## Return a matrix @var{tf} the same shape as @var{A} which has 1 if 
## @code{A(i,j)} is in @var{S} or 0 if it isn't. If a second output argument 
## is requested, the indexes into @var{S} of the matching elements are
## also returned. 
##
## @example
## @group
## a = [3, 10, 1];
## s = [0:9];
## [tf, a_idx] = residue (a, s);
##      @result{} tf = [1, 0, 1]
##      @result{} a_idx = [4, 0, 2]
## @end group
## @end example
##
## The inputs, @var{A} and @var{S}, may also be cell arrays.
##
## @example
## @group
## a = @{'abc'@};
## s = @{'abc', 'def'@};
## [tf, a_idx] = residue (a, s);
##      @result{} tf = [1, 0]
##      @result{} a_idx = [1, 0]
## @end group
## @end example
##
## With the optional third argument @code{"rows"}, and matrices 
## @var{A} and @var{S} with the same number of columns, compare rows in
## @var{A} with the rows in @var{S}.
##
## @example
## @group
## a = [1:3; 5:7; 4:6];
## s = [0:2; 1:3; 2:4; 3:5; 4:6];
## [tf, a_idx] = ismember(a, s, 'rows');
##      @result{} tf = logical ([1; 0; 1])
##      @result{} a_idx = [2; 0; 5];
## @end group
## @end example
##
## @seealso{unique, union, intersect, setxor, setdiff}
## @end deftypefn

## Author: Paul Kienzle <pkienzle@users.sf.net>
## Author: Søren Hauberg <hauberg@gmail.com>
## Author: Ben Abbott <bpabbott@mac.com>
## Adapted-by: jwe

function [tf, a_idx] = ismember (a, s, rows_opt) 

  if (nargin == 2 || nargin == 3) 
    if (iscell (a) || iscell (s))
      if (nargin == 3)
        error ("ismember: with 'rows' both sets must be matrices"); 
      else
        [tf, a_idx] = cell_ismember (a, s);
      endif
    else
      if (nargin == 3) 
        ## The 'rows' argument is handled in a fairly ugly way. A better
        ## solution would be to vectorize this loop over 'r' below.
        if (strcmpi (rows_opt, "rows") && ismatrix (a) && ismatrix (s)
	    && columns (a) == columns (s)) 
          rs = rows (s);
          ra = rows (a);
          a_idx = zeros (ra, 1);
          for r = 1:ra
           tmp = ones (rs, 1) * a(r,:);
            f = find (all (tmp' == s'), 1);
            if (! isempty (f))
              a_idx(r) = f;
            endif
          endfor
          tf = logical (a_idx);
        elseif (strcmpi (rows_opt, "rows"))
          error ("ismember: with 'rows' both sets must be matrices with an equal number of columns"); 
        else
          error ("ismember: invalid input"); 
        endif
      else
        ## Input checking 
        if (! isa (a, class (s))) 
          error ("ismember: both input arguments must be the same type");
        elseif (! ischar (a) && ! isnumeric (a))
          error ("ismember: input arguments must be arrays, cell arrays, or strings"); 
        elseif (ischar (a) && ischar (s))
          a = uint8 (a);
          s = uint8 (s);
        endif
        ## Convert matrices to vectors.
        if (all (size (a) > 1))
          a = a(:);
        endif 
        if (all (size (s) > 1))
          s = s(:);
        endif 
        ## Do the actual work.
        if (isempty (a) || isempty (s))
          tf = zeros (size (a), "logical");
          a_idx = zeros (size (a)); 
        elseif (numel (s) == 1) 
          tf = (a == s);
          a_idx = double (tf);
        elseif (numel (a) == 1) 
          f = find (a == s, 1); 
          tf = !isempty (f);
          a_idx = f; 
          if (isempty (a_idx))
            a_idx = 0;
          endif 
        else
          ## Magic:  the following code determines for each a, the index i 
          ## such that s(i)<= a < s(i+1).  It does this by sorting the a 
          ## into s and remembering the source index where each element came 
          ## from.  Since all the a's originally came after all the s's, if 
          ## the source index is less than the length of s, then the element 
          ## came from s.  We can then do a cumulative sum on the indices to 
          ## figure out which element of s each a comes after. 
          ## E.g., s=[2 4 6], a=[1 2 3 4 5 6 7] 
          ##    unsorted [s a]  = [ 2 4 6 1 2 3 4 5 6 7 ] 
          ##    sorted [s a]    = [ 1 2 2 3 4 4 5 6 6 7 ] 
          ##    source index p  = [ 4 1 5 6 2 7 8 3 9 10 ] 
          ##    boolean p<=l(s) = [ 0 1 0 0 1 0 0 1 0 0 ] 
          ##    cumsum(p<=l(s)) = [ 0 1 1 1 2 2 2 3 3 3 ] 
          ## Note that this leaves a(1) coming after s(0) which doesn't 
          ## exist.  So arbitrarily, we will dump all elements less than 
          ## s(1) into the interval after s(1).  We do this by dropping s(1) 
          ## from the sort!  E.g., s=[2 4 6], a=[1 2 3 4 5 6 7] 
          ##    unsorted [s(2:3) a] =[4 6 1 2 3 4 5 6 7 ] 
          ##    sorted [s(2:3) a] = [ 1 2 3 4 4 5 6 6 7 ] 
          ##    source index p    = [ 3 4 5 1 6 7 2 8 9 ] 
          ##    boolean p<=l(s)-1 = [ 0 0 0 1 0 0 1 0 0 ] 
          ##    cumsum(p<=l(s)-1) = [ 0 0 0 1 1 1 2 2 2 ] 
          ## Now we can use Octave's lvalue indexing to "invert" the sort, 
          ## and assign all these indices back to the appropriate a and s, 
          ## giving s_idx = [ -- 1 2], a_idx = [ 0 0 0 1 1 2 2 ].  Add 1 to 
          ## a_idx, and we know which interval s(i) contains a.  It is 
          ## easy to now check membership by comparing s(a_idx) == a.  This 
          ## magic works because s starts out sorted, and because sort 
          ## preserves the relative order of identical elements. 
          lt = numel(s); 
          [s, sidx] = sort (s); 
          [v, p] = sort ([s(2:lt)(:); a(:)]); 
          idx(p) = cumsum (p <= lt-1) + 1; 
          idx = idx(lt:end); 
          tf = (a == reshape (s(idx), size (a))); 
          a_idx = zeros (size (tf)); 
          a_idx(tf) = sidx(idx(tf));
        endif
        ## Resize result to the original size of 'a' 
        size_a = size (a);
        tf = reshape (tf, size_a); 
        a_idx = reshape (a_idx, size_a);
      endif
    endif
  else
    print_usage ();
  endif

endfunction

function [tf, a_idx] = cell_ismember (a, s)
  if (nargin == 2)
    if (ischar (a) && iscellstr (s)) 
      if (isempty (a)) # Work around bug in 'cellstr' 
        a = {''};
      else
        a = cellstr (a);
      endif
    elseif (iscellstr (a) && ischar (s))
      if (isempty (s)) # Work around bug in 'cellstr' 
        s = {''};
      else
        s = cellstr (s);
      endif
    endif 
    if (iscellstr (a) && iscellstr (s))
      ## Do the actual work
      if (isempty (a) || isempty (s))
        tf = zeros (size (a), "logical");
        a_idx = zeros (size (a)); 
      elseif (numel (s) == 1) 
        tf = strcmp (a, s);
        a_idx = double (tf);
      elseif (numel (a) == 1) 
        f = find (strcmp (a, s), 1); 
        tf = !isempty (f);
        a_idx = f; 
        if (isempty (a_idx))
          a_idx = 0;
        endif 
      else 
        lt = numel (s);
        [s, sidx] = sort (s);
        [v, p] = sort ([s(2:lt)(:); a(:)]);
        idx(p) = cumsum (p <= lt-1) + 1;
        idx = idx(lt:end);
        tf = (cellfun ("length", a) 
              == reshape (cellfun ("length", s(idx)), size (a)));
        idx2 = find (tf);
        tf(idx2) = (all (char (a(idx2)) == char (s(idx)(idx2)), 2));
        a_idx = zeros (size (tf));
        a_idx(tf) = sidx(idx(tf));
      endif
    else
      error ("cell_ismember: arguments must be cell arrays of character strings");
    endif
  else
    print_usage ();
  endif
  ## Resize result to the original size of 'a' 
  size_a = size (a);
  tf = reshape (tf, size_a); 
  a_idx = reshape (a_idx, size_a); 
endfunction

%!assert (ismember ({''}, {'abc', 'def'}), false);
%!assert (ismember ('abc', {'abc', 'def'}), true);
%!assert (isempty (ismember ([], [1, 2])), true);
%!assert (isempty (ismember ({}, {'a', 'b'})), true);
%!assert (ismember ('', {'abc', 'def'}), false);
%!fail ('ismember ([], {1, 2})');
%!fail ('ismember ({[]}, {1, 2})');
%!fail ('ismember ({}, {1, 2})');
%!fail ('ismember ({1}, {''1'', ''2''})');
%!fail ('ismember (1, ''abc'')');
%!fail ('ismember ({''1''}, {''1'', ''2''},''rows'')');
%!fail ('ismember ([1 2 3], [5 4 3 1], ''rows'')');
%!assert (ismember ({'foo', 'bar'}, {'foobar'}), logical ([0, 0]));
%!assert (ismember ({'foo'}, {'foobar'}), false);
%!assert (ismember ({'bar'}, {'foobar'}), false);
%!assert (ismember ({'bar'}, {'foobar', 'bar'}), true);
%!assert (ismember ({'foo', 'bar'}, {'foobar', 'bar'}), logical ([0, 1]));
%!assert (ismember ({'xfb', 'f', 'b'}, {'fb', 'b'}), logical ([0, 0, 1]));
%!assert (ismember ("1", "0123456789."), true);

%!test
%! [result, a_idx] = ismember ([1, 2], []);
%! assert (result, logical ([0, 0]))
%! assert (a_idx, [0, 0]);

%!test
%! [result, a_idx] = ismember ([], [1, 2]);
%! assert (result, logical ([]))
%! assert (a_idx, []);

%!test
%! [result, a_idx] = ismember ({'a', 'b'}, '');
%! assert (result, logical ([0, 0]))
%! assert (a_idx, [0, 0]);

%!test
%! [result, a_idx] = ismember ({'a', 'b'}, {});
%! assert (result, logical ([0, 0]))
%! assert (a_idx, [0, 0]);

%!test
%! [result, a_idx] = ismember ('', {'a', 'b'});
%! assert (result, false)
%! assert (a_idx, 0);

%!test
%! [result, a_idx] = ismember ({}, {'a', 'b'});
%! assert (result, logical ([]))
%! assert (a_idx, []);

%!test
%! [result, a_idx] = ismember([1 2 3 4 5], [3]);
%! assert (all (result == logical ([0 0 1 0 0])) && all (a_idx == [0 0 1 0 0]));

%!test
%! [result, a_idx] = ismember([1 6], [1 2 3 4 5 1 6 1]);
%! assert (all (result == logical ([1 1])) && all (a_idx == [8 7]));

%!test
%! [result, a_idx] = ismember ([3,10,1], [0,1,2,3,4,5,6,7,8,9]);
%! assert (all (result == logical ([1, 0, 1])) && all (a_idx == [4, 0, 2]));

%!test
%! [result, a_idx] = ismember ("1.1", "0123456789.1");
%! assert (all (result == logical ([1, 1, 1])) && all (a_idx == [12, 11, 12]));

%!test
%! [result, a_idx] = ismember([1:3; 5:7; 4:6], [0:2; 1:3; 2:4; 3:5; 4:6], 'rows');
%! assert (all (result == logical ([1; 0; 1])) && all (a_idx == [2; 0; 5]));

%!test
%! [result, a_idx] = ismember([1.1,1.2,1.3; 2.1,2.2,2.3; 10,11,12], [1.1,1.2,1.3; 10,11,12; 2.12,2.22,2.32], 'rows');
%! assert (all (result == logical ([1; 0; 1])) && all (a_idx == [1; 0; 2]));

