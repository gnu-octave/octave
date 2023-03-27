########################################################################
##
## Copyright (C) 2001-2023 The Octave Project Developers
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
## @deftypefn  {} {@var{P} =} perms (@var{v})
## @deftypefnx {} {@var{P} =} perms (@var{v}, "unique")
## Generate all permutations of vector @var{v} with one row per permutation.
##
## Results are returned in inverse lexicographic order.  The result has size
## @code{factorial (@var{n}) * @var{n}}, where @var{n} is the length of
## @var{v}.  Any repeated elements are included in the output.
##
## If the optional argument @qcode{"unique"} is given then only unique
## permutations are returned, using less memory and generally taking less time
## than calling @code{unique (perms (@var{v}), "rows")}.
##
## Example 1
##
## @example
## @group
## perms ([1, 2, 3])
## @result{}
##   3   2   1
##   3   1   2
##   2   3   1
##   2   1   3
##   1   3   2
##   1   2   3
## @end group
## @end example
##
## Example 2
##
## @example
## @group
## perms ([1, 1, 2, 2], "unique")
## @result{}
##   2   2   1   1
##   2   1   2   1
##   2   1   1   2
##   1   2   2   1
##   1   2   1   2
##   1   1   2   2
## @end group
## @end example
##
## Programming Note: If the @qcode{"unique"} option is not used, the length of
## @var{v} should be no more than 10-12 to limit memory consumption.  Even with
## @qcode{"unique"}, there should be no more than 10-12 unique elements in
## @var{v}.
## @seealso{permute, randperm, nchoosek}
## @end deftypefn

## FIXME: In principle it should be more efficient to do indexing using uint8
## type.  However, benchmarking shows doubles are faster.  If this changes in
## a later version of Octave the index variables here can be made uint8.

function P = perms (v, opt)

  if (nargin < 1)
    print_usage ();
  endif

  unique_v = false;
  if (nargin == 2)
    if (! strcmpi (opt, "unique"))
      error ('perms: option must be the string "unique"');
    endif
    unique_v = true;
  endif

  v = v(:).';  # convert to row vector
  if (isnumeric (v) || ischar (v))
    ## Order of output is only dependent on the actual values for
    ## character and numeric arrays.
    v = sort (v, "ascend");
  endif

  n = numel (v);
  if (n < 4)    # special cases for small n
    switch (n)
      case 0
        P = reshape (v, 1, 0);
      case 1
        P = v;
      case 2
        P = [v([2 1]);v];
      case 3
        P = v([3 2 1; 3 1 2; 2 3 1; 2 1 3; 1 3 2; 1 2 3]);
    endswitch
    if (unique_v)
      P = unique (P, "rows");
    endif

  elseif (! unique_v)
    ## FIXME: Brief explanation of the algorithm being used would be useful.
    v = v(end:-1:1);
    n-= 1;

    idx = zeros (factorial (n), n);
    idx(1:6, n-2:n) = [1, 2, 3;1, 3, 2;2, 1, 3;2, 3, 1;3, 1, 2;3, 2, 1]+(n-3);
    f = 2;    # jump-start for efficiency with medium n
    for j = 3:n-1
      b = 1:n;
      f *= j;
      perm = idx(1:f, n-(j-1):n);
      idx(1:(j+1)*f, n-j) = (n-j:n)(ones (f, 1),:)(:);
      for i = 0:j
        b(i+n-j) -= 1;
        idx((1:f)+i*f, n-(j-1):n) = b(perm);
      endfor
    endfor

    n += 1;
    f *= n-1;
    P = v(1)(ones (factorial (n), n));
    P(:,1) = v(ones (f, 1),:)(:);

    for i = 1:n
      b = v([1:i-1 i+1:n]);
      P((1:f)+(i-1)*f, 2:end) = b(idx);
    endfor

  else  # unique permutations
    [v, ~, j] = unique (v);
    h = accumarray (j, 1)';
    idx = m_perms (h);
    P = v(sortrows (idx', -(1:rows(idx))));
  endif

endfunction

function out_perms = m_perms (multis)
  ## FIXME: Brief explanation of the algorithm being used would be useful.

  l = numel (multis);
  if (l == 1)
    out_perms = uint8 (ones (multis, 1));
  else
    p1 = m_perms (multis (1:floor (l/2)));
    p2 = m_perms (multis (floor (l/2+1):l)) + max (p1(:, 1));
    l1 = rows (p1);
    l2 = rows (p2);
    cp1 = columns (p1);
    cp2 = columns (p2);

    p = nchoosek (1:l1+l2, l1);
    rp = rows (p);

    ii = false (l1+l2, rp);
    ii(p + (0:rp - 1)' * (l1 + l2)) = true;
    out_perms = zeros (l1 + l2, cp1 * cp2 * rp, "uint8");
    out_perms(repmat ( ii, cp1 * cp2, 1)(:)) = repmat (p1, cp2, rp)(:);
    out_perms(repmat (!ii, cp1 * cp2, 1)(:)) = repmat (p2, 1, cp1 * rp)(:);
  endif

endfunction


%!assert (rows (perms (1:6)), factorial (6))
%!assert (perms (pi), pi)
%!assert (perms ([pi, e]), [pi, e; e, pi])
%!assert (perms ([1,2,3]), [3,2,1;3,1,2;2,3,1;2,1,3;1,3,2;1,2,3])
%!assert (perms (1:5), perms ([2 5 4 1 3]'))
%!assert (perms ("abc"), char ("cba", "cab", "bca", "bac", "acb", "abc"))
%!assert (perms ("fobar"), sortrows (unique (perms ("fobar"), "rows"), -(1:5)))
%!assert (unique (perms (1:5)(:))', 1:5)
%!assert (perms (int8 (1:4)), int8 (perms (1:4)))

%!assert (sortrows (perms ("abb", "unique")), ["abb"; "bab"; "bba"])
%!assert (size (perms ([1 1 1 1 2 2 2 3 3], "unique")), [1260 9])
%!assert (size (perms (int8([1 1 1 1 1 2 2 2 2 3 3 3]), "unique")), [27720 12])

## Should work for any array type, such as cells and structs,
## and not only for numeric data.

%!assert <*52431> (perms ({1}), {1})
%!assert <*52431> (perms ({0.1, "foo"}), {"foo", 0.1; 0.1, "foo"})
%!assert <*52431> (perms ({"foo", 0.1}), {0.1, "foo"; "foo", 0.1})
%!assert <*52431> (perms ({"foo"; 0.1}), {0.1, "foo"; "foo", 0.1})
%!assert <*52431> (perms ({0.1; "foo"}), {"foo", 0.1; 0.1, "foo"})
%!assert <*52431> (perms ({"foo", "bar"}), {"bar", "foo"; "foo", "bar"})
%!assert <*52431> (perms ({"bar", "foo"}), {"foo", "bar"; "bar", "foo"})
%!
%!assert <*52431> (perms (struct ()), struct ())
%!assert <*52431> (perms (struct ("foo", {1, 2})),
%!                struct ("foo", {2, 1; 1, 2}))
%!assert <*52431> (perms (struct ("foo", {1, 2}, "bar", {3, 4})),
%!                struct ("foo", {2, 1; 1, 2}, "bar", {4, 3; 3, 4}))

## Also sort logical input with order dependent on the input order and
## not their values.

%!assert <*52431> (perms (logical ([1 0])), logical ([0 1;, 1 0]))
%!assert <*52431> (perms (logical ([0 1])), logical ([1 0; 0 1]))
%!assert <*52431> (perms (logical ([0 1 0])),
%!                logical ([0 1 0; 0 0 1; 1 0 0; 1 0 0; 0 0 1; 0 1 0]))
%!assert <*52431> (perms (logical ([0 1 1])),
%!                logical ([1 1 0; 1 0 1; 1 1 0; 1 0 1; 0 1 1; 0 1 1]))

%!assert <*52432> (perms ([]), reshape ([], 1, 0))
%!assert <*52432> (perms (single ([])), reshape (single ([]), 1, 0))
%!assert <*52432> (perms (int8 ([])), reshape (int8 ([]), 1, 0))
%!assert <*52432> (perms ({}), cell (1, 0))

%!test <*52432>
%! s = struct ();
%! s(1) = [];
%! assert (perms (reshape (s, 0, 0)), reshape (s, 1, 0));
%! assert (perms (reshape (s, 0, 1)), reshape (s, 1, 0));

## Test input validation
%!error <Invalid call> perms ()
%!error <option must be the string "unique"> perms (1:5, "foobar")
%!error <option must be the string "unique"> perms (1:5, {"foo"})

