## Copyright (C) 2017 Michael Leitner
## Copyright (C) 2001-2017 Paul Kienzle
## Copyright (C) 2009 VZLU Prague
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or
## (at your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn {} {} perms (@var{v})
## Generate all permutations of vector @var{v} with one row per permutation.
##
## Results are returned in inverse lexicographic order.  The result has size
## @code{factorial (@var{n}) * @var{n}}, where @var{n} is the length of
## @var{v}.  Any repetitions are included in the output.  To generate just the
## unique permutations use @code{unique (perms (@var{v}), "rows")(end:-1:1,:)}.
##
## Example
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
## Programming Note: The maximum length of @var{v} should be less than or
## equal to 10 to limit memory consumption.
## @seealso{permute, randperm, nchoosek}
## @end deftypefn

## FIXME: In principle it should be more efficient to do indexing using uint8
## type.  However, benchmarking shows doubles are faster.  If this changes in
## a later version of Octave the index variables here can be made uint8.

function A = perms (v)

  if (nargin != 1)
    print_usage ();
  endif

  if (! (isreal (v) || iscomplex (v)))
    error ("perms: V must be a numeric, char, or logical vector");
  endif

  v = sort (reshape (v, 1, []), "descend");
  n = length (v);

  if (n < 4)    # special cases for small n
    switch (n)
      case 0
        A = [];
      case 1
        A = v;
      case 2
        A = [v;v([2 1])];
      case 3
        A = v([1, 2, 3; 1, 3, 2; 2, 1, 3; 2, 3, 1; 3, 1, 2; 3, 2, 1]);
    endswitch
  else
    n-= 1;

    idx = zeros (factorial (n), n);
    idx(1:6, n-2:n) = [1, 2, 3;1, 3, 2;2, 1, 3;2, 3, 1;3, 1, 2;3, 2, 1]+(n-3);
    f = 2;    # jump-start for efficiency with medium n
    for j = 3:n-1
      b = 1:n;
      f *= j;
      perm = idx(1:f, n-(j-1):n);
      idx(1:(j+1)*f, n-j) = (n-j:n)(ones (f, 1),:)(:);
      for i=0:j
        b(i+n-j) -= 1;
        idx((1:f)+i*f, n-(j-1):n) = b(perm);
      endfor
    endfor

    n += 1;
    f *= n-1;
    A = v(1)(ones (factorial (n), n));
    A(:,1) = v(ones (f, 1),:)(:);

    for i = 1:n
      b = v([1:i-1 i+1:n]);
      A((1:f)+(i-1)*f, 2:end) = b(idx);
    end
  endif

endfunction


%!assert (rows (perms (1:6)), factorial (6))
%!assert (perms ([]), [])
%!assert (perms (pi), pi)
%!assert (perms ([pi, e]), [pi, e; e, pi])
%!assert (perms ([1,2,3]), [3,2,1;3,1,2;2,3,1;2,1,3;1,3,2;1,2,3])
%!assert (perms (1:5), perms ([2 5 4 1 3]'))
%!assert (perms ("abc"), char ("cba", "cab", "bca", "bac", "acb", "abc"))
%!assert (perms ("fobar"), sortrows (unique (perms ("fobar"), "rows"), -(1:5)))
%!assert (unique (perms (1:5)(:))', 1:5)
%!assert (perms (int8 (1:4)), int8 (perms (1:4)))

%!error perms ()
%!error perms (1, 2)
%!error <V must be a numeric> perms ({1})
