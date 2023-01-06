########################################################################
##
## Copyright (C) 2015-2023 The Octave Project Developers
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
## @deftypefn  {} {@var{xxx} =} repelem (@var{x}, @var{R})
## @deftypefnx {} {@var{xxx} =} repelem (@var{x}, @var{R_1}, @dots{}, @var{R_n})
## Construct an array of repeated elements from @var{x} and repeat
## instructions @var{R_1}, @dots{}.
##
## @var{x} must be a scalar, vector, or N-dimensional array.
##
## A repeat instruction @var{R_j} must either be a scalar or a vector.  If the
## instruction is a scalar then each component of @var{x} in dimension @var{j}
## is repeated @var{R_j} times.  If the instruction is a vector then it must
## have the same number of elements as the corresponding dimension @var{j} of
## @var{x}.  In this case, the @var{k}th component of dimension @var{j} is
## repeated @code{@var{R_j}(@var{k})} times.
##
## If @var{x} is a scalar or vector then @code{repelem} may be called with just
## a single repeat instruction @var{R} and @code{repelem} will return a vector
## with the same orientation as the input.
##
## If @var{x} is a matrix then at least two @var{R_j}s must be specified.
##
## Note: Using @code{repelem} with a vector @var{x} and a vector for @var{R_j}
## is equivalent to Run Length Decoding.
##
## Examples:
##
## @example
## @group
## A = [1 2 3 4 5];
## B = [2 1 0 1 2];
## repelem (A, B)
##   @result{}   1   1   2   4   5   5
## @end group
## @end example
##
## @example
## @group
## A = magic (3)
##   @result{} A =
##        8   1   6
##        3   5   7
##        4   9   2
## B1 = [1 2 3];
## B2 = 2;
## repelem (A, B1, B2)
##   @result{}     8   8   1   1   6   6
##          3   3   5   5   7   7
##          3   3   5   5   7   7
##          4   4   9   9   2   2
##          4   4   9   9   2   2
##          4   4   9   9   2   2
## @end group
## @end example
##
## More @var{R_j} may be specified than the number of dimensions of @var{x}.
## Any excess @var{R_j} must be scalars (because @var{x}'s size in those
## dimensions is only 1), and @var{x} will be replicated in those dimensions
## accordingly.
##
## @example
## @group
## A = [1 2 3 4 5];
## B1 = 2;
## B2 = [2 1 3 0 2];
## B3 = 3;
## repelem (A, B1, B2, B3)
##   @result{}    ans(:,:,1) =
##            1   1   2   3   3   3   5   5
##            1   1   2   3   3   3   5   5
##
##         ans(:,:,2) =
##
##            1   1   2   3   3   3   5   5
##            1   1   2   3   3   3   5   5
##
##         ans(:,:,3) =
##            1   1   2   3   3   3   5   5
##            1   1   2   3   3   3   5   5
## @end group
## @end example
##
## @var{R_j} must be specified in order.  A placeholder of 1 may be used for
## dimensions which do not need replication.
##
## @example
## @group
## repelem ([-1, 0; 0, 1], 1, 2, 1, 2)
##   @result{}  ans(:,:,1,1) =
##         -1  -1   0   0
##          0   0   1   1
##
##       ans(:,:,1,2) =
##         -1  -1   0   0
##          0   0   1   1
## @end group
## @end example
##
## If fewer @var{R_j} are given than the number of dimensions in @var{x},
## @code{repelem} will assume @var{R_j} is 1 for those dimensions.
##
## @example
## A = cat (3, [-1 0; 0 1], [-1 0; 0 1])
##   @result{}  ans(:,:,1) =
##         -1   0
##          0   1
##
##       ans(:,:,2) =
##         -1   0
##          0   1
##
## repelem (A,2,3)
##   @result{}  ans(:,:,1) =
##         -1  -1  -1   0   0   0
##         -1  -1  -1   0   0   0
##          0   0   0   1   1   1
##          0   0   0   1   1   1
##
##       ans(:,:,2) =
##         -1  -1  -1   0   0   0
##         -1  -1  -1   0   0   0
##          0   0   0   1   1   1
##          0   0   0   1   1   1
## @end example
##
## @code{repelem} preserves the class of @var{x}, and works with strings,
## cell arrays, NA, and NAN inputs.  If any @var{R_j} is 0 the output will
## be an empty array.
##
## @example
## @group
## repelem ("Octave", 2, 3)
##   @result{}    OOOccctttaaavvveee
##         OOOccctttaaavvveee
##
## repelem ([1 2 3; 1 2 3], 2, 0)
##   @result{}    [](4x0)
## @end group
## @end example
##
## @seealso{cat, kron, repmat}
## @end deftypefn

## Author: Markus Bergholz <markuman@gmail.com>
## Author: Nicholas R. Jankowski <jankowskin@asme.org>

## As a U.S. government employee, Nicholas R. Jankowski makes no claim
## of copyright.

## The prepareIdx routine is Copyright (C) 2015 Peter John Acklam
## <pjacklam@gmail.com>, used with permission.

function retval = repelem (x, varargin)

  if (nargin < 2)
    print_usage ();

  elseif (nargin == 2)

    R = varargin{1};

    if (isscalar (R))

      if (! isvector (x))
        error (["repelem: %dD Array requires %d or more input " ...
                "arguments, but only %d given"], ...
               ndims (x), ndims (x) + 1, nargin);
      endif

      if (isrow (x))
        ## element values repeated R times in a scalar or row vector
        retval = x(ones (R, 1), :)(:).';
      else
        ## element values repeated R times in a col vector
        retval = x.'(ones (R, 1), :)(:);
      endif

    elseif (isvector (x) && isvector (R))

      ## vector x with vector repeat.
      if (numel (R) != numel (x))
        error (["repelem: R1 must either be scalar or have the same " ...
                "number of elements as the vector to be replicated"]);
      endif

      ## Basic run-length decoding in function prepareIdx returns
      ## idx2 as a row vector of element indices in the right positions.
      idx2 = prepareIdx (R);
      ## Fill with element values, direction matches element.
      retval = x(idx2);

    else # catch any arrays passed to x or varargin with nargin==2
      error (["repelem: when called with only two inputs they must be " ...
              "either scalars or vectors, not %s and %s."],
             typeinfo (x), typeinfo (R));
    endif

  elseif (nargin == 3)  # special optimized case for 2-D (matrices)

    ## Input Validation
    xsz = size (x);
    vector_r = ! (cellfun (@numel, varargin) == 1);

    ## 1. Check that all varargin are either scalars or vectors, not arrays.
    ##    isvector returns true for scalars so one test captures both inputs.
    if (! (isvector (varargin{1}) && (isvector (varargin{2}))))
      error ("repelem: R1 and R2 must be scalars or vectors");

    ## 2. check that any repeat vectors have the right length.
    elseif (any (cellfun (@numel, varargin(vector_r)) != xsz(vector_r)))
      error (["repelem: R_j vectors must have the same number of elements " ...
              "as the size of dimension j of X"]);
    endif

    ## Create index arrays to pass to element.
    ## (It is no slower to call prepareIdx than to check and do scalars
    ## directly.)
    idx1 = prepareIdx (varargin{1}, xsz(1));
    idx2 = prepareIdx (varargin{2}, xsz(2));

    if (issparse (x))
      retval = x(idx1, idx2);
    else
      ## The ":" at the end takes care of any x dimensions > 2.
      retval = x(idx1, idx2, :);
    endif

  else  # (nargin > 3)

    ## Input Validation
    xsz = size (x);
    n_xdims = numel (xsz);
    vector_r = ! (cellfun (@numel, varargin) == 1);

    ## 1. Check that all repeats are scalars or vectors
    ##    (isvector gives true for scalars);
    if (! all (cellfun (@isvector, varargin(vector_r))))
      error ("repelem: R_j must all be scalars or vectors");

    ## 2. Catch any vectors thrown at trailing singletons,
    ##    which should only have scalars;
    elseif (find (vector_r, 1, "last") > n_xdims)
      error ("repelem: R_j for trailing singleton dimensions must be scalar");

    ## 3. Check that the ones that are vectors have the right length.
    elseif (any (cellfun (@numel, varargin(vector_r)) != xsz(vector_r)))
      error (["repelem: R_j vectors must have the same number of elements " ...
              "as the size of dimension j of X"]);

    endif

    n_rpts = nargin - 1;
    dims_with_vectors_and_scalars = min (n_xdims, n_rpts);

    ## Preallocate idx which will contain index array to be put into element.
    idx = cell (1, n_rpts);

    ## Use prepareIdx() to fill indices for dimensions that could be
    ## a scalar or a vector.
    for i = 1 : dims_with_vectors_and_scalars
      idx(i) = prepareIdx (varargin{i}, xsz(i));
    endfor

    ## If there are more varargin inputs than x dimensions, then input tests
    ## have verified that they are just scalars, so add [1 1 1 1 1 ... 1] to
    ## those dims to perform concatenation along those dims.
    if (n_rpts > n_xdims)
      for i = n_xdims + (1 : (n_rpts - n_xdims))
        idx(i) = ones (1, varargin{i});
      endfor
    endif

    ## Use completed idx to specify repetition of x values in all dimensions.
    ## The trailing ":" will take care of cases where n_xdims > n_rpts.
    retval = x(idx{:}, :);

  endif

endfunction

## Return a row vector of indices prepared for replicating.
function idx = prepareIdx (v, n)

  if (isscalar (v))
    ## will always return row vector
    idx = [1:n](ones (v, 1), :)(:).';

  else
    ## This works for a row or column vector.

    ## Get ending position for each element item.
    idx_temp = cumsum (v);

    ## Set starting position of each element to 1.
    idx(idx_temp + 1) = 1;

    ## Set starting position of each element to 1.
    idx(1) = 1;

    ## Row vector with proper length for output
    idx = idx(1:idx_temp(end));

    ## with prepared index
    idx = (find (v != 0))(cumsum (idx));

  endif

endfunction


## tests for help examples
%!assert (repelem ([1 2 3 4 5], [2 1 0 1 2]), [1 1 2 4 5 5])
%!assert (repelem (magic(3), [1 2 3],2), ...
%!  [8 8 1 1 6 6;3 3 5 5 7 7;3 3 5 5 7 7;4 4 9 9 2 2;4 4 9 9 2 2;4 4 9 9 2 2])
%!assert (repelem ([1 2 3 4 5],2,[2 1 3 0 2],3),repmat([1 1 2 3 3 3 5 5],2,1,3))
%!assert (repelem ([-1 0;0 1],1,2,1,2), repmat([-1 -1 0 0; 0 0 1 1],1,1,1,2))
%!assert (repelem (cat(3,[-1 0 ; 0 1],[-1 0 ; 0 1]),2,3), ...
%!  repmat([-1 -1 -1 0 0 0;-1 -1 -1 0 0 0;0 0 0 1 1 1;0 0 0 1 1 1],1,1,2))
%!assert (repelem ("Octave", 2,3), ["OOOccctttaaavvveee";"OOOccctttaaavvveee"])

## test complex vectors are not Hermitian conjugated
%!assert (repelem ([i, -i], 2), [i, i, -i, -i])
%!assert (repelem ([i; -i], 2), [i; i; -i; -i])

## nargin == 2 tests
%!assert (repelem (2, 6), [2 2 2 2 2 2])
%!assert (repelem ([-1 0 1], 2), [-1 -1 0 0 1 1])
%!assert (repelem ([-1 0 1]', 2), [-1; -1; 0; 0; 1; 1])
%!assert (repelem ([-1 0 1], [1 2 1]), [-1 0 0 1])
%!assert (repelem ([-1 0 1]', [1 2 1]), [-1; 0; 0; 1])
%!assert (repelem ([1 2 3 4 5]', [2 1 0 1 2]), [1 1 2 4 5 5]')

## nargin == 3 tests
%!assert (repelem ([1 0;0 -1], 2, 3),
%!       [1 1 1 0 0 0;1 1 1 0 0 0;0 0 0 -1 -1 -1;0 0 0 -1 -1 -1])
%!assert (repelem ([1 0; 0 -1], 1,[3 2]), [1 1 1 0 0;0 0 0 -1 -1])
%!assert (repelem ([1 0; 0 -1], 2,[3 2]),
%!        [1 1 1 0 0;1 1 1 0 0;0 0 0 -1 -1;0 0 0 -1 -1])
%!assert (repelem (cat(3,[1 0; 0 -1],[1 0;0 -1]), 1,[3 2]),
%!        repmat([1 1 1 0 0 ; 0 0 0 -1 -1],1,1,2))
%!assert (repelem ([1 0; 0 -1], [3 2], 1), [1 0;1 0;1 0;0 -1;0 -1])
%!assert (repelem ([1 0; 0 -1], [3 2], 2),
%!        [1 1 0 0;1 1 0 0;1 1 0 0;0 0 -1 -1;0 0 -1 -1])
%!assert (repelem ([1 0; 0 -1], [2 3] ,[3 2]),
%!        [1 1 1 0 0;1 1 1 0 0;0 0 0 -1 -1;0 0 0 -1 -1;0 0 0 -1 -1])
%!assert (repelem (cat(3,[1 1 1 0;0 1 0 0],[1 1 1 1;0 0 0 1],[1 0 0 1;1 1 0 1]),
%!                2, 3),
%!        cat (3,[1 1 1 1 1 1 1 1 1 0 0 0
%!                1 1 1 1 1 1 1 1 1 0 0 0
%!                0 0 0 1 1 1 0 0 0 0 0 0
%!                0 0 0 1 1 1 0 0 0 0 0 0],
%!               [1 1 1 1 1 1 1 1 1 1 1 1
%!                1 1 1 1 1 1 1 1 1 1 1 1
%!                0 0 0 0 0 0 0 0 0 1 1 1
%!                0 0 0 0 0 0 0 0 0 1 1 1],
%!               [1 1 1 0 0 0 0 0 0 1 1 1
%!                1 1 1 0 0 0 0 0 0 1 1 1
%!                1 1 1 1 1 1 0 0 0 1 1 1
%!                1 1 1 1 1 1 0 0 0 1 1 1]))
%!assert (repelem (cat(3,[1 1 1 0;0 1 0 0],[1 1 1 1;0 0 0 1],[1 0 0 1;1 1 0 1]),
%!                2, [3 3 3 3]), ...
%!        cat (3,[1 1 1 1 1 1 1 1 1 0 0 0
%!                1 1 1 1 1 1 1 1 1 0 0 0
%!                0 0 0 1 1 1 0 0 0 0 0 0
%!                0 0 0 1 1 1 0 0 0 0 0 0], ...
%!               [1 1 1 1 1 1 1 1 1 1 1 1
%!                1 1 1 1 1 1 1 1 1 1 1 1
%!                0 0 0 0 0 0 0 0 0 1 1 1
%!                0 0 0 0 0 0 0 0 0 1 1 1], ...
%!               [1 1 1 0 0 0 0 0 0 1 1 1
%!                1 1 1 0 0 0 0 0 0 1 1 1
%!                1 1 1 1 1 1 0 0 0 1 1 1
%!                1 1 1 1 1 1 0 0 0 1 1 1]));
%!assert (repelem ([1 2 3 4 5], 2,[2 1 2 0 2]), [1 1 2 3 3 5 5;1 1 2 3 3 5 5])
%
## nargin > 3 tests
%!assert (repelem ([1 0;0 -1], 2, 3, 4), ...
%!        cat(3,[1 1 1 0 0 0;1 1 1 0 0 0;0 0 0 -1 -1 -1;0 0 0 -1 -1 -1], ...
%!              [1 1 1 0 0 0;1 1 1 0 0 0;0 0 0 -1 -1 -1;0 0 0 -1 -1 -1], ...
%!              [1 1 1 0 0 0;1 1 1 0 0 0;0 0 0 -1 -1 -1;0 0 0 -1 -1 -1], ...
%!              [1 1 1 0 0 0;1 1 1 0 0 0;0 0 0 -1 -1 -1;0 0 0 -1 -1 -1]))
%!assert (repelem (repmat([-1 0;0 1],1,1,2,3),2,2,2), ...
%!        repmat([-1 -1 0 0;-1 -1 0 0;0 0 1 1; 0 0 1 1],1,1,4,3))
%!assert (repelem (repmat([-1 0;0 1],1,1,2,3),[2 2],[2 2],2), ...
%!        repmat([-1 -1 0 0;-1 -1 0 0;0 0 1 1; 0 0 1 1],1,1,4,3))
%!assert (repelem (repmat([-1 0;0 1],1,1,2,3),2,2,2,2,2), ...
%!        repmat([-1 -1 0 0;-1 -1 0 0;0 0 1 1; 0 0 1 1],1,1,4,6,2))
%!assert (repelem ([1,0,-1;-1,0,1],[2 3],[2 3 4],2), ...
%!        cat (3,[ 1  1 0 0 0 -1 -1 -1 -1
%!                 1  1 0 0 0 -1 -1 -1 -1
%!                -1 -1 0 0 0  1  1  1  1
%!                -1 -1 0 0 0  1  1  1  1
%!                -1 -1 0 0 0  1  1  1  1], ...
%!               [ 1  1 0 0 0 -1 -1 -1 -1
%!                 1  1 0 0 0 -1 -1 -1 -1
%!                -1 -1 0 0 0  1  1  1  1
%!                -1 -1 0 0 0  1  1  1  1
%!                -1 -1 0 0 0  1  1  1  1]));
%!assert (repelem ([1 2 3;4 5 6],[0 2],2,2), repmat([4 4 5 5 6 6],2,1,2))

## test with structures
%!test
%! a(2).x = 1;
%! a(2).y = 2;
%! a(1).x = 3;
%! a(1).y = 4;
%! b = repelem (a, 2, [1 3]);
%! assert (size (b) == [2, 4]);
%! assert ([b.y], [4 4 2 2 2 2 2 2]);

## test with cell arrays
%!test
%! assert (repelem ({-1 0 1},  2), {-1 -1 0 0 1 1});
%! assert (repelem ({-1 0 1}', 2), {-1; -1; 0; 0; 1; 1;});
%! assert (repelem ({1 0;0 -1}, 2, 3),
%!         {1 1 1 0 0 0;1 1 1 0 0 0;0 0 0 -1 -1 -1;0 0 0 -1 -1 -1});

%!test <*54275>
%! assert (repelem (11:13, [1 3 0]), [11 12 12 12]);

%!test <*59705>
%! xs = sparse (magic (3));
%! assert (repelem (xs, 1, 2), ...
%!         sparse ([8,8,1,1,6,6; 3,3,5,5,7,7; 4,4,9,9,2,2]));

## nargin <= 1 error tests
%!error <Invalid call> repelem ()
%!error <Invalid call> repelem (1)
%!error repelem (5,[])
%!error repelem ([1 2 3 3 2 1])
%!error repelem ([1 2 3; 3 2 1])

## nargin == 2 error tests
%!error repelem ([1 2 3; 3 2 1],[])
%!error repelem ([1 2 3; 3 2 1],2)
%!error repelem ([1 2 3; 3 2 1],2)
%!error repelem ([1 2 3; 3 2 1],[1 2 3])
%!error repelem ([1 2 3; 3 2 1],[1 2 3]')
%!error repelem ([1 2 3; 3 2 1],[1 2 2 1])
%!error repelem ([1 2 3; 3 2 1],[1 2 3;4 5 6])
%!error repelem ([1 2 3 4 5],[1 2 3 4 5;1 2 3 4 5])

## nargin == 3 error tests
%!error repelem ([1 2 3; 3 2 1], 1, [1 2;1 2])
%!error repelem ([1 2 3; 3 2 1], 1, [1 2])
%!error repelem ([1 2 3; 3 2 1], 2, [])
%!error repelem ([1 2 3; 3 2 1], [1 2 3], [1 2 3])
%!error repelem ([1 2 3; 3 2 1], [1 2 3], [1 2 3 4])
%!error repelem ([1 2 3; 3 2 1], [1 2], [1 2 3 4])

## nargin > 3 error tests
%!error repelem ([1 2 3; 3 2 1], 1, [1 2;1 2],1,2,3)
%!error repelem ([1 2 3; 3 2 1], [],1,2,3)
%!error repelem ([1 2 3; 3 2 1], [1 2], [1 2 3],1,2,[1 2;1 2])
%!error repelem ([1 2 3; 3 2 1], [1 2 3], [1 2 3],1,2)
%!error repelem ([1 2 3; 3 2 1], [1 2], [1 2 3 4],1,2)
