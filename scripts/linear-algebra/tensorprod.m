########################################################################
##
## Copyright (C) 2022-2023 The Octave Project Developers
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
## @deftypefn  {} {@var{C} =} tensorprod (@var{A}, @var{B}, @var{dimA}, @var{dimB})
## @deftypefnx {} {@var{C} =} tensorprod (@var{A}, @var{B}, @var{dim})
## @deftypefnx {} {@var{C} =} tensorprod (@var{A}, @var{B})
## @deftypefnx {} {@var{C} =} tensorprod (@var{A}, @var{B}, "all")
## @deftypefnx {} {@var{C} =} tensorprod (@var{A}, @var{B}, @dots{}, "NumDimensionsA", @var{value})
## Compute the tensor product between numeric tensors @var{A} and @var{B}.
##
## The dimensions of @var{A} and @var{B} that are contracted are defined by
## @var{dimA} and @var{dimB}, respectively.  @var{dimA} and @var{dimB} are
## scalars or equal length vectors that define the dimensions to match up.
## The matched dimensions of @var{A} and @var{B} must have the same number of
## elements.
##
## When only @var{dim} is used, it is equivalent to
## @code{@var{dimA} = @var{dimB} = @var{dim}}.
##
## When no dimensions are specified, @code{@var{dimA} = @var{dimB} = []}.  This
## computes the outer product between @var{A} and @var{B}.
##
## Using the @qcode{"all"} option results in the inner product between @var{A}
## and @var{B}.  This requires @code{size (@var{A}) == size (@var{B})}.
##
## Use the property-value pair with the property name @qcode{"NumDimensionsA"}
## when @var{A} has trailing singleton dimensions that should be transferred to
## @var{C}.  The specified @var{value} should be the total number of dimensions
## of @var{A}.
##
## Matlab Compatibility: Octave does not currently support the
## @qcode{"@var{property_name}=@var{value}"} syntax for the
## @qcode{"NumDimensionsA"} parameter.
##
## @seealso{kron, dot, mtimes}
## @end deftypefn

function C = tensorprod (A, B, varargin)

  ## FIXME: shortcut code paths could be added for trivial cases, such as if
  ##        either A or B are a scalars, null, identity tensors, etc.

  if (nargin < 2 || nargin > 6)
    print_usage ();
  endif

  ## Check that A and B are single or double
  if (! isfloat (A))
    error ("tensorprod: A must be a single or double precision array");
  endif

  if (! isfloat (B))
    error ("tensorprod: B must be a single or double precision array");
  endif

  ## Check for misplaced NumDimensionsA property
  NumDimensionsA = 0;
  if (nargin > 2)
    if (strcmpi (varargin{end}, "NumDimensionsA"))
      error (["tensorprod: a value for the NumDimensionsA property must ", ...
              "be provided"]);
    elseif (strncmpi (arg = inputname (nargin, false), "NumDimensionsA", 13))
      error ("tensorprod: NumDimensionsA=VALUE syntax is unsupported.  Use syntax 'NumDimensionsA', VALUE");
    endif
  endif
  ## Check for NumDimensionsA property
  if (nargin > 3)
    if (strcmpi (varargin{end-1}, "NumDimensionsA"))
      if (! (isnumeric (varargin{end}) && isscalar (varargin{end})))
        error (["tensorprod: value for NumDimensionsA must be a ", ...
                "numeric scalar"]);
      elseif (! isindex (varargin{end}))
        error (["tensorprod: value for NumDimensionsA must be a ", ...
                "positive integer"]);
      endif
      NumDimensionsA = varargin{end};
      nargin = nargin - 2;
    endif
  endif

  ndimargs = nargin - 2;

  ## Set dimA and dimB
  if (ndimargs == 0)
    ## Calling without dimension arguments
    dimA = [];
    dimB = [];
  elseif (ndimargs == 1)
    ## Calling with dim or "all" option
    if (isnumeric (varargin{1}))
      if (! (isvector (varargin{1}) || isnull (varargin{1})))
        error ("tensorprod: dim must be a numeric vector of integers or []");
      endif
      ## Calling with dim
      dimA = varargin{1}(:).';  # Reshape to row vector
    elseif (ischar (varargin{1}))
      if (strcmpi (varargin{1}, "all"))
        if (! size_equal (A, B))
          error (["tensorprod: size of A and B must be identical when ", ...
                  "using the 'all' option"]);
        endif
      else
        error ("tensorprod: unknown option '%s'", varargin{1});
      endif
      ## Calling with "all" option
      dimA = 1:ndims(A);
    else
      error (["tensorprod: third argument must be a numeric vector of ", ...
              "integers, [], or 'all'"]);
    endif
    dimB = dimA;
  elseif (ndimargs == 2)
    ## Calling with dimA and dimB
    if (! (isnumeric (varargin{1}) && (isvector (varargin{1}) || ...
        isnull (varargin{1}))))
      error("tensorprod: dimA must be a numeric vector of integers or []");
    endif

    if (! (isnumeric (varargin{2}) && (isvector (varargin{2}) || ...
        isnull (varargin{2}))))
      error ("tensorprod: dimB must be a numeric vector of integers or []");
    endif

    if (numel (varargin{1}) != numel (varargin{2}))
      error (["tensorprod: an equal number of dimensions must be ", ...
              "matched for A and B"]);
    endif
    dimA = varargin{1}(:).';  # Reshape to row vector
    dimB = varargin{2}(:).';
  else
    ## Something is wrong - try to find the error
    for i = 1:ndimargs
      if (ischar (varargin{i}))
        if (strcmpi (varargin{i}, "NumDimensionsA"))
          error ("tensorprod: misplaced 'NumDimensionsA' option");
        elseif (strcmpi (varargin{i}, "all"))
          error ("tensorprod: misplaced 'all' option");
        else
          error ("tensorprod: unknown option '%s'", varargin{i});
        endif
      elseif (! isnumeric (varargin{i}))
        error (["tensorprod: optional arguments must be numeric vectors ", ...
                "of integers, [], 'all', or 'NumDimensionsA'"]);
      endif
    endfor
    error ("tensorprod: too many dimension inputs given");
  endif

  ## Check that dimensions are positive integers ([] will also pass)
  if (! isindex (dimA) || ! isindex (dimB))
    error ("tensorprod: dimensions must be positive integers");
  endif

  ## Check that the length of matched dimensions are equal
  if (any (size (A, dimA) != size (B, dimB)))
    error (["tensorprod: matched dimensions of A and B must have the ", ...
            "same lengths"]);
  endif

  ## Find size and ndims of A and B
  ndimsA = max ([ndims(A), max(dimA)]);
  sizeA = size (A, 1:ndimsA);
  ndimsB = max ([ndims(B), max(dimB)]);
  sizeB = size (B, 1:ndimsB);

  ## Take NumDimensionsA property into account
  if (NumDimensionsA)
    if (NumDimensionsA < ndimsA)
      if (ndimargs == 1)
        error (["tensorprod: highest dimension of dim must be less than ", ...
                "or equal to NumDimensionsA"]);
      elseif (ndimargs == 2)
        error (["tensorprod: highest dimension of dimA must be less ", ...
                "than or equal to NumDimensionsA"]);
      else
        error (["tensorprod: NumDimensionsA cannot be smaller than the ", ...
                "number of dimensions of A"]);
      endif
    elseif (NumDimensionsA > ndimsA)
      sizeA = [sizeA, ones(1, NumDimensionsA - ndimsA)];
      ndimsA = NumDimensionsA;
    endif
  endif

  ## Interchange the dimension to sum over the end of A and the front of B
  ## Prepare for A
  remainDimA = [1:ndimsA];
  remainDimA(dimA) = [];   # Dimensions of A to keep
  newDimOrderA = [remainDimA, dimA]; # New dim order [to_keep, to_contract]
  newSizeA = [prod(sizeA(remainDimA)), prod(sizeA(dimA))]; # Temp. 2D size for A

  ## Prepare for B (See comments for A.  Note that in principle,
  ## prod (sizeB (dimB)) should always be equal to prod (sizeA (dimA)).  May
  ## be able to optimize further here.
  remainDimB = [1:ndimsB];
  remainDimB(dimB) = [];   # Dimensions of B to keep
  newDimOrderB = [remainDimB, dimB];
  newSizeB = [prod(sizeB(remainDimB)), prod(sizeB(dimB))];

  ## Do reshaping into 2D array
  newA = reshape (permute (A, newDimOrderA), newSizeA);
  newB = reshape (permute (B, newDimOrderB), newSizeB);

  ## Compute
  C = newA * newB.';

  ## If not an inner product, reshape back to tensor
  if (! isscalar (C))
    ## Contribution to size of C from remaining A dims
    remainSizeA = sizeA(remainDimA);
    remainSizeB = sizeB(remainDimB);
    C = reshape (C, [remainSizeA, remainSizeB]);
  endif

endfunction


%!assert (tensorprod (2, 3), 6)
%!assert (tensorprod (2, 3, 1), 6)
%!assert (tensorprod (2, 3, 2), 6)
%!assert (tensorprod (2, 3, 10), 6)
%!assert (tensorprod (2, 3, [1 2]), 6)
%!assert (tensorprod (2, 3, [1 10]), 6)
%!assert (tensorprod (2, 3, []), 6)
%!assert (tensorprod (2, 3, 2, 1), 6)
%!assert (tensorprod (2, 3, [], []), 6)

%!shared v1, v2, M1, M2, T
%! v1 = [1, 2];
%! M1 = [1, 2; 3, 4];
%! M2 = [1, 2; 3, 4; 5, 6];
%! T = cat (3, M2, M2);

%!assert (tensorprod (3, v1), reshape ([3, 6], [1, 1, 1, 2]))
%!assert (tensorprod (v1, 3), [3, 6])
%!assert (tensorprod (v1, v1, "all"), 5)
%!assert (tensorprod (v1, v1), reshape ([1, 2, 2, 4], [1, 2, 1, 2]))
%!assert (tensorprod (v1, v1, 1), [1, 2; 2, 4])
%!assert (tensorprod (v1, v1, 2), 5)
%!assert (tensorprod (v1, v1, 3), reshape ([1, 2, 2, 4], [1, 2, 1, 2]))
%!assert (tensorprod (v1, v1, 5), reshape ([1, 2, 2, 4], [1, 2, 1, 1, 1, 2]))

%!assert (tensorprod (M1, v1), cat (4, [1,2;3,4], [2,4;6,8]))
%!assert (tensorprod (M1, v1'), cat (3, [1,2;3,4], [2,4;6,8]))
%!assert (tensorprod (v1, M1), reshape ([1 2 3 6 2 4 4 8], [1,2,2,2]))
%!assert (tensorprod (v1', M1), reshape ([1 2 3 6 2 4 4 8], [2,1,2,2]))
%!assert (tensorprod (M1, v1', 2, 1), [5; 11])
%!assert (tensorprod (M1, v1', 4, 4), cat(4, M1, 2*M1))
%!assert (tensorprod (M1, v1', [1, 3]), [7; 10])
%!assert (tensorprod (M1, v1', [1, 3], [1, 3]), [7; 10])
%!assert (tensorprod (M1, v1', [2, 3], [1, 3]), [5; 11])
%!assert (tensorprod (M1, v1', [2; 3], [1; 3]), [5; 11])
%!assert (tensorprod (M1, v1', [2; 3], [1, 3]), [5; 11])
%!assert (tensorprod (M1, v1', [2, 3], [1; 3]), [5; 11])
%!assert (tensorprod (M1, v1', [], []), cat (3, M1, 2*M1))
%!assert (tensorprod (M1, M1, "all"), 30)
%!assert (tensorprod (M1, M1, 1), [10, 14; 14, 20])
%!assert (tensorprod (M1, M1, 2), [5, 11; 11, 25])
%!assert (tensorprod (M1, M2, 2), [5, 11, 17; 11, 25, 39])
%!assert (tensorprod (M1, M2, 1, 2), [7, 15, 23; 10, 22, 34])
%!assert (tensorprod (M1, M2), reshape ([1,3,2,4,3,9,6,12,5,15,10,20,2,6,4, ...
%!                                      8,4,12,8,16,6,18,12,24], [2,2,3,2]))

%!assert (tensorprod (T, M1),
%!        reshape([1,3,5,2,4,6,1,3,5,2,4,6,3,9,15,6,12,18,3,9,15,6,12,18,2, ...
%!                6,10,4,8,12,2,6,10,4,8,12,4,12,20,8,16,24,4,12,20,8,16,24],
%!                [3,2,2,2,2]))
%!assert (tensorprod (T, M1, 2),
%!        cat (3, [5, 5; 11 11; 17, 17], [11, 11; 25, 25; 39, 39]))
%!assert (tensorprod (T, M2, 1), cat (3, [35, 35; 44, 44], [44, 44; 56, 56]))
%!assert (tensorprod (T, M2, 2), cat (3, [5, 5; 11, 11; 17, 17],
%!                     [11,11;25,25;39,39], [17, 17; 39, 39; 61, 61]))
%!assert (tensorprod (T, T, "all"), 182)
%!assert (tensorprod (T, T, 1),
%!        reshape ([35,44,35,44,44,56,44,56,35,44,35,44,44,56,44,56],
%!                 [2,2,2,2]))
%!assert (tensorprod (T, T, 2),
%!        reshape ([5,11,17,5,11,17,11,25,39,11,25,39,17,39,61,17,39,61,5, ...
%!                 11,17,5,11,17,11,25,39,11,25,39,17,39,61,17,39,61],
%!                 [3,2,3,2]))
%!assert (tensorprod (T, T, 3),
%!        reshape ([2,6,10,4,8,12,6,18,30,12,24,36,10,30,50,20,40,60,4,12, ...
%!                 20,8,16,24,8,24,40,16,32,48,12,36,60,24,48,72], [3,2,3,2]));
%!assert (tensorprod (T, T, 10),
%!        reshape ([1,3,5,2,4,6,1,3,5,2,4,6,3,9,15,6,12,18,3,9,15,6,12,18, ...
%!                 5,15,25,10,20,30,5,15,25,10,20,30,2,6,10,4,8,12,2,6,10, ...
%!                 4,8,12,4,12,20,8,16,24,4,12,20,8,16,24,6,18,30,12,24,36, ...
%!                 6,18,30,12,24,36,1,3,5,2,4,6,1,3,5,2,4,6,3,9,15,6,12,18, ...
%!                 3,9,15,6,12,18,5,15,25,10,20,30,5,15,25,10,20,30,2,6,10, ...
%!                 4,8,12,2,6,10,4,8,12,4,12,20,8,16,24,4,12,20,8,16,24,6, ...
%!                 18,30,12,24,36,6,18,30,12,24,36],
%!                 [3,2,2,1,1,1,1,1,1,3,2,2]))
%!assert (tensorprod (T, T, []),
%!        reshape ([1,3,5,2,4,6,1,3,5,2,4,6,3,9,15,6,12,18,3,9,15,6,12,18, ...
%!                 5,15,25,10,20,30,5,15,25,10,20,30,2,6,10,4,8,12,2,6,10, ...
%!                 4,8,12,4,12,20,8,16,24,4,12,20,8,16,24,6,18,30,12,24,36, ...
%!                 6,18,30,12,24,36,1,3,5,2,4,6,1,3,5,2,4,6,3,9,15,6,12,18, ...
%!                 3,9,15,6,12,18,5,15,25,10,20,30,5,15,25,10,20,30,2,6,10, ...
%!                 4,8,12,2,6,10,4,8,12,4,12,20,8,16,24,4,12,20,8,16,24,6, ...
%!                 18,30,12,24,36,6,18,30,12,24,36],
%!                 [3,2,2,3,2,2]))
%!assert (tensorprod (T, T, 2, 3),
%!        reshape ([3,7,11,3,7,11,9,21,33,9,21,33,15,35,55,15,35,55,6,14, ...
%!                 22,6,14,22,12,28,44,12,28,44,18,42,66,18,42,66],
%!                 [3,2,3,2]))
%!assert (tensorprod (T, T(1:2, 1:2, :), [2, 3],[1, 3]),
%!        [14, 20; 30, 44; 46, 68])
%!assert (tensorprod (T, T(1:2, 1:2, :), [3, 2],[1, 3]),
%!        [12, 18; 28, 42; 44, 66])
%!assert (tensorprod (T, reshape (T, [2, 2, 3]), 2, 1),
%!        reshape ([7,15,23,7,15,23,9,23,37,9,23,37,16,36,56,16,36,56,7,15, ...
%!                 23,7,15,23,9,23,37,9,23,37,16,36,56,16,36,56],
%!                 [3,2,2,3]))
%!assert (tensorprod (T, T, [1, 3]), [70, 88; 88, 112])
%!assert (tensorprod (T, T, [1, 3]), tensorprod (T, T, [3, 1]))
%!assert (tensorprod (T, reshape (T, [2, 2, 3]), [2, 3], [1, 2]),
%!        [16, 23, 25; 38, 51, 59; 60, 79, 93])

## NumDimensionsA tests
%!assert (tensorprod (v1, v1, "NumDimensionsA", 2),
%!        reshape ([1, 2, 2, 4], [1, 2, 1, 2]));
%!assert (tensorprod (v1, v1, "numdimensionsa", 2),
%!        tensorprod (v1, v1, "NumDimensionsA", 2));
%!assert (tensorprod (v1, v1, "NumDimensionsA", 3),
%!        reshape ([1, 2, 2, 4], [1, 2, 1, 1, 2]));
%!assert (tensorprod (v1, v1, [], "NumDimensionsA", 3),
%!        reshape ([1, 2, 2, 4], [1, 2, 1, 1, 2]));
%!assert (tensorprod (v1, v1, [], [], "NumDimensionsA", 3),
%!        reshape ([1, 2, 2, 4], [1, 2, 1, 1, 2]));
%!assert (tensorprod (v1, v1, "all", "NumDimensionsA", 3), 5);
%!assert (tensorprod (M1, v1, 2, "NumDimensionsA", 2), [5; 11]);
%!assert (tensorprod (M1, v1, 2, "NumDimensionsA", 5), [5; 11]);
%!assert (tensorprod (M1, v1, [2, 3], "NumDimensionsA", 5), [5; 11]);
%!assert (tensorprod (M1, M2, "NumDimensionsA", 2), reshape ([1,3,2,4,3,9,6, ...
%!        12,5,15,10,20,2,6,4,8,4,12,8,16,6,18,12,24], [2,2,3,2]))
%!assert (tensorprod (M1, M2, "NumDimensionsA", 3), reshape ([1,3,2,4,3,9,6, ...
%!        12,5,15,10,20,2,6,4,8,4,12,8,16,6,18,12,24], [2,2,1,3,2]))
%!assert (tensorprod (T, T, 1, "NumDimensionsA", 3),
%!        reshape ([35,44,35,44,44,56,44,56,35,44,35,44,44,56,44,56],
%!                 [2,2,2,2]))
%!assert (tensorprod (T, T, 3, "NumDimensionsA", 3),
%!        reshape ([2,6,10,4,8,12,6,18,30,12,24,36,10,30,50,20,40,60,4,12, ...
%!                20,8,16,24, 8,24,40,16,32,48,12,36,60,24,48,72],
%!                [3,2,3,2]))
%!assert (tensorprod (T, T, 1, "NumDimensionsA", 4),
%!        reshape ([35,44,35,44,44,56,44,56,35,44,35,44,44,56,44,56],
%!                [2,2,1,2,2]))
%!assert (tensorprod (T, T, 4, "NumDimensionsA", 4),
%!        reshape ([1,3,5,2,4,6,1,3,5,2,4,6,3,9,15,6,12,18,3,9,15,6,12,18,5, ...
%!                15,25,10,20,30,5,15,25,10,20,30,2,6,10,4,8,12,2,6,10,4,8, ...
%!                12,4,12,20,8,16,24,4,12,20,8,16,24,6,18,30,12,24,36,6,18, ...
%!                30,12,24,36,1,3,5,2,4,6,1,3,5,2,4,6,3,9,15,6,12,18,3,9,15, ...
%!                6,12,18,5,15,25,10,20,30,5,15,25,10,20,30,2,6,10,4,8,12,2, ...
%!                6,10,4,8,12,4,12,20,8,16,24,4,12,20,8,16,24,6,18,30,12,24, ...
%!                36,6,18,30,12,24,36],
%!                [3,2,2,3,2,2]))

## Test empty inputs
%!assert (tensorprod ([], []), zeros (0, 0, 0, 0))
%!assert (tensorprod ([], 1), [])
%!assert (tensorprod (1, []), zeros (1, 1, 0, 0))
%!assert (tensorprod (zeros (0, 0, 0), zeros (0, 0, 0)), zeros (0, 0, 0, 0, 0, 0))
%!assert (tensorprod ([], [], []), zeros (0, 0, 0, 0))
%!assert (tensorprod ([], [], 1), [])
%!assert (tensorprod ([], [], 2), [])
%!assert (tensorprod ([], [], 3), zeros (0, 0, 0, 0))
%!assert (tensorprod ([], [], 4), zeros (0, 0, 1, 0, 0))
%!assert (tensorprod ([], [], 5), zeros (0, 0, 1, 1, 0, 0))
%!assert (tensorprod ([], [], 3, "NumDimensionsA", 4), zeros (0, 0, 1, 0, 0))
%!assert (tensorprod ([], [], 3, 4, "NumDimensionsA", 5), zeros (0, 0, 1, 1, 0, 0))

## Test input validation
%!error <Invalid call> tensorprod ()
%!error <Invalid call> tensorprod (1)
%!error <Invalid call> tensorprod (1,2,3,4,5,6,7)
%!error <A must be a single or double precision array> tensorprod ("foo", 1)
%!error <B must be a single or double precision array> tensorprod (1, "bar")
%!error <A must be a single or double precision array> tensorprod (int32(1), 1)
%!error <B must be a single or double precision array> tensorprod (1, int32(1))
%!error <value for the NumDimensionsA property must be provided> tensorprod (1, 1, "NumDimensionsA")
%!error <NumDimensionsA=VALUE syntax is unsupported> tensorprod (1, 1, NumDimensionsA=1)
%!error <value for NumDimensionsA must be a numeric scalar> tensorprod (1, 1, 2, 1, "NumDimensionsA", "foo")
%!error <value for NumDimensionsA must be a numeric scalar> tensorprod (1, 1, 2, 1, "NumDimensionsA", [1 2])
%!error <value for NumDimensionsA must be a positive integer> tensorprod (1, 1, 2, 1, "NumDimensionsA", -1)
%!error <value for NumDimensionsA must be a positive integer> tensorprod (1, 1, 2, 1, "NumDimensionsA", 0)
%!error <value for NumDimensionsA must be a positive integer> tensorprod (1, 1, 2, 1, "NumDimensionsA", 1.5)
%!error <value for NumDimensionsA must be a positive integer> tensorprod (1, 1, 2, 1, "NumDimensionsA", NaN)
%!error <value for NumDimensionsA must be a positive integer> tensorprod (1, 1, 2, 1, "NumDimensionsA", Inf)
%!error <dim must be a numeric vector of integers or \[\]> tensorprod (1, 1, ones (2,2))
%!error <dim must be a numeric vector of integers or \[\]> tensorprod (1, 1, zeros (0,0,0))
%!error <size of A and B must be identical when using the 'all' option> tensorprod (ones (3, 4), ones (4, 3), "all")
%!error <unknown option 'foo'> tensorprod (1, 1, "foo")
%!error <third argument must be a numeric vector of integers, \[\], or 'all'> tensorprod (1, 1, {})
%!error <dimA must be a numeric vector of integers or \[\]> tensorprod (1, 1, "foo", 1)
%!error <dimA must be a numeric vector of integers or \[\]> tensorprod (1, 1, ones (2,2), 1)
%!error <dimA must be a numeric vector of integers or \[\]> tensorprod (1, 1, zeros (0,0,0), 1)
%!error <dimB must be a numeric vector of integers or \[\]> tensorprod (1, 1, 1, "bar")
%!error <dimB must be a numeric vector of integers or \[\]> tensorprod (1, 1, 1, ones (2,2))
%!error <dimB must be a numeric vector of integers or \[\]> tensorprod (1, 1, 1, zeros (0,0,0))
%!error <an equal number of dimensions must be matched for A and B> tensorprod (ones (3, 4), ones (4, 3), 1, [1, 2])
%!error <an equal number of dimensions must be matched for A and B> tensorprod (ones (3, 4), ones (4, 3), 1, [])
%!error <an equal number of dimensions must be matched for A and B> tensorprod (ones (3, 4), ones (4, 3), [], [1, 2])
%!error <misplaced 'NumDimensionsA' option> tensorprod (1, 1, "NumDimensionsA", 1, 1)
%!error <misplaced 'all' option> tensorprod (1, 1, 1, "all", 1)
%!error <unknown option 'foo'> tensorprod (1, 1, 1, "foo", 1)
%!error <optional arguments must be numeric vectors of integers, \[\], 'all', or 'NumDimensionsA'> tensorprod (1, 1, 1, {}, 1)
%!error <too many dimension inputs given> tensorprod (1, 1, 2, 1, 1)
%!error <too many dimension inputs given> tensorprod (1, 1, 2, 1, 1, 1)
%!error <dimensions must be positive integers> tensorprod (1, 1, 0)
%!error <dimensions must be positive integers> tensorprod (1, 1, -1)
%!error <dimensions must be positive integers> tensorprod (1, 1, 1.5)
%!error <dimensions must be positive integers> tensorprod (1, 1, NaN)
%!error <dimensions must be positive integers> tensorprod (1, 1, Inf)
%!error <matched dimensions of A and B must have the same lengths> tensorprod (ones (3, 4), ones (4, 3), 1)
%!error <matched dimensions of A and B must have the same lengths> tensorprod (ones (3, 4), ones (4, 3), 1, 1)
%!error <highest dimension of dim must be less than or equal to NumDimensionsA> tensorprod (1, 1, 5, "NumDimensionsA", 4)
%!error <highest dimension of dimA must be less than or equal to NumDimensionsA> tensorprod (1, 1, 5, 2, "NumDimensionsA", 4)
%!error <NumDimensionsA cannot be smaller than the number of dimensions of A> tensorprod (ones (2, 2, 2), 1, "NumDimensionsA", 2)
