########################################################################
##
## Copyright (C) 2007-2025 The Octave Project Developers
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
## @deftypefn  {} {@var{m} =} mode (@var{x})
## @deftypefnx {} {@var{m} =} mode (@var{x}, @var{dim})
## @deftypefnx {} {@var{m} =} mode (@var{x}, @var{vecdim})
## @deftypefnx {} {@var{m} =} mode (@var{x}, "all")
## @deftypefnx {} {[@var{m}, @var{f}, @var{c}] =} mode (@dots{})
## Compute the most frequently occurring value in the input data @var{x}.
##
## @code{mode} determines the frequency of values along the first non-singleton
## dimension and returns the value with the highest frequency.  If two, or
## more, values have the same frequency @code{mode} returns the smallest.
##
## The optional input @var{dim} specifies the dimension to operate on and must
## be a positive integer.  Specifying any singleton dimension of @var{x},
## including any dimension exceeding @code{ndims (@var{x})}, will return
## @var{x}.
##
## Specifying multiple dimensions with input @var{vecdim}, a vector of
## non-repeating dimensions, will operate along the array slice defined by
## @var{vecdim}.  If @var{vecdim} indexes all dimensions of @var{x}, then it is
## equivalent to the option @qcode{"all"}.  Any dimension in @var{vecdim}
## greater than @code{ndims (@var{x})} is ignored.  If all dimensions in
## @var{vecdim} are greater than @code{ndims (@var{x})}, then @code{mode}
## will return @var{x}.
##
## Specifying the dimension as @qcode{"all"} will cause @code{mode} to operate
## on all elements of @var{x}, and is equivalent to @code{mode (@var{x}(:))}.
##
## The return variable @var{f} is the number of occurrences of the mode in
## the dataset.
##
## The cell array @var{c} contains all of the elements with the maximum
## frequency.
## @seealso{mean, median}
## @end deftypefn

function [m, f, c] = mode (x, dim)

  if (nargin < 1)
    print_usage ();
  endif

  if (! (isnumeric (x)))
    error ("mode: X must be a numeric array");
  endif

  do_perm = false;
  nd = ndims (x);
  sz = size (x);
  empty_x = isempty (x);

  if (nargin < 2)
    ## Find the first non-singleton dimension.
    (dim = find (sz != 1, 1)) || (dim = 1);

    ## Return immediately for an empty matrix.
    if (empty_x)
      ## Empty x produces NaN for m, 0 for f, , but m, f and c
      ## shape depends on size of x.
      if (nd == 2 && all (sz == 0))
        f = 0; # f always a double even if x is single.
        if (isa (x, "single"))
          m = NaN ("single");
          c = {(NaN (0, 1, "single"))};
        else
          m = NaN;
          c = {(NaN (0, 1))};
        endif
      else
        sz(dim) = 1;
        f = zeros (sz); # f always a double even if x is single.
        c = cell (sz);
        if (isa (x, "single"))
          m = NaN (sz, "single");
          c(:) = NaN (1, 0, "single");
        else
          m = NaN (sz);
          c(:) = NaN (1, 0);
        endif
      endif
      return;
    endif
  endif

  if (isnumeric (dim))

    ## Check for DIM argument
    if (isscalar (dim))
      if (! (dim == fix (dim) && dim > 0))
        error ("mode: DIM must be a positive integer");
      endif

      ## Return immediately for an empty matrix.
      if (empty_x)
        ## Ignore exceeding dimension
        if (dim <= nd)
          sz(dim) = 1;
        endif
        f = zeros (sz); # f always a double even if x is single.
        c = cell (sz);
        if (isa (x, "single"))
          m = NaN (sz, "single");
          c(:) = NaN (1, 0, "single");
        else
          m = NaN (sz);
          c(:) = NaN (1, 0);
        endif
        return;
      endif

      if (dim > nd || sz(dim) == 1)
        ## Special case of mode over singleton dimension.
        m = x;
        f = ones (size (x));
        c = num2cell (x);
        return;
      endif

    ## Check for proper VECDIM (more than 1 dim, no repeats)
    elseif (isvector (dim) && isindex (dim) && all (diff (sort (dim))))

      ## Discard exceeding dims, unless all dims > nd so keep smallest
      vecdim = dim(dim <= nd);
      if (isempty (vecdim))
        dim = min (dim);
      else
        dim = vecdim;
      endif

      ## Return immediately for an empty matrix.
      if (empty_x)
        ## Ignore exceeding dimension
        if (all (dim <= nd))
          sz(dim) = 1;
        endif
        f = zeros (sz); # f always a double even if x is single.
        c = cell (sz);
        if (isa (x, "single"))
          m = NaN (sz, "single");
          c(:) = NaN (1, 0, "single");
        else
          m = NaN (sz);
          c(:) = NaN (1, 0);
        endif
        return;
      endif

      ## Return numel (p) copies of X if remaining DIM > nd
      if (any (dim > nd) || all (sz(dim) == 1))
        ## Special case of mode over singleton dimensions.
        m = x;
        f = ones (size (x));
        c = num2cell (x);
        return;
      endif

      ## Detect trivial case of DIM being all dimensions (same as "all").
      vecdims = numel (dim);
      max_dim = max (nd, max (dim));
      if (vecdims == nd && max_dim == nd)
        x = x(:);
        sz = size (x);
        dim = 1;
      else
        ## Algorithm: Move dimensions for operation to the front, keeping the
        ## order of the remaining dimensions.

        dim = dim(:).';  # Force row vector

        ## Permutation vector with DIM at front
        perm = [1:max_dim];
        perm(dim) = [];
        perm = [dim, perm];
        do_perm = true;

        ## Reset vecdim and dim for permuted and reshaped x
        vecdim = 1:numel (dim);
        dim = 1;

        ## Reshape X to put dims to process at front.
        x = permute (x, perm);
        sz = size (x);

        ## Preserve trailing singletons when dim > ndims (x).
        sx = [sz, ones(1, max_dim - numel (sz))];
        sx = [prod(sx(1:vecdims)), ones(1, (vecdims-1)), sx((vecdims+1):end)];

        ## Size must always have 2 dimensions.
        if (isscalar (sx))
          sx = [sx, 1];
        endif

        ## Collapse dimensions to be processsed into single column.
        x = reshape (x, sx);
      endif

    else
      error ("mode: VECDIM must be a vector of non-repeating positive integers");
    endif

  elseif (strcmpi (dim, "all"))

    ## Return immediately for an empty matrix
    if (empty_x)
      f = 0; # f always a double even if x is single.
      if (isa (x, "single"))
        m = single (NaN);
        c = {(NaN (1, 0, "single"))};
      else
        m = NaN;
        c = {(NaN (1, 0))};
      endif
      return;
    endif

    ## "all" simplifies to collapsing all elements to single vector.
    x = x(:);
    sz = size (x);
    dim = 1;

  else
    error ("mode: DIM must be a positive integer scalar, vector, or 'all'");
  endif

  sx = sz;
  if (do_perm)
    sx(vecdim) = 1;
  else
    sx(dim) = 1;
  endif

  if (issparse (x))
    t2 = sparse (sz(1), sz(2));
  else
    t2 = zeros (sz);
  endif

  if (dim != 1)
    perm = [dim, 1:dim-1, dim+1:nd];
    t2 = permute (t2, perm);
  endif

  xs = sort (x, dim);
  t = cat (dim, true (sx), diff (xs, 1, dim) != 0);

  if (dim != 1)
    t2(permute (t != 0, perm)) = diff ([find(permute (t, perm))(:); prod(sz)+1]);
    f = max (ipermute (t2, perm), [], dim);
    xs = permute (xs, perm);
  else
    t2(t) = diff ([find(t)(:); prod(sz)+1]);
    f = max (t2, [], dim);
  endif

  c = cell (sx);
  if (issparse (x))
    m = sparse (sx(1), sx(2));
  else
    m = zeros (sx, class (x));
  endif
  for i = 1 : prod (sx)
    c{i} = xs(t2(:, i) == f(i), i);
    m(i) = c{i}(1);
  endfor

  ## Permute the 1st index back to dim.
  if (do_perm)
    m = ipermute (m, perm);
    f = ipermute (f, perm);
    c = ipermute (c, perm);
  endif

endfunction


%!test
%! [m, f, c] = mode (toeplitz (1:5));
%! assert (m, [1,2,2,2,1]);
%! assert (f, [1,2,2,2,1]);
%! assert (c, {[1;2;3;4;5],[2],[2;3],[2],[1;2;3;4;5]});
%!test
%! [m, f, c] = mode (toeplitz (1:5), 2);
%! assert (m, [1;2;2;2;1]);
%! assert (f, [1;2;2;2;1]);
%! assert (c, {[1;2;3;4;5];[2];[2;3];[2];[1;2;3;4;5]});
%!test
%! a = sprandn (32, 32, 0.05);
%! sp0 = sparse (0);
%! [m, f, c] = mode (a);
%! [m2, f2, c2] = mode (full (a));
%! assert (m, sparse (m2));
%! assert (f, sparse (f2));
%! c_exp(1:length (a)) = { sp0 };
%! assert (c ,c_exp);
%! assert (c2,c_exp);

%!assert (mode ([2,3,1,2,3,4],1),[2,3,1,2,3,4])
%!assert (mode ([2,3,1,2,3,4],2),2)
%!assert (mode ([2,3,1,2,3,4]),2)
%!assert (mode (single ([2,3,1,2,3,4])), single (2))
%!assert (mode (int8 ([2,3,1,2,3,4])), int8 (2))

%!assert (mode ([2;3;1;2;3;4],1),2)
%!assert (mode ([2;3;1;2;3;4],2),[2;3;1;2;3;4])
%!assert (mode ([2;3;1;2;3;4]),2)

%!test
%! x = magic (3);
%! [m, f, c] = mode (x, 3);
%! assert (m, x);
%! assert (f, ones (3,3));
%! assert (c, num2cell (x));

%!test
%! x = single (magic (3));
%! [m, f, c] = mode (x, 3);
%! assert (class (m), "single");
%! assert (class (f), "double");
%! assert (class (c), "cell");
%! assert (class (c(1)), "cell");
%! assert (class (c{1}), "single");

%!shared x
%! x(:,:,1) = toeplitz (1:3);
%! x(:,:,2) = circshift (toeplitz (1:3), 1);
%! x(:,:,3) = circshift (toeplitz (1:3), 2);
%!test
%! [m, f, c] = mode (x, 1);
%! assert (reshape (m, [3, 3]), [1 1 1; 2 2 2; 1 1 1]);
%! assert (reshape (f, [3, 3]), [1 1 1; 2 2 2; 1 1 1]);
%! c = reshape (c, [3, 3]);
%! assert (c{1}, [1; 2; 3]);
%! assert (c{2}, 2);
%! assert (c{3}, [1; 2; 3]);
%!test
%! [m, f, c] = mode (x, 2);
%! assert (reshape (m, [3, 3]), [1 1 2; 2 1 1; 1 2 1]);
%! assert (reshape (f, [3, 3]), [1 1 2; 2 1 1; 1 2 1]);
%! c = reshape (c, [3, 3]);
%! assert (c{1}, [1; 2; 3]);
%! assert (c{2}, 2);
%! assert (c{3}, [1; 2; 3]);
%!test
%! [m, f, c] = mode (x, 3);
%! assert (reshape (m, [3, 3]), [1 2 1; 1 2 1; 1 2 1]);
%! assert (reshape (f, [3, 3]), [1 2 1; 1 2 1; 1 2 1]);
%! c = reshape (c, [3, 3]);
%! assert (c{1}, [1; 2; 3]);
%! assert (c{2}, [1; 2; 3]);
%! assert (c{3}, [1; 2; 3]);
%!shared   ## Clear shared to prevent variable echo for any later test failures

## Test empty inputs
%!test <*48690>
%! [m, f, c] = mode ([]);
%! assert (m, NaN);
%! assert (f, 0);
%! assert (c, {(NaN (0, 1))});
%!test <*48690>
%! [m, f, c] = mode (single ([]));
%! assert (class (m), "single");
%! assert (class (f), "double");
%! assert (c, {(single (NaN (0, 1)))});
%!test <*48690>
%! [m, f, c] = mode ([], 1);
%! assert (m, NaN (1, 0));
%! assert (f, zeros (1, 0));
%! assert (c, cell (1, 0));
%!test <*48690>
%! [m, f, c] = mode ([], 2);
%! assert (m, NaN (0, 1));
%! assert (f, zeros (0, 1));
%! assert (c, cell (0, 1));
%!test <*48690>
%! [m, f, c] = mode ([], 3);
%! assert (m, []);
%! assert (f, []);
%! assert (c, cell (0, 0));
%!test <*48690>
%! [m, f, c] = mode (ones (0, 1));
%! assert (m, NaN);
%! assert (f, 0);
%! assert (c, {(NaN (1, 0))});
%!test <*48690>
%! [m, f, c] = mode (ones (0, 1), 1);
%! assert (m, NaN);
%! assert (f, 0);
%! assert (c, {(NaN (1, 0))});
%!test <*48690>
%! [m, f, c] = mode (ones (0, 1), 2);
%! assert (m, NaN (0, 1));
%! assert (f, zeros (0, 1));
%! assert (c, cell (0, 1));
%!test <*48690>
%! [m, f, c] = mode (ones (0, 1), 3);
%! assert (m, NaN (0, 1));
%! assert (f, zeros (0, 1));
%! assert (c, cell (0, 1));
%!test <*48690>
%! [m, f, c] = mode (ones (1, 0));
%! assert (m, NaN);
%! assert (f, 0);
%! assert (c, {(NaN (1, 0))});
%!test <*48690>
%! [m, f, c] = mode (ones (1, 0), 1);
%! assert (m, NaN (1, 0));
%! assert (f, zeros (1, 0));
%! assert (c, cell (1, 0));
%!test <*48690>
%! [m, f, c] = mode (ones (1, 0), 2);
%! assert (m, NaN);
%! assert (f, 0);
%! assert (c, {(NaN (1, 0))});
%!test <*48690>
%! [m, f, c] = mode (ones (1, 0), 3);
%! assert (m, NaN (1, 0));
%! assert (f, zeros (1, 0));
%! assert (c, cell (1, 0));
%!test <*48690>
%! [m, f, c] = mode (ones (0, 0, 0));
%! assert (m, NaN (1, 0, 0));
%! assert (f, zeros (1, 0, 0));
%! assert (c, cell (1, 0, 0));
%!test <*48690>
%! [m, f, c] = mode (ones (0, 0, 0), 1);
%! assert (m, NaN (1, 0, 0));
%! assert (f, zeros (1, 0, 0));
%! assert (c, cell (1, 0, 0));
%!test <*48690>
%! [m, f, c] = mode (ones (0, 0, 0), 2);
%! assert (m, NaN (0, 1, 0));
%! assert (f, zeros (0, 1, 0));
%! assert (c, cell (0, 1, 0));
%!test <*48690>
%! [m, f, c] = mode (ones (0, 0, 0), 3);
%! assert (m, NaN (0, 0, 1));
%! assert (f, zeros (0, 0, 1));
%! assert (c, cell (0, 0, 1));
%!test <*48690>
%! [m, f, c] = mode (ones (1, 5, 0), 2);
%! assert (m, NaN (1, 1, 0));
%! assert (f, zeros (1, 1, 0));
%! assert (c, cell (1, 1, 0));
%!test <*48690>
%! [m, f, c] = mode (ones (5, 1, 0), 2);
%! assert (m, NaN (5, 1, 0));
%! assert (f, zeros (5, 1, 0));
%! assert (c, cell (5, 1, 0));
%!test <*48690>
%! [m, f, c] = mode (ones (2, 0));
%! assert (m, NaN (1, 0));
%! assert (f, zeros (1, 0));
%! assert (c, cell (1, 0));
%!test <*48690>
%! [m, f, c] = mode (ones (0, 2));
%! assert (m, NaN (1, 2));
%! assert (f, zeros (1, 2));
%! assert (c, {(NaN (1, 0)),(NaN (1, 0))});
%!test <*48690>
%! [m, f, c] = mode (ones (1, 1, 1, 0));
%! assert (m, NaN (1, 1));
%! assert (f, zeros (1, 1));
%! assert (c, {(NaN (1, 0))});
%!test <*48690>
%! [m, f, c] = mode (ones (1, 1, 1, 0), 1);
%! assert (m, NaN (1, 1, 1, 0));
%! assert (f, zeros (1, 1, 1, 0));
%! assert (c, cell (1,1,1,0));

## Test more ampty inputs with vecdim and "all"
%!test
%! [m, f, c] = mode ([], [2, 3]);
%! assert (m, NaN (0, 1));
%! assert (f, zeros (0, 1));
%! assert (c, cell (0, 1));
%!test
%! [m, f, c] = mode ([], [1, 3]);
%! assert (m, NaN (1, 0));
%! assert (f, zeros (1, 0));
%! assert (c, cell (1, 0));
%!test
%! [m, f, c] = mode (ones (1, 0), [1, 2]);
%! assert (m, NaN);
%! assert (f, 0);
%! assert (c, {(NaN (1, 0))});
%!test
%! [m, f, c] = mode (single (ones (1, 0)), [1, 2]);
%! assert (m, single (NaN));
%! assert (f, 0);
%! assert (c, {(NaN (1, 0, "single"))});
%!test
%! [m, f, c] = mode (ones (0, 2), [1, 2]);
%! assert (m, NaN);
%! assert (f, 0);
%! assert (c, {(NaN (1, 0))});
%!test
%! [m, f, c] = mode (ones (1, 0), [1, 3]);
%! assert (m, NaN (1, 0));
%! assert (f, zeros (1, 0));
%! assert (c, cell (1, 0));
%!test
%! [m, f, c] = mode (ones (0, 2), [1, 3]);
%! assert (m, [NaN, NaN]);
%! assert (f, [0, 0]);
%! assert (c, {(NaN (1, 0)), (NaN (1, 0))});
%!test
%! [m, f, c] = mode (ones (1, 0), [2, 3]);
%! assert (m, NaN);
%! assert (f, 0);
%! assert (c, {(NaN (1, 0))});
%!test
%! [m, f, c] = mode (single (ones (1, 0)), [2, 3]);
%! assert (m, single (NaN));
%! assert (f, 0);
%! assert (c, {(NaN (1, 0, "single"))});
%!test
%! [m, f, c] = mode (ones (0, 2), [2, 3]);
%! assert (m, NaN (0, 1));
%! assert (f, zeros (0, 1));
%! assert (c, cell (0, 1));
%!test
%! [m, f, c] = mode (ones (1, 0), [4, 3]);
%! assert (m, NaN (1, 0));
%! assert (f, zeros (1, 0));
%! assert (c, cell (1, 0));
%!test
%! [m, f, c] = mode (ones (0, 2), [4, 3]);
%! assert (m, NaN (0, 2));
%! assert (f, zeros (0, 2));
%! assert (c, cell (0, 2));
%!test
%! [m, f, c] = mode (ones (1, 1, 1, 0), [2, 3]);
%! assert (m, NaN (1, 1, 1, 0));
%! assert (f, zeros (1, 1, 1, 0));
%! assert (c, cell (1, 1, 1, 0));
%!test
%! [m, f, c] = mode (ones (1, 1, 1, 0), [1, 3]);
%! assert (m, NaN (1, 1, 1, 0));
%! assert (f, zeros (1, 1, 1, 0));
%! assert (c, cell (1, 1, 1, 0));
%!test
%! [m, f, c] = mode (ones (1, 1, 1, 0), [1, 2]);
%! assert (m, NaN (1, 1, 1, 0));
%! assert (f, zeros (1, 1, 1, 0));
%! assert (c, cell (1, 1, 1, 0));
%!test
%! [m, f, c] = mode (ones (1, 1, 1, 0), [1, 4]);
%! assert (m, NaN);
%! assert (f, 0);
%! assert (c, {(NaN (1, 0))});
%!test
%! [m, f, c] = mode (ones (3, 0), 'all');
%! assert (m, NaN);
%! assert (f, 0);
%! assert (c, {(NaN (1, 0))});
%!test
%! [m, f, c] = mode (ones (0, 3), 'all');
%! assert (m, NaN);
%! assert (f, 0);
%! assert (c, {(NaN (1, 0))});
%!test
%! [m, f, c] = mode (ones (1, 0, 2, 3), 'all');
%! assert (m, NaN);
%! assert (f, 0);
%! assert (c, {(NaN (1, 0))});
%!test
%! [m, f, c] = mode (ones (0, 3, 2, 1), 'all');
%! assert (m, NaN);
%! assert (f, 0);
%! assert (c, {(NaN (1, 0))});
%!test
%! [m, f, c] = mode ([], 'all');
%! assert (m, NaN);
%! assert (f, 0);
%! assert (c, {(NaN (1, 0))});
%!test
%! [m, f, c] = mode (ones (0, 0, 2, 1), 'all');
%! assert (m, NaN);
%! assert (f, 0);
%! assert (c, {(NaN (1, 0))});

## Test vecdim and "all"
%!test
%! x = repmat ([1, 2, 1, 3, 2, 4, 3, 2, 1], 3, 1, 2);
%! assert (mode (x, [1, 2]), repmat (1, [1, 1, 2]));
%! assert (mode (x, [1, 3]), [1, 2, 1, 3, 2, 4, 3, 2, 1]);
%! assert (mode (x, [2, 3]), [1; 1; 1]);
%! assert (mode (x, [1, 2, 3]), 1);
%! assert (mode (x, "all"), 1);

## Test input validation
%!error <Invalid call> mode ()
%!error <mode: X must be a numeric array> mode ({1 2 3})
%!error <mode: X must be a numeric array> mode ([true; false])
%!error <mode: DIM must be a positive integer> mode (1, 1.5)
%!error <mode: DIM must be a positive integer> mode (1, 0)
%!error <mode: VECDIM must be a vector of non-repeating positive integers> ...
%!       mode (1, [1, 2, 2])
%!error <mode: VECDIM must be a vector of non-repeating positive integers> ...
%!       mode (1, [1, 2, 0])
%!error <mode: DIM must be a positive integer scalar, vector, or 'all'> ...
%!       mode (1, "some")
