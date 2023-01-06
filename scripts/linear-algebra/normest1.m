########################################################################
##
## Copyright (C) 2016-2023 The Octave Project Developers
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
## @deftypefn  {} {@var{nest} =} normest1 (@var{A})
## @deftypefnx {} {@var{nest} =} normest1 (@var{A}, @var{t})
## @deftypefnx {} {@var{nest} =} normest1 (@var{A}, @var{t}, @var{x0})
## @deftypefnx {} {@var{nest} =} normest1 (@var{Afcn}, @var{t}, @var{x0}, @var{p1}, @var{p2}, @dots{})
## @deftypefnx {} {[@var{nest}, @var{v}] =} normest1 (@var{A}, @dots{})
## @deftypefnx {} {[@var{nest}, @var{v}, @var{w}] =} normest1 (@var{A}, @dots{})
## @deftypefnx {} {[@var{nest}, @var{v}, @var{w}, @var{iter}] =} normest1 (@var{A}, @dots{})
## Estimate the 1-norm of the matrix @var{A} using a block algorithm.
##
## @code{normest1} is best for large sparse matrices where only an estimate of
## the norm is required.  For small to medium sized matrices, consider using
## @code{norm (@var{A}, 1)}.  In addition, @code{normest1} can be used for the
## estimate of the 1-norm of a linear operator @var{A} when matrix-vector
## products @code{@var{A} * @var{x}} and @code{@var{A}' * @var{x}} can be
## cheaply computed.  In this case, instead of the matrix @var{A}, a function
## @code{@var{Afcn} (@var{flag}, @var{x})} is used; it must return:
##
## @itemize @bullet
## @item
## the dimension @var{n} of @var{A}, if @var{flag} is @qcode{"dim"}
##
## @item
## true if @var{A} is a real operator, if @var{flag} is @qcode{"real"}
##
## @item
## the result @code{@var{A} * @var{x}}, if @var{flag} is @qcode{"notransp"}
##
## @item
## the result @code{@var{A}' * @var{x}}, if @var{flag} is @qcode{"transp"}
## @end itemize
##
## A typical case is @var{A} defined by @code{@var{b} ^ @var{m}}, in which the
## result @code{@var{A} * @var{x}} can be computed without even forming
## explicitly @code{@var{b} ^ @var{m}} by:
##
## @example
## @group
## @var{y} = @var{x};
## for @var{i} = 1:@var{m}
##   @var{y} = @var{b} * @var{y};
## endfor
## @end group
## @end example
##
## The parameters @var{p1}, @var{p2}, @dots{} are arguments of
## @code{@var{Afcn} (@var{flag}, @var{x}, @var{p1}, @var{p2}, @dots{})}.
##
## The default value for @var{t} is 2.  The algorithm requires matrix-matrix
## products with sizes @var{n} x @var{n} and @var{n} x @var{t}.
##
## The initial matrix @var{x0} should have columns of unit 1-norm.  The default
## initial matrix @var{x0} has the first column
## @code{ones (@var{n}, 1) / @var{n}} and, if @var{t} > 1, the remaining
## columns with random elements @code{-1 / @var{n}}, @code{1 / @var{n}},
## divided by @var{n}.
##
## On output, @var{nest} is the desired estimate, @var{v} and @var{w}
## are vectors such that @code{@var{w} = @var{A} * @var{v}}, with
## @code{norm (@var{w}, 1)} = @code{@var{c} * norm (@var{v}, 1)}.  @var{iter}
## contains in @code{@var{iter}(1)} the number of iterations (the maximum is
## hardcoded to 5) and in @code{@var{iter}(2)} the total number of products
## @code{@var{A} * @var{x}} or @code{@var{A}' * @var{x}} performed by the
## algorithm.
##
## Algorithm Note: @code{normest1} uses random numbers during evaluation.
## Therefore, if consistent results are required, the @qcode{"state"} of the
## random generator should be fixed before invoking @code{normest1}.
##
## Reference: @nospell{N. J. Higham and F. Tisseur},
## @cite{A block algorithm for matrix 1-norm estimation, with and
## application to 1-norm @nospell{pseudospectra}},
## @nospell{SIAM J. Matrix Anal.@: Appl.@:},
## pp.@: 1185--1201, Vol 21, No.@: 4, 2000.
##
## @seealso{normest, norm, cond, condest}
## @end deftypefn

## Ideally, we would set t and X to their default values but Matlab
## compatibility would require we set the default even when they are empty.
function [nest, v, w, iter] = normest1 (A, t = [], x0 = [], varargin)

  if (nargin < 1)
    print_usage ();
  endif

  if (isempty (t))
    t = 2;
  endif

  ## FIXME: t < 0 should print trace information
  if (isnumeric (A) && issquare (A))
    Aismat = true;
    Aisreal = isreal (A);
    n = rows (A);
    if (n <= 4 || t == n)
      ## small input, compute directly
      [nest, idx] = max (sum (abs (A), 1), [] , 2);
      v = zeros (n, 1);
      v(idx) = 1;
      w = A(:, idx);
      ## Matlab incompatible on purpose.  Matlab returns iter as a row vector
      ## for this special case, but a column vector in all other cases.
      ## This is obviously a bug in Matlab that we don't reproduce.
      iter = [0; 1];
      return;
    endif
  elseif (is_function_handle (A))
    Aismat = false;
    Aisreal = A ("real", [], varargin{:});
    n = A ("dim", [], varargin{:});
    Afcn = @(x) A ("notransp", x, varargin{:});
    A1fcn = @(x) A ("transp", x, varargin{:});
  else
    error ("normest1: A must be a square matrix or a function handle");
  endif

  t = min (t, n);

  if (isempty (x0))
    X = [ones(n, 1), sign(2 * rand(n, t - 1) - 1)];
    i = 2;
    imax = min (t, 2^(n-1));
    ## There are at most 2^(n-1) unparallel columns, see later.
    while (i <= imax)
      if (any (abs (X(:,i)' * X(:,1:i-1)) == n))
        ## column i is parallel to a column 1:i-1.  Change it.
        X(:,i) = sign (2 * rand (n, 1) - 1);
      else
        i++;
      endif
    endwhile
    X /= n;
  else
    if (columns (x0) < t)
      error ("normest1: X0 must have %d columns", t);
    endif
    X = x0;
  endif

  itmax = 5;
  idx_hist = zeros (n, 1);
  nest_old = 0;
  idx = zeros (n, 1);
  S = zeros (n, t);
  iter = [0; 0];
  converged = false;
  while (! converged && (iter(1) < itmax))
    iter(1)++;
    if (Aismat)
      Y = A * X;
    else
      Y = Afcn (X);
    endif
    iter(2)++;
    [nest, j] = max (sum (abs (Y), 1), [], 2);
    if ((nest > nest_old) || (iter(1) == 2))
      idx_best = idx(j);
      w = Y(:, j);  # there is an error in Algorithm 2.4
    endif
    if (nest <= nest_old && iter(1) >= 2)  # (1) of Algorithm 2.4
      nest = nest_old;
      break;  # while
    endif
    nest_old = nest;
    Sold = S;
    S = sign (Y);
    S(S==0) = 1;
    possible_break = false;
    if (Aisreal)
      ## test parallel (only real case)
      if (all (any (abs (Sold' * S) == n)))  # (2) of Algorithm 2.4
        ## all columns of S parallel to a column of Sold, exit
        possible_break = true;
        converged = true;
      else
        if (t > 1)
          ## at least two columns of S are not parallel
          i = 1;
          ## The maximum number of unparallel columns of length n with
          ## entries {-1,1} is 2^(n-1).  n of them are already in Sold.
          imax = min (t, 2 ^ (n - 1) - n);
          while (i <= imax)
            if (any (abs (S(:,i)' * S(:,1:i-1)) == n)
                || any (abs (S(:,i)' * Sold) == n))
              ## i-th column of S is parallel to a previous column
              ## or to a column of Sold.  Change it.
              S(:,i) = sign (2*rand (n, 1)-1);
            else
              i++;
            endif
          endwhile
        endif
      endif
    endif
    if (! possible_break)
      if (Aismat)
        Z = A' * S;
      else
        Z = A1fcn (S);  # (3) of Algorithm 2.4
      endif
      iter(2)++;
      h = max (abs (Z), [], 2);
      idx = (1:n)';
      if (iter(1) >= 2 && (max (h, [], 1) == h(idx_best)))  # (4) of Alg. 2.4
        break;  # while
      endif
      [h, idx] = sort (h, "descend");
      if (t > 1)
       if (all (idx_hist(idx(1:t)))) # (5) of Algorithm 2.4
          break;  # while
        endif
        idx = idx(! idx_hist(idx));
        ## length(idx) could be less than t, especially if t is not << n.
        ## This is not considered in point (5) of Algorithm 2.4.
        tmax = min (numel (idx), t);
        idx = idx(1:tmax);
      else
        tmax = 1;
      endif
      X = zeros (n, tmax);
      X(sub2ind (size (X), idx(1:tmax), (1:tmax)')) = 1;
      idx_hist(idx(1:tmax)) = 1;
    endif
  endwhile
  v = zeros (n, 1);
  v(idx_best) = 1;

endfunction


%!function z = afcn_A (flag, x, A, n)
%!  switch (flag)
%!  case {"dim"}
%!    z = n;
%!  case {"real"}
%!    z = isreal (A);
%!  case {"transp"}
%!    z = A' * x;
%!  case {"notransp"}
%!    z = A * x;
%!  endswitch
%!endfunction
%!function z = afcn_A_P (flag, x, A, m)
%!  switch (flag)
%!  case "dim"
%!    z = length (A);
%!  case "real"
%!    z = isreal (A);
%!  case "transp"
%!    z = x; for i = 1:m, z = A' * z;, endfor
%!  case "notransp"
%!    z = x; for i = 1:m, z = A * z;, endfor
%!  endswitch
%!endfunction

%!test
%! A = reshape ((1:16)-8, 4, 4);
%! assert (normest1 (A), norm (A, 1), eps);

## test t=1
%!test
%! A = rand (4); # for positive matrices always work
%! assert (normest1 (A, 1), norm (A,1), 2 * eps);

## test t=3
%!test
%! A = [-0.21825   0.16598   0.19388   0.75297
%!      -1.47732   0.78443  -1.04254   0.42240
%!       1.39857  -0.34046   2.28617   0.68089
%!       0.31205   1.50529  -0.75804  -1.22476];
%! X = [1,1,-1;1,1,1;1,1,-1;1,-1,-1]/3;
%! assert (normest1 (A, 3, X), norm (A, 1), 2 * eps);

## test Afcn
%!test
%! A = rand (10);
%! n = length (A);
%! Afcn = @(flag, x) afcn_A (flag, x, A, n);
%! assert (normest1 (Afcn), norm (A, 1), 2 * eps);

## test Afcn with parameters
%!test
%! A = rand (10);
%! assert (normest1 (@afcn_A_P, [], [], A, 3), norm (A ^ 3, 1), 1000 * eps);

## test output
%!test
%! A = reshape (1:16,4,4);
%! [nest, v, w, iter] = normest1 (A);
%! assert (norm (w, 1), nest * norm (v, 1), eps);

## test output
%!test
%! A = rand (100);
%! A(A <= 1/3) = -1;
%! A(A > 1/3 & A <= 2/3) = 0;
%! A(A > 2/3) = 1;
%! [nest, v, w, iter] = normest1 (A, 10);
%! assert (w, A * v, eps);

%!test
%! A = rand (5);
%! nest = normest1 (A, 6);
%! assert (nest, norm (A,1), eps);

%!test
%! A = rand (5);
%! nest = normest1 (A, 2, ones (5, 2) / 5);
%! assert (nest, norm (A,1), eps);

%!test
%! N = 10;
%! A = ones (N);
%! [nm1, v1, w1] = normest1 (A);
%! [nminf, vinf, winf] = normest1 (A', 6);
%! assert (nm1, N, -2*eps);
%! assert (nminf, N, -2*eps);
%! assert (norm (w1, 1), nm1 * norm (v1, 1), -2*eps);
%! assert (norm (winf, 1), nminf * norm (vinf, 1), -2*eps);

%!test
%! N = 5;
%! A = hilb (N);
%! [nm1, v1, w1] = normest1 (A);
%! [nminf, vinf, winf] = normest1 (A', 6);
%! assert (nm1, norm (A, 1), -2*eps);
%! assert (nminf, norm (A, inf), -2*eps);
%! assert (norm (w1, 1), nm1 * norm (v1, 1), -2*eps);
%! assert (norm (winf, 1), nminf * norm (vinf, 1), -2*eps);

## Only likely to be within a factor of 10.
%!test
%! old_state = rand ("state");
%! unwind_protect
%!   rand ("state", 42);  # Initialize to guarantee reproducible results
%!   N = 100;
%!   A = rand (N);
%!   [nm1, v1, w1] = normest1 (A);
%!   [nminf, vinf, winf] = normest1 (A', 6);
%!   assert (nm1, norm (A, 1), -.1);
%!   assert (nminf, norm (A, inf), -.1);
%!   assert (norm (w1, 1), nm1 * norm (v1, 1), -2*eps);
%!   assert (norm (winf, 1), nminf * norm (vinf, 1), -2*eps);
%! unwind_protect_cleanup
%!   rand ("state", old_state);
%! end_unwind_protect

## Check ITER is always a column vector.
%!test
%! [~, ~, ~, it] = normest1 (rand (3), 3);
%! assert (iscolumn (it));
%! [~, ~, ~, it] = normest1 (rand (50), 20);
%! assert (iscolumn (it));

## Test input validation
%!error <Invalid call> normest1 ()
%!error <A must be a square matrix or a function handle> normest1 ({1})
%!error <A must be a square matrix> normest1 ([1 2])
%!error <X0 must have 2 columns> normest1 (magic (5), :, [1])
