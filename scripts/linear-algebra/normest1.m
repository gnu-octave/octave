## Copyright (C) 2016 Marco Caliari
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
## @deftypefn  {Function File} {@var{c} =} normest1 (@var{a})
## @deftypefnx {Function File} {@var{c} =} normest1 (@var{a}, @var{t})
## @deftypefnx {Function File} {@var{c} =} normest1 (@var{a}, @var{t}, @var{x0})
## @deftypefnx {Function File} {@var{c} =} normest1 (@var{afun}, @var{t}, @var{x0}, @var{p1}, @var{p2}, @dots{})
## @deftypefnx {Function File} {[@var{c}, @var{v}] =} normest1 (@var{a}, @dots{})
## @deftypefnx {Function File} {[@var{c}, @var{v}, @var{w}] =} normest1 (@var{a}, @dots{})
## @deftypefnx {Function File} {[@var{c}, @var{v}, @var{w}, @var{it}] =} normest1 (@var{a}, @dots{})
## Estimate the 1-norm of the matrix @var{a} using a block algorithm.
##
## For a medium size matrix @var{a}, @code{norm (@var{a}, 1)} should be
## used instead. For a large sparse matrix, when only an estimate of the norm
## is needed, @code{normest1 (@var{a})} might be faster. Moreover, it can be
## used for the estimate of the 1-norm of a linear
## operator @var{a} when matrix-vector products @code{@var{a} * @var{x}} and
## @code{@var{a}' * @var{x}} can be cheaply computed. In this case,
## instead of the matrix @var{a}, a function
## @code{@var{afun} (@var{flag}, @var{x})} can be used. It should return:
##
## @itemize @bullet
## @item
## the dimension @var{n} of @var{a}, if @var{flag} is @qcode{"dim"}
## @item
## true if @var{a} is a real operator, if @var{flag} is @qcode{"real"}
## @item
## the result @code{@var{a} * @var{x}}, if @var{flag} is @qcode{"notransp"}
## @item
## the result @code{@var{a}' * @var{x}}, if @var{flag} is @qcode{"transp"}
## @end itemize
##
## A typical case is @var{a} defined by @code{@var{b} ^ @var{m}},
## in which the result @code{@var{a} * @var{x}} can be computed without
## even forming explicitely @code{@var{b} ^ @var{m}} by:
##
## @example
## @var{y} = @var{x};
## for @var{i} = 1:@var{m}
##   @var{y} = @var{b} * @var{y};
## endfor
## @end example
##
## The parameters @var{p1}, @var{p2}, @dots{} are arguments of
## @code{@var{afun} (@var{flag}, @var{x}, @var{p1}, @var{p2}, @dots{})}.
##
## The default value for @var{t} is 2. The algorithm requires
## matrix-matrix products with sizes @var{n} x @var{n} and
## @var{n} x @var{t}.
##
## The initial matrix @var{x0} should have columns of unit 1-norm.
## The default initial matrix @var{x0} has the first column
## @code{ones (@var{n}, 1) / @var{n}}
## and, if @var{t} >  1, the remaining columns with random elements
## @code{-1 / @var{n}}, @code{1 / @var{n}}, divided by @var{n}.
## Therefore, if consistent results are required, the "state" of the
## random generator should be fixed before invoking @code{normest1}.
##
## On output, @var{c} is the desired estimate, @var{v} and @var{w}
## vectors such that @code{@var{w} = @var{a} * @var{v}}, with
## @code{norm (@var{w}, 1)} = @code{@var{c} * norm (@var{v}, 1)}.
## @var{it} contains in @code{@var{it}(1)} the number of iterations
## (the maximum number is hardcoded to 5) and in  @code{@var{it}(2)}
## the total number of products @code{@var{a} * @var{x}} or
## @code{@var{a}' * @var{x}} performed by the algorithm.
##
## Reference: @nospell{N. J. Higham and F. Tisseur},
## @cite{A block algorithm for matrix 1-norm estimation, with and
## application to 1-norm pseudospectra}, SIAM J. Matrix Anal. Appl.,
## pp. 1185--1201, Vol 21, No. 4, 2000.
##
## @seealso{normest, rand}
## @end deftypefn

## Ideally, we would set t and X to their default values but Matlab
## compatibility would require we set the default even when they are
## empty.
function [est, v, w, k] = normest1 (A, t = [], X = [], varargin)

  if (nargin < 1)
    print_usage ();
  endif

  if (isempty (t))
    t = 2;
  endif

  ## FIXME: t < 0 should print trace information
  if (isnumeric (A) && issquare (A))
    Aisnum = true;
    n = rows (A);
    if ((n <= 4) || (t == n))
      ## compute directly
      [est, idx] = max (sum (abs (A), 1), [] ,2);
      v = zeros (n, 1);
      v(idx) = 1;
      w = A(:, idx);
      ## Matlab incompatible on purpose.  Matlab returns k as a row vector
      ## for this special case, but a column vector in all other cases.
      ## This is obviously a bug in Matlab that we don't reproduce.
      k = [0; 1];
      return
    else
      realm = isreal (A);
    endif
  elseif (is_function_handle (A))
    Aisnum = false;
    n = A ("dim", [], varargin{:});
    realm = A ("real", [], varargin{:});
    Afun = @(x) A ("notransp", x, varargin{:});
    A1fun = @(x) A ("transp", x, varargin{:});
  else
    error ("normest1: A must be a square matrix or a function handle");
  endif

  t = min (t, n);

  if (isempty (X))
    X = [ones(n, 1), sign(2 * rand(n, t - 1) - 1)];
    i = 2;
    imax = min (t, 2 ^ (n - 1));
    ## There are at most 2^(n-1) unparallel columns, see later.
    while (i <= imax)
      if (any (abs (X(:,i)' * X(:,1:i-1)) == n))
        ## column i is parallel to a colum 1:i-1. Change it.
        X(:,i) = sign (2 * rand (n, 1) - 1);
      else
        i++;
      endif
    endwhile
    X /= n;
  elseif (columns (X) < t)
    error ("normest1: X should have %d columns", t);
  endif

  itmax = 5;
  ind_hist = zeros (n, 1);
  est_old = 0;
  ind = zeros (n, 1);
  S = zeros (n, t);
  k = [0; 0];
  conv = false;
  while ((! conv) && (k(1) < itmax))
    k(1)++;
    if (Aisnum)
      Y = A * X;
    else
      Y = Afun (X);
    endif
    k(2)++;
    [est, j] = max (sum (abs (Y), 1), [], 2);
    if ((est > est_old) || (k(1) == 2))
      ind_best = ind(j);
      w = Y(:, j); # there is an error in Algorithm 2.4
    endif
    if ((est <= est_old) && (k(1) >= 2)) # (1) of Algorithm 2.4
      est = est_old;
      break; # while
    endif
    est_old = est;
    Sold = S;
    S = sign (Y);
    S(S==0) = 1;
    possible_break = false;
    if (realm)
      ## test parallel (only real case)
      if (all (any (abs (Sold' * S) == n))) # (2) of Algorithm 2.4
        ## all columns of S parallel to a column of Sold, exit
        possible_break = true;
        conv = true;
      else
        if (t > 1)
          ## at least two columns of S are not parallel
          i = 1;
          imax = min (t, 2 ^ (n - 1));
          while (i <= imax)
            ## The maximum number of parallel columns of length n with entries
            ## {-1,1} is 2^(n-1). Therefore, if the number of columns of S is
            ## greater than 2^(n-1), for sure some of them are parallel to some
            ## columns of Sold. Don't even try to change them (i <= 2^(n-1)).
            ## Now, check if S(:,i) is parallel to any previous column of S
            p = (any (abs (S(:,i)' * S(:,1:i-1)) == n));
            if (p || (any (abs (S(:,i)' * Sold) == n)))
              ## i-th column of S parallel to a previous or to a
              ## column of Sold: change it.
              S(:,i) = sign (2*rand (n, 1)-1);
            else
              i++;
            endif
          endwhile
        endif
      endif
    endif
    if (! possible_break)
      if (Aisnum)
        Z = A' * S;
      else
        Z = A1fun (S); # (3) of Algorithm 2.4
      endif
      k(2)++;
      h = max (abs (Z), [], 2);
      ind = (1:n)';
      if ((k(1) >= 2) && (max (h, [], 1) == h(ind_best))) # (4) of Alg. 2.4
        break; # while
      endif
      [h, ind] = sort (h, "descend");
      if (t > 1)
       if (all (ind_hist(ind(1:t)))) # (5) of Algorithm 2.4
          break; # while
        endif
        ind = ind(! ind_hist(ind));
        ## length(ind) could be less than t, especially if t is not << n.
        ## This is not considered in point (5) of Algorithm 2.4.
        tmax = min (numel (ind), t);
        ind = ind(1:tmax);
      else
        tmax = 1;
      endif
      X = zeros (n, tmax);
      X(sub2ind (size (X), ind(1:tmax), (1:tmax)')) = 1;
      ind_hist(ind(1:tmax)) = 1;
    endif
  endwhile
  v = zeros (n, 1);
  v(ind_best) = 1;
endfunction

%!function z = afun_A (flag, x, A, n)
%! switch flag
%! case {"dim"}
%!   z = n;
%! case {"real"}
%!   z = isreal (A);
%! case {"transp"}
%!   z = A' * x;
%! case {"notransp"}
%!   z = A * x;
%! endswitch
%!endfunction
%!function z = afun_A_P (flag, x, A, m)
%! switch flag
%! case "dim"
%!   z = length (A);
%! case "real"
%!   z = isreal (A);
%! case "transp"
%!   z = x; for i = 1:m, z = A' * z;, endfor
%! case "notransp"
%!   z = x; for i = 1:m, z = A * z;, endfor
%! endswitch
%!endfunction

%!test
%! A = reshape ((1:16)-8, 4, 4);
%! assert (normest1 (A), norm (A, 1), eps)

## test t=1
%!test
%! A = rand (4); # for positive matrices always work
%! assert (normest1 (A, 1), norm (A,1), 2 * eps)

## test t=3
%!test
%! A = [-0.21825   0.16598   0.19388   0.75297
%!      -1.47732   0.78443  -1.04254   0.42240
%!       1.39857  -0.34046   2.28617   0.68089
%!       0.31205   1.50529  -0.75804  -1.22476];
%! X = [1,1,-1;1,1,1;1,1,-1;1,-1,-1]/3;
%! assert (normest1 (A, 3, X), norm (A, 1), 2 * eps)

## test afun
%!test
%! A = rand (10);
%! n = length (A);
%! afun = @(flag, x) afun_A (flag, x, A, n);
%! assert (normest1 (afun), norm (A, 1), 2 * eps)

## test afun with parameters
%!test
%! A = rand (10);
%! assert (normest1 (@afun_A_P, [], [], A, 3), norm (A ^ 3, 1), 1000 * eps)

## test output
%!test
%! A = reshape (1:16,4,4);
%! [c, v, w, it] = normest1 (A);
%! assert (norm (w, 1), c * norm (v, 1), eps)

## test output
%!test
%! A = rand (100);
%! A(A <= 1/3) = -1;
%! A(A > 1/3 & A <= 2/3) = 0;
%! A(A > 2/3) = 1;
%! [c, v, w, it] = normest1 (A, 10);
%! assert (w, A * v, eps)

%!test
%! A = rand (5);
%! c = normest1 (A, 6);
%! assert (c, norm (A,1), eps)

%!test
%! A = rand (5);
%! c = normest1 (A, 2, ones (5, 2) / 5);
%! assert (c, norm (A,1), eps)

%!test
%! N = 10;
%! A = ones (N);
%! [nm1, v1, w1] = normest1 (A);
%! [nminf, vinf, winf] = normest1 (A', 6);
%! assert (nm1, N, -2*eps)
%! assert (nminf, N, -2*eps)
%! assert (norm (w1, 1), nm1 * norm (v1, 1), -2*eps)
%! assert (norm (winf, 1), nminf * norm (vinf, 1), -2*eps)

%!test
%! N = 5;
%! A = hilb (N);
%! [nm1, v1, w1] = normest1 (A);
%! [nminf, vinf, winf] = normest1 (A', 6);
%! assert (nm1, norm (A, 1), -2*eps)
%! assert (nminf, norm (A, inf), -2*eps)
%! assert (norm (w1, 1), nm1 * norm (v1, 1), -2*eps)
%! assert (norm (winf, 1), nminf * norm (vinf, 1), -2*eps)

## Only likely to be within a factor of 10.
%!test
%! old_state = rand ("state");
%! unwind_protect
%!   rand ("state", 42);  # Initialize to guarantee reproducible results
%!   N = 100;
%!   A = rand (N);
%!   [nm1, v1, w1] = normest1 (A);
%!   [nminf, vinf, winf] = normest1 (A', 6);
%!   assert (nm1, norm (A, 1), -.1)
%!   assert (nminf, norm (A, inf), -.1)
%!   assert (norm (w1, 1), nm1 * norm (v1, 1), -2*eps)
%!   assert (norm (winf, 1), nminf * norm (vinf, 1), -2*eps)
%! unwind_protect_cleanup
%!   rand ("state", old_state);
%! end_unwind_protect

## Check IT is always a column vector.
%!test
%! [~, ~, ~, it] = normest1 (rand (3), 3);
%! assert (iscolumn (it))
%! [~, ~, ~, it] = normest1 (rand (50), 20);
%! assert (iscolumn (it))
