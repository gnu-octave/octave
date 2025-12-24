########################################################################
##
## Copyright (C) 2025 The Octave Project Developers
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
## @deftypefn  {} {@var{F} =} funm (@var{A}, @var{fun})
## @deftypefnx {} {@var{F} =} funm (@var{A}, @var{fun}, @var{delta})
## @deftypefnx {} {@var{F} =} funm (@var{A}, @var{fun}, @var{delta}, @var{tol})
## @deftypefnx {} {@var{F} =} funm (@var{A}, @var{fun}, @var{delta}, @var{tol}, @var{prnt})
## @deftypefnx {} {@var{F} =} funm (@var{A}, @var{fun}, @var{delta}, @var{tol}, @var{prnt}, @var{m})
## @deftypefnx {} {[@var{F}, @var{exitflag}] =} funm (@dots{})
## @deftypefnx {} {[@var{F}, @var{exitflag}, @var{output}] =} funm (@dots{})
##
## Evaluate a general matrix function.
##
## @code{funm (@var{A}, @var{fun})} evaluates the function @var{fun} at the
## square matrix @var{A}.  The input @var{fun}(@var{x}, @var{k}) must return
## the @var{k}'th derivative of the function represented by @var{fun} evaluated
## at the vector @var{x}.  The function @var{fun} must have a Taylor series
## representation with an infinite radius of convergence.
##
## The special functions @code{exp}, @code{log}, @code{sin}, @code{cos},
## @code{sinh}, and @code{cosh} can be passed by function handle; for example
## @code{funm (@var{A}, @@cos)}.
##
## For matrix square roots, use @code{sqrtm} instead.  For matrix exponentials,
## either @code{expm} or @code{funm (@var{A}, @@exp)} may be faster or more
## accurate, depending on @var{A}.
##
## Optional inputs:
##
## @table @var
## @item delta
## Tolerance used in determining the blocking (default: 0.1).
##
## @item tol
## Tolerance used in the convergence test for evaluating the Taylor series
## (default: @code{eps}).
##
## @item prnt
## If nonzero, information describing the behavior of the algorithm is printed
## (default: 0).
##
## @item m
## If supplied, defines a custom blocking pattern.
## @end table
##
## Optional outputs:
##
## @table @var
## @item exitflag
## Scalar exit flag that describes the exit condition:
##
## @itemize
## @item 0 --- The algorithm was successful.
##
## @item 1 --- One or more Taylor series evaluations did not converge, but the
## computed value of @var{F} might still be accurate.
## @end itemize
##
## @item output
## Structure with the following fields:
##
## @table @code
## @item terms
## Vector for which @code{output.terms(i)} is the number of Taylor series terms
## used when evaluating the i'th block, or, in the case of the logarithm, the
## number of square roots of matrices of dimension greater than 2.
##
## @item ind
## Cell array for which the (i,j) block of the reordered Schur factor @var{T}
## is @code{@var{T}(output.ind@{i@}, output.ind@{j@})}.
##
## @item ord
## Ordering of the Schur form, as passed to @code{ordschur}.
##
## @item T
## Reordered Schur form.
## @end table
##
## If the Schur form is diagonal then
## @code{output = struct ("terms", ones (n, 1), "ind", @{1:n@}, "ord", [],
## "T", @var{T})}.
## @end table
##
## Example:
##
## @example
## @group
## F = funm (magic (3), @@sin);
## @result{} F =
##    -0.3850    1.0191    0.0162
##     0.6179    0.2168   -0.1844
##     0.4173   -0.5856    0.8185
## @end group
## @end example
##
## The code
##
## @example
## @group
## S = funm (X, @@sin);
## C = funm (X, @@cos);
## @end group
## @end example
##
## @noindent
## will produce the same results (within possible roundoff error) as
##
## @example
## @group
## E = expm (i*X);
## C = real (E);
## S = imag (E);
## @end group
## @end example
##
## @seealso{expm, logm, sqrtm}
## @end deftypefn

function [F, exitflag, output] = funm (A, fun, delta = 0.1, tol = eps, prnt = 0, m = [])

  if (! (issquare (A) && isnumeric (A)))
    error ("funm: A must be a numeric square matrix");
  elseif (! (isa (fun, "function_handle") || ischar (fun)))
    error ("funm: FUN must be a function handle or function name");
  elseif (! (isnumeric (delta) && isscalar (delta)))
    error ("funm: DELTA must be a numeric scalar");
  elseif  (! (isnumeric (tol) && isscalar (tol)))
    error ("funm: TOL must be a numeric scalar");
  endif

  ## Algorithm performs all calculations in double.
  ## Return class single if A is single, otherwise double.

  ## Convert to double
  input_is_single = isa (A, "single");  # Remember input class.
  A = double (A);

  ## Check for special functions
  switch (fun)
    case {@exp, "exp"}
      fun = @fun_exp;
    case {@log, "log"}
      fun = @fun_log;
    case {@sin, "sin"}
      fun = @fun_sin;
    case {@cos, "cos"}
      fun = @fun_cos;
    case {@sinh, "sinh"}
      fun = @fun_sinh;
    case {@cosh, "cosh"}
      fun = @fun_cosh;
  endswitch

  n = rows (A);
  exitflag = 0;  # Initialize exitflag: 0 = success, 1 = convergence issues

  ## Form complex Schur form (if A not already upper triangular).
  T_upper = triu (A);
  if (norm (A - T_upper, "fro") <= 10 * n * eps * norm (A, "fro"))
    U = eye (n);
    T = T_upper;
  else
    [U, T] = schur (A, "complex");
  endif

  ## Handle special case of diagonal T.
  D = diag (T);
  if (norm (T - diag (D), "fro") <= 10 * n * eps * norm (T, "fro"))
    F = U * diag (feval (fun, D)) * U';

    if (nargout > 2)
      output = struct ("terms", ones (n, 1), "ind", {{1:n}},
                       "ord", [], "T", T);
    endif

    ## Return single output class if necessary.
    if (input_is_single)
      F = single (F);
      if (nargout > 2)
        output.T = single (output.T);
      endif
    endif
    return;

  endif

  ## Determine reordering of Schur form into block form.
  if (isempty (m))
    m = blocking (T, delta, abs (prnt) >= 3);
  endif

  if (prnt)
    printf ("Delta (blocking) = %9.2e, tol (TS) = %9.2e\n", delta, tol);
  endif

  [M, ind, n_swaps, ord] = swapping (m);
  if (n_swaps > 0)
    ## Reordering is needed.
    [U, T] = trexc (U, T, M);
  endif

  m = numel (ind);

  ## Calculate F(T).
  F = zeros (n);

  for col = 1:m
    j = ind{col};
    [F(j, j), n_terms] = funm_atom (T(j, j), fun, tol, ...
                                    abs (prnt) * (prnt != 1));
    terms(col) = n_terms;

    ## Check for convergence failure.
    if (n_terms == -1)
      exitflag = 1;
    endif

    for row = col-1 : -1 : 1
      i = ind{row};
      if (numel (i) == 1 && numel (j) == 1)
        ## Scalar case.
        k = i + 1 : j - 1;
        temp =   T(i, j) * (F(i, i) - F(j, j))  ...
               + F(i, k) * T(k, j)              ...
               - T(i, k) * F(k, j);
        F(i, j) = temp / (T(i, i) - T(j, j));
      else
        k = cat (2, ind{row + 1 : col - 1});
        rhs = F(i, i) * T(i, j) - T(i, j) * F(j, j) + ...
              F(i, k) * T(k, j) - T(i, k) * F(k, j);
        F(i, j) = sylvester (T(i, i), -T(j, j), rhs);
      endif
    endfor
  endfor

  F = U * F * U';

  ## As in FUNM:
  if (isreal (A) && norm (imag (F), "fro") <= 10 * n * eps * norm (F, "fro"))
    F = real (F);
  endif

  ## Prepare output structure if requested.
  if (nargout > 2)
    output = struct ("terms", terms, "ind", {ind}, "ord", ord, "T", T);
  endif

  ## Return single output class if necessary.
  if (input_is_single)
    F = single (F);
    if (nargout > 2)
      output.T = single (output.T);
    endif
  endif

endfunction


## FUN_COS
function f = fun_cos (x, k)

  if (nargin < 2 || k == 0)
    f = cos (x);
  else
    g = mod (ceil (k / 2), 2);
    h = mod (k, 2);
    if (h == 1)
      f = sin (x) * (-1)^g;
    else
      f = cos (x) * (-1)^g;
    endif
  endif

endfunction


## FUN_COSH
function f = fun_cosh (x, k)

  if (nargin < 2 || k == 0)
    f = cosh (x);
  elseif (mod (k, 2))
    f = sinh (x);
  else
    f = cosh (x);
  endif

endfunction


## FUN_SINH
function f = fun_sinh (x, k)

  if (nargin < 2 || k == 0)
    f = sinh (x);
  elseif (mod (k, 2))
    f = cosh (x);
  else
    f = sinh (x);
  endif

endfunction


## FUN_EXP
function f = fun_exp (x, k)

  f = exp (x);

endfunction


## FUN_SIN
function f = fun_sin (x, k)

  if (nargin < 2 || k == 0)
    f = sin (x);
  else
    k = k - 1;
    g = mod (ceil (k / 2), 2);
    h = mod (k, 2);
    if (h == 1)
      f = sin (x) * (-1)^g;
    else
      f = cos (x) * (-1)^g;
    endif
  endif

endfunction


## BLOCKING function
## Produce blocking pattern for block Parlett recurrence.
##
## M = blocking (A, DELTA, SHOWPLOT) accepts an upper triangular matrix A and
## produces a blocking pattern, specified by the vector M, for the block
## Parlett recurrence.
##
## M(i) is the index of the block into which A(i,i) should be placed.
##
## DELTA is a gap parameter used to determine the blocking.
##
## Setting SHOWPLOT nonzero produces a plot of the eigenvalues
## that indicates the blocking:
##  - Black circles show a set of 1 eigenvalue.
##  - Blue circles show a set of >1 eigenvalues.
##    The lines connect eigenvalues in the same set.
##
## For A coming from a real matrix it should be possible to take
## advantage of the symmetry about the real axis.  This code does not.
function m = blocking (A, delta, showplot)

  a = diag (A);
  n = numel (a);
  m = zeros (1, n);
  maxM = 0;

  if (showplot)
    clf;
    hold on;
  endif

  ## Pre-compute distance matrix.
  a_col = a(:);
  a_row = a_col.';
  dist_matrix = abs (a_col - a_row);

  for i = 1:n
    if (m(i) == 0)
      m(i) = maxM + 1;             # If a(i) hasn't been assigned to a set
      maxM = maxM + 1;             # then make a new set and assign a(i) to it.
    endif

    for j = i + 1 : n
      if (m(i) != m(j) && dist_matrix(i, j) <= delta)
        if (showplot)
          plot (real ([a(i) a(j)]), imag ([a(i) a(j)]), "o-");
        endif

        if (m(j) == 0)
          m(j) = m(i);             # If a(j) hasn't been assigned to a set,
                                   # assign it to the same set as a(i).
        else
          p = max (m(i), m(j));
          q = min (m(i), m(j));
          m(m == p) = q;           # If a(j) has been assigned to a set
                                   # place all the elements in the set
                                   # containing a(j) into the set
                                   # containing a(i) (or vice versa).

          m(m > p) = m(m > p) - 1; # Tidying up.  As we have deleted set p,
          maxM = maxM - 1;         # we reduce the index of the sets > p by 1.
        endif
      endif
    endfor
  endfor

  if (showplot)
    for i = 1 : max (m)            # for each cluster
      a_ind = a(m == i);           # get eigv in cluster i
      if (numel (a_ind) == 1)      # if only one eigv in cluster
        plot (real (a_ind), imag (a_ind), "ok");  # plot it alone
      endif
    endfor
    hold off;
    grid on;
    box on;
    xlabel ('Re(\lambda)');
    ylabel ('Im(\lambda)');
    title (sprintf ('Eigenvalue clustering (\\delta = %g)', delta));
  endif

endfunction


## SWAPPING function
## Confluent permutation by swapping adjacent elements.
##
## [M, IND, N_SWAPS, ORD] = swapping (M) takes a vector M containing block
## indices and constructs a swapping scheme that produces a confluent
## permutation, with elements ordered by ascending average position.  The
## confluent permutation is obtained by using the LAPACK routine ZTREXC to move
## m(M(i,2)) to m(M(i,1)) by swapping adjacent elements, for i = 1:SIZE(M,1).
##
## The cell array vector IND defines the resulting block form: IND{i} contains
## the indices of the i'th block in the permuted form.
##
## N_SWAPS is the total number of swaps required.

## ORD is the cluster ordering vector suitable for use with ordschur.
function [M, ind, n_swaps, ord] = swapping (m)

  n = numel (m);
  mmax = max (m);
  M = [];
  ind = {};
  h = zeros (1, mmax);
  g = zeros (1, mmax);

  for i = 1 : mmax
    p = find (m == i);
    h(i) = numel (p);
    g(i) = sum (p) / h(i);
  endfor

  [~, y] = sort (g);

  ## Compute ord: eigenvalues in block y(k) get cluster k.
  ord = zeros (1, n);
  for k = 1 : mmax
    ord(m == y(k)) = k;
  endfor

  mdone = 1;

  for i = y
    if (any (m(mdone : mdone + h(i) - 1) != i))
      f = find (m == i);
      g = mdone : mdone + h(i) - 1;
      ff = f(f != g);
      gg = g(f != g);

      ## Create vector v = mdone:f(end) with all elements of f deleted.
      v = mdone - 1 + find (m(mdone : f(end)) != i);

      M(end + 1 : end + numel (gg), :) = [gg' ff'];

      m(g(end) + 1 : f(end)) = m(v);
      m(g) = i * ones (1, h(i));
      ind = cat (2, ind, {mdone : mdone + h(i) - 1});
      mdone = mdone + h(i);
    else
      ind = cat (2, ind, {mdone : mdone + h(i) - 1});
      mdone = mdone + h(i);
    endif
  endfor

  n_swaps = sum (abs (diff (M')));

endfunction


## FUNM_ATOM functions
## Function of triangular matrix with nearly constant diagonal.
##
## [F, N_TERMS] = funm_atom (T, FUN, TOL, PRNT) evaluates function FUN at the
## upper triangular matrix T, where T has nearly constant diagonal.  A Taylor
## series is used.
##
## FUN(X,K) must return the K'th derivative of the function represented by FUN
## evaluated at the vector X.
##
## TOL is a convergence tolerance for the Taylor series.
##
## If PRNT != 0 trace information is printed.
##
## N_TERMS is the number of terms taken in the Taylor series.
##
## N_TERMS = -1 signals lack of convergence.
function [F, n_terms] = funm_atom (T, fun, tol, prnt)

  if (isequal (fun, @fun_log))        # LOG is special case.
    [F, iter] = logm_isst (T, prnt);
    n_terms = iter;                   # iter = -1 signals convergence failure
    return;
  endif

  itmax = 500;

  n = rows (T);
  if (n == 1)
    F = feval (fun, T, 0);
    n_terms = 1;
    return;
  endif

  lambda = sum (diag (T)) / n;
  F = eye (n) * feval (fun, lambda, 0);
  f_deriv_max = zeros (itmax + n - 1, 1);
  N = T - lambda * eye (n);
  mu = norm ((eye (n) - abs (triu (T, 1))) \ ones (n, 1), "fro");

  P = N;
  max_d = 1;

  for k = 1 : itmax
    f = feval (fun, lambda, k);
    F_old = F;
    F = F + P * f;
    rel_diff = norm (F - F_old, "fro") / (tol + norm (F_old, "fro"));
    if (prnt)
      printf ("%3.0f: coef = %5.0e", k, abs (f) / factorial (k));
      printf ("  N^k/k! = %7.1e", norm (P, "fro"));
      printf ("  rel_d = %5.0e", rel_diff);
      printf ("  abs_d = %5.0e\n", norm (F - F_old, "fro"));
    endif
    P = P * N / (k + 1);

    if (rel_diff <= tol)
      ## Approximate the maximum of derivatives in convex set containing
      ## eigenvalues by maximum of derivatives at eigenvalues.
      for j = max_d : k + n - 1
        f_deriv_max(j) = norm (feval (fun, diag (T), j), "fro");
      endfor
      max_d = k + n;
      omega = 0;
      for j = 0 : n - 1
        omega = max (omega, f_deriv_max(k + j) / factorial (j));
      endfor

      trunc = norm (P, "fro") * mu * omega; # norm(F) moved to RHS to avoid / 0
      if (prnt)
        printf ("  [trunc, test] = [%5.0e %5.0e]\n", ...
                                    trunc, tol * norm (F, "fro"));
      endif
      if (prnt == 5)
        trunc = 0;
      endif
      if (trunc <= tol * norm (F, "fro"))  # Force simple stopping test.
        n_terms = k + 1;
        return;
      endif
    endif

  endfor

  n_terms = -1;  # Algorithm did not converge

endfunction


## FUN_LOG function
## Only to be called for plain log evaluation.
function f = fun_log (x)

  f = log (x);

endfunction


## LOGM_ISST function
## Log of triangular matrix by Schur-Pade method with scaling.
##
## X = LOGM_ISST(A, PRNT) computes the logarithm of an upper triangular matrix
## A, for a matrix with no nonpositive real eigenvalues, using the inverse
## scaling and squaring method with Pade approximation.
##
## [X, ITER] = LOGM_ISST(A, PRNT) returns the number ITER of square roots
## computed and prints this information if PRNT is nonzero.
##
## ITER = -1 signals that too many square roots were needed.
##
## References:
## S. H. Cheng, N. J. Higham, C. S. Kenney, and A. J. Laub, Approximating the
##    logarithm of a matrix to specified accuracy, SIAM J. Matrix Anal. Appl.,
##    22(4):1112-1125, 2001.
## N. J. Higham, Evaluating Pade approximants of the matrix logarithm,
##    SIAM J. Matrix Anal. Appl., 22(4):1126-1135, 2001.
function [X, iter] = logm_isst (T, prnt)

  n = rows (T);

  if (any (imag (diag (T)) == 0 & real (diag (T)) <= 0))
    warning ("funm: A must not have nonpositive real eigenvalues");
  endif

  if (n == 1)
    X = log (T);
    iter = 0;
    return;
  endif

  R = T;
  maxlogiter = 50;

  for iter = 0:maxlogiter
    phi = norm (T - eye (n), "fro");
    if (phi <= 0.25)
      if (prnt)
        printf ("LOGM_ISST computed %g square roots.\n", iter);
      endif
      break;
    endif
    if (iter == maxlogiter)
      ## Signal convergence failure instead of error.
      warning ("funm: too many square roots in LOGM_ISST");
      X = 2 ^ (iter) * logm_pf (T - eye (n), 8);
      iter = -1;    # Signal failure
      return;
    endif

    ## Compute upper triangular square root R of T, a column at a time.
    for j = 1 : n
      R(j, j) = sqrt (T(j, j));
      for i = j - 1 : -1 : 1
        R(i, j) = (T(i, j) - R(i, i + 1 : j - 1) * R(i + 1 : j - 1, j)) / ...
                  (R(i, i) + R(j, j));
      endfor
    endfor
    T = R;
  endfor

  X = 2 ^ (iter) * logm_pf (T - eye (n), 8);

endfunction


## LOGM_PF
## Pade approximation to matrix log by partial fraction expansion.
##
## S = LOGM_PF(A, M) approximates LOG(I+A) using M-point Gauss-Legendre
## quadrature.
function S = logm_pf (A, m)

  [nodes, wts] = gauss_legendre (m);
  ## Convert from [-1,1] to [0,1].
  nodes = (nodes + 1) / 2;
  wts = wts / 2;

  n = rows (A);
  S = zeros (n);

  for j = 1 : m
    S = S + wts(j) * (A / (eye (n) + nodes(j) * A));
  endfor

endfunction


## GAUSS_LEGENDRE function
## Nodes and weights for Gauss-Legendre quadrature.
##
## [X, W] = GAUSS_LEGENDRE(N) returns the N-point Gauss-Legendre nodes X and
## weights W for integration on [-1, 1].
##
## Reference:
## G. H. Golub and J. H. Welsch, Calculation of Gauss quadrature rules,
## Math. Comp., 23(106):221-230, 1969.
function [x, w] = gauss_legendre (n)

  i = 1 : n - 1;
  v = i ./ sqrt ((2 * i) .^ 2 - 1);
  [V, D] = eig (diag (v, -1) + diag (v, 1));
  x = diag (D);
  w = 2 * (V(1, :)' .^ 2);

endfunction


%!demo
%! ## Create a non-normal matrix with clustered complex eigenvalues.
%! ## Use non-symmetric 2x2 blocks to ensure non-normality.
%! B1 = [  1.1  3;  -2   1.1 ];  # eigenvalues: 1.1 +/- sqrt(6)i
%! B2 = [  1.4  2;  -2   1.4 ];  # eigenvalues: 1.5 +/- 2i (close, will cluster)
%! B3 = [ -0.3  1;   0   0.3 ];  # eigenvalues: -0.3, 0.3 (real1)
%! B4 = [  2.4  2;  -1   2.4 ];  # eigenvalues: 2.4 +/- sqrt(2)i
%! A = blkdiag (B1, B2, B3, B4);
%! ## Add some noise to make A less structured, but with the same eigenvalues.
%! Q = orth (randn (size (A)));
%! A = Q' * A * Q;
%! [F, exitflag] = funm (A, @sin, 0.8, eps, 4);  # prnt=4 triggers plot
%! disp ("Eigenvalues of A:");
%! disp (eig (A));

## Basic function tests
%!assert (funm (10, @log), log (10))
%!assert (funm ([1 2;3 4], @sin), [-0.4656 -0.1484;-0.2226 -0.6882], 4e-5)
%!assert (funm ([1 2;3 4], @cos), [0.8554 -0.1109;-0.1663 0.6891], 3e-5)
%!assert (funm ([1 2;3 4], @exp), [51.9690 74.7366;112.1048 164.0738], 5e-5)
%!assert (funm ([1 2;3 4], @logm), ...
%!        [-0.35044+2.39112i 0.92935-1.09376i;
%!         1.39403-1.64064i 1.04359+0.75047i], 1e-5)
%!assert (funm ([1 2;3 4], @sinh), [25.4317 37.6201;56.4301 81.8618], 4e-5)
%!assert (funm ([1 2;3 4], @cosh), [26.5372 37.1165;55.6747 82.2120], 5e-5)

## Test output format [F, exitflag]
%!test
%! [F, exitflag] = funm ([1 2;3 4], @sin);
%! assert (F, [-0.4656 -0.1484;-0.2226 -0.6882], 4e-5);
%! assert (exitflag, 0);

## Test output format [F, exitflag, output]
%!test
%! [F, exitflag, output] = funm ([1 2;3 4], @cos);
%! assert (F, [0.8554 -0.1109;-0.1663 0.6891], 3e-5);
%! assert (exitflag, 0);
%! assert (isstruct (output));
%! assert (isfield (output, "terms"));
%! assert (isfield (output, "ind"));
%! assert (isfield (output, "ord"));
%! assert (isfield (output, "T"));

## Test that sin/cos via funm matches expm approach
%!test
%! X = [1 2;3 4];
%! S = funm (X, @sin);
%! C = funm (X, @cos);
%! E = expm (i*X);
%! assert (S, imag (E), 4*eps);
%! assert (C, real (E), 4*eps);
%! I = eye (size (X));
%! assert (S*S + C*C, I, 5*eps);

## Test input/output class handling

%!test
%! ## Single precision input should return single precision output
%! A = single (magic (5));
%! F = funm (A, @exp);
%! assert (isa (F, "single"));

%!test
%! ## Double precision input should return double precision output
%! A = magic (5);
%! F = funm (A, @sin);
%! assert (isa (F, "double"));

%!test
%! ## Single precision result should be accurate
%! A = single (magic (4));
%! F_single = funm (A, @exp);
%! F_double = funm (double (A), @exp);
%! ## Should match within single precision tolerance
%! assert (F_single, single (F_double), eps ("single"));

%!test
%! ## Integer input should return double output
%! A = uint8 (magic (4));
%! F = funm (A, @exp);
%! assert (isa (F, "double"));
%! ## Result should match double computation
%! F_ref = funm (double (A), @exp);
%! assert (F, F_ref, eps);

%!test
%! ## Int32 input should return double output
%! A = int32 (magic (3));
%! F = funm (A, @sin);
%! assert (isa (F, "double"));

%!test
%! ## Single complex should remain single
%! A = single (complex (randn (5), randn (5)));
%! F = funm (A, @exp);
%! assert (isa (F, "single"));

%!test
%! ## Output structure T field should match input precision (single)
%! A = single (magic (4));
%! [F, exitflag, output] = funm (A, @exp);
%! assert (isa (output.T, "single"));

%!test
%! ## Output structure T field should match input precision (double)
%! A = magic (4);
%! [F, exitflag, output] = funm (A, @exp);
%! assert (isa (output.T, "double"));

## Additional edge cases

%!test
%! ## Empty matrix
%! A = [];
%! F = funm (A, @exp);
%! assert (F, []);

%!test
%! ## 1x1 matrix (scalar)
%! A = 2;
%! F = funm (A, @exp);
%! assert (F, exp (2), eps);

%!test
%! ## 1x1 single precision
%! A = single (2);
%! F = funm (A, @exp);
%! assert (isa (F, "single"));
%! assert (F, single (exp (2)), eps ("single"));

%!test
%! ## Diagonal matrix (already upper triangular)
%! A = diag ([1, 2, 3, 4, 5]);
%! F = funm (A, @exp);
%! assert (diag (F), exp ([1; 2; 3; 4; 5]), eps);

%!test
%! ## Diagonal single precision
%! A = single (diag ([1, 2, 3]));
%! F = funm (A, @sin);
%! assert (isa (F, "single"));
%! assert (diag (F), single (sin ([1; 2; 3])), eps ("single"));

%!test
%! ## Test sylvester code path: matrix with clustered eigenvalues
%! D = diag ([1, 1.05, 3, 3.05]);
%! [Q, ~] = qr (randn (4));
%! A = Q * D * Q';
%! F = funm (A, @exp);
%! assert (F, expm (A), 1e-10);

## Test error handling for invalid inputs
%!error <A must be a numeric square matrix> funm ("not a matrix", @exp)
%!error <A must be a numeric square matrix> funm ({1, 2; 3, 4}, @exp)
%!error <A must be a numeric square matrix> funm (ones (5, 2), @exp)
%!error <A must be a numeric square matrix> funm ([1 2 3; 0 4 5], @exp)
%!error <FUN must be a function handle or function name> funm (eye (3), 123)
%!error <FUN must be a function handle or function name> funm (eye (3), [1 2 3])
%!error <DELTA must be a numeric scalar> funm (eye (3), @exp, "A")
%!error <DELTA must be a numeric scalar> funm (eye (3), @exp, [1, 2])
%!error <TOL must be a numeric scalar> funm (eye (3), @exp, eps, "A")
%!error <TOL must be a numeric scalar> funm (eye (3), @exp, eps, [1, 2])
