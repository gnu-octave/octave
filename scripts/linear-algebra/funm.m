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
## Redistribution and use in source and binary forms, with or without
## modification, are permitted provided that the following conditions are met:
##
## * Redistributions of source code must retain the above copyright notice,
##   this list of conditions and the following disclaimer.
## * Redistributions in binary form must reproduce the above copyright notice,
##   this list of conditions and the following disclaimer in the documentation
##   and/or other materials provided with the distribution.
##
## THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
## AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
## IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
## ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
## LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
## CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
## SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
## INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
## CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
## ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
## POSSIBILITY OF SUCH DAMAGE.
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
## square matrix @var{A}.  @var{fun}(@var{x}, @var{k}) must return the
## @var{k}'th derivative of the function represented by @var{fun} evaluated
## at the vector @var{x}.
##
## The standard MATLAB/Octave functions @code{cos}, @code{sin}, @code{exp},
## @code{log}, @code{cosh}, and @code{sinh} can be passed as @var{fun}, i.e.,
## @code{funm (@var{A}, @@cos)}, @code{funm (@var{A}, @@sin)}, etc.
##
## For matrix square roots use @code{sqrtm} instead.  For matrix exponentials,
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
## @item 1 --- One or more Taylor series evaluations did not converge, but
## the computed value of @var{F} might still be accurate.
## @end itemize
##
## @item output
## Structure with the following fields:
##
## @table @code
## @item terms
## Vector for which @code{output.terms(i)} is the number of Taylor series
## terms used when evaluating the i'th block, or, in the case of the
## logarithm, the number of square roots of matrices of dimension greater
## than 2.
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

function [F, exitflag, output] = funm (A, fun, delta=0.1, tol=eps, prnt=0, m=[])

  if (! (issquare (A) && isnumeric (A)))
    error ("funm: A must be a numeric square matrix");
  elseif (! (isa (fun, "function_handle") || ischar (fun)))
    error ("funm: FUN must be a function handle or function name");
  elseif (! (isnumeric (delta) && isscalar (delta)))
    error ("funm: DELTA must be a numeric scalar");
  elseif  (! (isnumeric (tol) && isscalar (tol)))
    error ("funm: TOL must be a numeric scalar");
  endif

  ## We do all calculations in double
  ## If input A is single --> return single
  ## Integer --> return double

  ## Remember input class
  input_is_single = isa (A, "single");

  ## Convert to double
  if (! isa (A, "double"))
    A = double (A);
  endif

  switch fun
    case {@cos, "cos"}
      fun = @fun_cos;
    case {@sin, "sin"}
      fun = @fun_sin;
    case {@exp, "exp"}
      fun = @fun_exp;
    case {@cosh, "cosh"}
      fun = @fun_cosh;
    case {@sinh, "sinh"}
      fun = @fun_sinh;
    case {@log, "log"}
      fun = @fun_log;
    otherwise
  endswitch

  n = length (A);
  exitflag = 0;    # Initialize exitflag: 0 = success, 1 = convergence issues

  ## First form complex Schur form (if A not already upper triangular).
  if (isequal (A, triu (A)))
    T = A;
    U = eye (n);
  else
    [U, T] = schur (A, "complex");
  endif

  if (isequal (T, tril (T)))             # Handle special case of diagonal T.
    F = U * diag (feval (fun, diag (T))) * U';

    if (nargout > 2)
      output = struct ("terms", ones (n, 1), "ind", {{1:n}}, "ord", [], "T", T);
    endif

    ## Only convert back to single, not to integer/logical
    if (input_is_single)
      F = single (F);
      if (nargout > 2)
        output.T = single (output.T);
      endif
    endif
    return

  endif

  ## Determine reordering of Schur form into block form.
  if (isempty (m))
    m = blocking (T, delta, abs (prnt) >= 3);
  endif

  if (prnt)
    printf ("Delta (blocking) = %9.2e, tol (TS) = %9.2e\n", delta, tol);
  endif

  [M, ind, n_swaps, ord] = swapping (m);
  if (n_swaps > 0)                        # If reordering is needed...
    [U, T] = trexc (U, T, M);
  endif

  m = length (ind);

  ## Calculate F(T)
  F = zeros (n);

  for col = 1:m
    j = ind{col};
    [F(j, j), n_terms] = funm_atom (T (j, j), fun, tol, abs (prnt) * (prnt != 1));
    terms (col) = n_terms;

    ## Check for convergence failure
    if (n_terms == -1)
      exitflag = 1;
    endif

    for row = col-1 : -1 : 1
      i = ind{row};
      if (length (i) == 1 && length (j) == 1)
         ## Scalar case.
        k = i + 1 : j - 1;
        temp = T(i, j) * (F(i, i) - F(j, j)) + F(i, k) * T(k, j) - T(i, k) * F(k, j);
        F (i, j) = temp / (T(i, i) - T(j, j));
      else
        k = cat (2, ind{row + 1 : col - 1});
        rhs = F(i, i) * T(i, j) - T(i, j) * F(j, j) + ...
              F(i, k) * T(k, j) - T(i, k) * F (k, j);
        F (i, j) = sylv_tri (T(i, i), -T(j, j), rhs);
      endif
    endfor
  endfor

  F = U * F * U';

  ## As in FUNM:
  if (isreal (A) && norm (imag (F), 1) <= 10 * n * eps * norm (F, 1))
    F = real (F);
  endif

  ## Prepare output structure if requested
  if (nargout > 2)
    output = struct ("terms", terms, "ind", {ind}, "ord", ord, "T", T);
  endif

  ## Only convert back to single, not to integer/logical
  if (input_is_single)
    F = single (F);
    if (nargout > 2)
      output.T = single (output.T);
    endif
  endif

endfunction ## FUNM


##FUN_COS
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


##FUN_COSH
function f = fun_cosh (x, k)

  if (mod (k, 2))
    f = sinh(x);
  else
    f = cosh(x);
  endif

endfunction


##FUN_SINH
function f = fun_sinh (x, k)

  if (mod (k, 2))
   f = cosh (x);
  else
   f = sinh (x);
  endif

endfunction


##FUN_EXP
function f = fun_exp (x, k)

  f = exp (x);

endfunction


##FUN_SIN
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


function m = blocking (A, delta, showplot)
  ##BLOCKING  Produce blocking pattern for block Parlett recurrence.
  ##          M = BLOCKING(A, DELTA, SHOWPLOT) accepts an upper triangular matrix
  ##          A and produces a blocking pattern, specified by the vector M,
  ##          for the block Parlett recurrence.
  ##          M(i) is the index of the block into which A(i,i) should be placed.
  ##          DELTA is a gap parameter (default 0.1) used to determine the
  ##          blocking.
  ##          Setting SHOWPLOT nonzero produces a plot of the eigenvalues
  ##          that indicates the blocking:
  ##           - Black circles show a set of 1 eigenvalue.
  ##           - Blue circles show a set of >1 eigenvalues.
  ##             The lines connect eigenvalues in the same set.
  ##             Red squares show the mean of each set.
  ##
  ##         For A coming from a real matrix it should be posible to take
  ##         advantage of the symmetry about the real axis.  This code does not.

  a = diag (A);
  n = length (a);
  m = zeros (1, n);
  maxM = 0;

  if (nargin < 2 || isempty (delta))
    delta = 0.1;
  endif

  if (nargin < 3 )
    showplot = false;
  endif

  if (showplot)
    clf;
    hold on;
  endif

  ## Pre-compute distance matrix
  a_col = a(:);
  a_row = transpose (a_col);
  dist_matrix = abs (a_col - a_row);

  for i = 1:n
    if (m(i) == 0)
      m(i) = maxM + 1;                  # If a(i) hasn`t been assigned to a set
      maxM = maxM + 1;                  # then make a new set and assign a(i) to it.
    endif

    for j = i + 1 : n
    if (m(i) != m(j) && dist_matrix(i, j) <= delta)
        if (showplot)
          plot (real ([a(i) a(j)]), imag ([a(i) a(j)]), "o-");
        endif

        if (m(j) == 0)
          m(j) = m(i);                # If a(j) hasn`t been assigned to a
                                      # set, assign it to the same set as a(i).
        else
          p = max (m(i), m(j));
          q = min (m(i), m(j));
          m(m == p) = q;              # If a(j) has been assigned to a set
                                      # place all the elements in the set
                                      # containing a(j) into the set
                                      # containing a(i) (or vice versa).
          m(m > p) = m(m > p) - 1;
          maxM = maxM - 1;
                                      # Tidying up. As we have deleted set
                                      # p we reduce the index of the sets
                                      # > p by 1.
        endif
      endif
    endfor
  endfor

  if (showplot)
    for i = 1 : max (m)
      a_ind = a(m == i);
      if (length (a_ind) == 1)
        plot (real (a_ind), imag (a_ind), "ok");
##    else
##      plot (real (mean (a_ind)), imag (mean (a_ind)), "sr");
      endif
    endfor
    grid on;
    hold off;
    box on;
  endif

endfunction


function [M, ind, n_swaps, ord] = swapping (m)
  ##SWAPPING  Confluent permutation by swapping adjacent elements.
  ##         [ISWAP,IND,N_SWAPS,ORD] = SWAPPING(M) takes a vector M containing
  ##         and constructs a swapping scheme that produces
  ##         a confluent permutation, with elements ordered by ascending
  ##         average position. The confluent permutation is obtained by using
  ##         the LAPACK routine ZTREX to move m(ISWAP(i,2)) to m(ISWAP(i,1))
  ##         by swapping adjacent elements, for i = 1:SIZE(M,1).
  ##         The cell array vector IND defines the resulting block form:
  ##         IND{i} contains the indices of the i'th block in the permuted form.
  ##         N_SWAPS is the total number of swaps required.
  ##         ORD is the cluster ordering vector suitable for use with ordschur.

  n = length (m);
  mmax = max (m);
  M   = [];
  ind = {};
  h   = zeros (1, mmax);
  g   = zeros (1, mmax);

  for i = 1 : mmax
    p = find (m == i);
    h(i) = length (p);
    g(i) = sum (p) / h(i);
  endfor

  [x, y] = sort (g);

  ## Compute ord: eigenvalues in block y(k) get cluster k
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

      M(end + 1 : end + length (gg), :) = [gg' ff'];

      m(g(end) + 1 : f (end)) = m(v);
      m(g) = i * ones (1, h(i));
      ind = cat (2, ind, {mdone : mdone + h(i) - 1} );
      mdone = mdone + h(i);
    else
      ind = cat (2, ind, {mdone : mdone + h(i) - 1} );
      mdone = mdone + h(i);
    endif
  endfor

  n_swaps = sum (abs (diff (M')));

endfunction


function [F, n_terms] = funm_atom (T, fun, tol, prnt)
  ##FUNM_ATOM  Function of triangular matrix with nearly constant diagonal.
  ##          [F, N_TERMS] = FUNM_ATOM(T, FUN, TOL, PRNT) evaluates function
  ##          FUN at the upper triangular matrix T, where T has nearly constant
  ##          diagonal.  A Taylor series is used.
  ##          FUN(X,K) must return the K'th derivative of
  ##          the function represented by FUN evaluated at the vector X.
  ##          TOL is a convergence tolerance for the Taylor series,
  ##          defaulting to EPS.
  ##          If PRNT != 0 trace information is printed.
  ##          N_TERMS is the number of terms taken in the Taylor series.
  ##          N_TERMS  = -1 signals lack of convergence.

  if (nargin < 3 || isempty (tol))
    tol = eps;
  endif
  if (nargin < 4)
    prnt = 0;
  endif

  if (isequal (fun, @fun_log))          # LOG is special case.
    [F, iter]  = logm_isst (T, prnt);
    n_terms = iter;    # iter = -1 signals convergence failure
    return
  endif

  itmax = 500;

  n = length (T);
  if (n == 1)
    F = feval (fun, T, 0);
    n_terms = 1;
    return
  endif

  lambda = sum (diag (T)) / n;
  F = eye (n) * feval (fun, lambda, 0);
  f_deriv_max = zeros (itmax + n - 1, 1);
  N = T - lambda * eye (n);
  mu = norm ((eye (n) - abs (triu (T, 1))) \ ones (n, 1), inf);

  P = N;
  max_d = 1;

  for k = 1 : itmax
    f = feval (fun, lambda, k);
    F_old = F;
    F = F + P * f;
    rel_diff = norm (F - F_old, inf) / (tol + norm (F_old, inf));
    if (prnt)
      printf ("%3.0f: coef = %5.0e", k, abs (f) / factorial (k));
      printf ("  N^k/k! = %7.1e", norm (P, inf));
      printf ("  rel_d = %5.0e", rel_diff);
      printf ("  abs_d = %5.0e", norm (F - F_old, inf));
    endif
    P = P * N / (k + 1);

    if (rel_diff <= tol)
      ## Approximate the maximum of derivatives in convex set containing
      ## eigenvalues by maximum of derivatives at eigenvalues.
      for j = max_d : k + n - 1
          f_deriv_max(j) = norm (feval (fun, diag (T), j), inf);
      endfor
      max_d = k + n;
      omega = 0;
      for j = 0 : n - 1
        omega = max (omega, f_deriv_max(k + j) / factorial (j));
      endfor

      trunc = norm (P, inf) * mu * omega; # norm(F) moved to RHS to avoid / 0.
      if (prnt)
        printf ("  [trunc, test] = [%5.0e %5.0e]", trunc, tol * norm (F, inf));
      endif
      if (prnt == 5)
        trunc = 0;
      endif                               # Force simple stopping test.
      if (trunc <= tol * norm (F, inf))
        n_terms = k + 1;
        if (prnt)
          printf ("\n");
        endif
        return
      endif
    endif

    if (prnt)
      printf ("\n");
    endif

  endfor
  n_terms = -1;

endfunction


##FUN_LOG
##Only to be called for plain log evaluation.
function f = fun_log (x)

  f = log (x);

endfunction


function [X, iter] = logm_isst (T, prnt)
  ##LOGM_ISST   Log of triangular matrix by Schur-Pade method with scaling.
  ##        X = LOGM_ISST(A) computes the logarithm of an upper triangular
  ##        matrix A, for a matrix with no nonpositive real eigenvalues,
  ##        using the inverse scaling and squaring method with Pade
  ##        approximation.  TOL is an error tolerance.
  ##        [X, ITER] = LOGM_ISST(A, PRNT) returns the number ITER of square
  ##        roots computed and prints this information if PRNT is nonzero.
  ##        ITER = -1 signals that too many square roots were needed.
  ##
  ## References:
  ##S. H. Cheng, N. J. Higham, C. S. Kenney, and A. J. Laub, Approximating the
  ##   logarithm of a matrix to specified accuracy, SIAM J. Matrix Anal. Appl.,
  ##   22(4):1112-1125, 2001.
  ##N. J. Higham, Evaluating Pade approximants of the matrix logarithm,
  ##   SIAM J. Matrix Anal. Appl., 22(4):1126-1135, 2001.

  if (nargin < 2)
    prnt = 0;
  endif
  n = length (T);

  if (any (imag (diag (T)) == 0 & real (diag (T)) <= 0))
    warning ("funm: A must not have nonpositive real eigenvalues");
  endif

  if (n == 1)
    X = log (T);
    iter = 0;
    return
  endif

  R = T;
  maxlogiter = 50;

  for iter = 0:maxlogiter
    phi = norm (T - eye (n), "fro");
    if (phi <= 0.25)
      if (prnt)
        printf ("LOGM_ISST computed %g square roots. \n", iter);
      endif
      break
    endif
    if (iter == maxlogiter)
      ## Signal convergence failure instead of error
      warning ("funm: too many square roots in LOGM_ISST");
      X = 2 ^ (iter) * logm_pf (T - eye (n), 8);
      iter = -1;    # Signal failure
      return
    endif

    ## Compute upper triangular square root R of T, a column at a time.
    for j = 1 : n
      R(j, j) = sqrt (T(j, j));
      for i = j - 1 : -1 : 1
        R(i, j) = (T(i, j) - R(i, i + 1 : j - 1) * R(i + 1 : j - 1 , j)) / ...
                  (R(i, i) + R(j, j));
      endfor
    endfor
    T = R;
  endfor

  X = 2 ^ (iter) * logm_pf (T - eye (n), 8);

endfunction


##LOGM_PF   Pade approximation to matrix log by partial fraction expansion.
##         Y = LOGM_PF(A,m) approximates LOG(I+A).
function S = logm_pf (A, m)

  [nodes, wts] = gauss_legendre (m);
  ## Convert from [-1,1] to [0,1].
  nodes = (nodes + 1) / 2;
  wts = wts / 2;

  n = length (A);
  S = zeros (n);

  for j = 1 : m
    S = S + wts(j) * (A / (eye (n) + nodes(j) * A));
  endfor

endfunction


##GAUSS_LEGENDRE  Nodes and weights for Gauss-Legendre quadrature.
function [x, w] = gauss_legendre (n)
  ## Reference:
  ## G. H. Golub and J. H. Welsch, Calculation of Gauss quadrature
  ## rules, Math. Comp., 23(106):221-230, 1969.

  i = 1 : n - 1;
  v = i ./ sqrt ((2 * i) .^ 2 - 1);
  [V, D] = eig (diag (v, -1) + diag (v, 1));
  x = diag (D);
  w = 2 * (V(1, :)' .^ 2);

endfunction


##SYLV_TRI    Solves triangular Sylvester equation.
function X = sylv_tri (T, U, B)
  ##       x = SYLV_TRI(T,U,B) solves the Sylvester equation
  ##       T*X + X*U = B, where T and U are square upper triangular matrices.

  m = length (T);
  n = length (U);
  X = zeros (m, n);
  ##Forward substitution.
  for i = 1 : n
    X(:, i) = (T + U(i, i) * eye (m)) \ ...
              (B(:, i) - X(:, 1 : i - 1) * U(1 : i - 1, i));
  endfor

endfunction


#####################################################
#####################################################
%!assert (funm (10, @log), log (10))
%!assert (funm ([1 2;3 4], @sin), [-0.4656   -0.1484;-0.2226   -0.6882], 4e-5)
%!assert (funm ([1 2;3 4], @cos), [ 0.8554   -0.1109;-0.1663    0.6891], 3e-5)
%!assert (funm ([1 2;3 4], @exp), [51.9690   74.7366;112.1048  164.0738], 5e-5)
%!assert (funm ([1 2;3 4], @logm), [ -0.35044 + 2.39112i   0.92935 - 1.09376i; 1.39403 - 1.64064i   1.04359 + 0.75047i], 1e-5)
%!assert (funm ([1 2;3 4], @sinh), [25.4317   37.6201;56.4301   81.8618], 4e-5)
%!assert (funm ([1 2;3 4], @cosh), [26.5372   37.1165;55.6747   82.2120], 5e-5)

## Test output format [F, exitflag]
%!test
%! [F, exitflag] = funm ([1 2;3 4], @sin);
%! assert (F, [-0.4656   -0.1484;-0.2226   -0.6882], 4e-5);
%! assert (exitflag, 0);

## Test  output format [F, exitflag, output]
%!test
%! [F, exitflag, output] = funm ([1 2;3 4], @cos);
%! assert (F, [ 0.8554   -0.1109;-0.1663    0.6891], 3e-5);
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


#####################################################
## Test input/output class handling
#####################################################

%!test
%! ## Single precision input should return single precision output
%! A = single (magic (5));
%! F = funm (A, @exp);
%! assert (isa (F, "single"));
%! assert (isa (A, "single"));  # Input unchanged

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
%! assert (F_single, single (F_double), eps("single"));

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
%! A = single (randn (5) + 1i*randn (5));
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

#####################################################
## Test error handling for invalid inputs
#####################################################

## All invalid inputs should error with exact message
%!error<funm: A must be a numeric square matrix> funm ("not a matrix", @exp)
%!error<funm: A must be a numeric square matrix> funm ({1, 2; 3, 4}, @exp)
%!error<funm: A must be a numeric square matrix> funm (ones (5, 2), @exp)

%!test
%! ## Non-square upper triangular (edge case where schur is skipped)
%! A = [1 2 3; 0 4 5];
%! fail ("funm (A, @exp)", "funm: A must be a numeric square matrix");

%!error<funm: FUN must be a function handle or function name> funm (eye (3), 123)
%!error<funm: FUN must be a function handle or function name> funm (eye (3), [1 2 3])

#####################################################
## Additional edge cases
#####################################################

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
%! assert (F, single (exp (2)), eps("single"));

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
%! assert (diag (F), single (sin ([1; 2; 3])), eps("single"));

#####################################################
## Demos
#####################################################

%!demo
%! # Create some ugly matrix
%! B1 = [  1  -2;   2   1 ];    # eigenvalues:  1 +/- 2i
%! B2 = [ -1  -1.5; 1.5 -1 ];   # eigenvalues: -1 +/- 1.5i
%! B3 = [  0.5 -3;  3  0.5 ];   # eigenvalues:  0.5 +/- 3i
%! B4 = [  2  -0.7; 0.7  2 ];   # eigenvalues:  2 +/- 0.7i
%! A = blkdiag (B1, B2, B3, B4);
%! # Add some noise to make A less structured but with the same eigenvalues
%! Q = orth (randn (size (A)));
%! A = Q' * A * Q;
%! [F, exitflag] = funm (A, @sin, 0.1, eps, 4) # "4" means "make a plot"
%! disp ("Eigenvalues of A:")
%! disp (eig (A))

