########################################################################
##
## Copyright (C) 2006-2023 The Octave Project Developers
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
## @deftypefn  {} {@var{x} =} bicg (@var{A}, @var{b})
## @deftypefnx {} {@var{x} =} bicg (@var{A}, @var{b}, @var{tol})
## @deftypefnx {} {@var{x} =} bicg (@var{A}, @var{b}, @var{tol}, @var{maxit})
## @deftypefnx {} {@var{x} =} bicg (@var{A}, @var{b}, @var{tol}, @var{maxit}, @var{M})
## @deftypefnx {} {@var{x} =} bicg (@var{A}, @var{b}, @var{tol}, @var{maxit}, @var{M1}, @var{M2})
## @deftypefnx {} {@var{x} =} bicg (@var{A}, @var{b}, @var{tol}, @var{maxit}, @var{M}, [], @var{x0})
## @deftypefnx {} {@var{x} =} bicg (@var{A}, @var{b}, @var{tol}, @var{maxit}, @var{M1}, @var{M2}, @var{x0})
## @deftypefnx {} {@var{x} =} bicg (@var{A}, @var{b}, @var{tol}, @var{maxit}, @var{M}, [], @var{x0}, @dots{})
## @deftypefnx {} {@var{x} =} bicg (@var{A}, @var{b}, @var{tol}, @var{maxit}, @var{M1}, @var{M2}, @var{x0}, @dots{})
## @deftypefnx {} {[@var{x}, @var{flag}, @var{relres}, @var{iter}, @var{resvec}] =} bicg (@var{A}, @var{b}, @dots{})
## Solve the linear system of equations @w{@code{@var{A} * @var{x} = @var{b}}}
## by means of the Bi-Conjugate Gradient iterative method.
##
## The input arguments are:
##
## @itemize
##
## @item @var{A} is the matrix of the linear system and it must be square.
## @var{A} can be passed as a matrix, function handle, or inline function
## @code{Afcn} such that @w{@code{Afcn (x, "notransp") = A * x}} and
## @w{@code{Afcn (x, "transp") = A' * x}}.  Additional parameters to
## @code{Afcn} may be passed after @var{x0}.
##
## @item @var{b} is the right-hand side vector.  It must be a column vector
## with the same number of rows as @var{A}.
##
## @item
## @var{tol} is the required relative tolerance for the residual error,
## @w{@code{@var{b} - @var{A} * @var{x}}}.  The iteration stops if
## @w{@code{norm (@var{b} - @var{A} * @var{x})} @leq{}
## @w{@code{@var{tol} * norm (@var{b})}}}.
## If @var{tol} is omitted or empty, then a tolerance of 1e-6 is used.
##
## @item
## @var{maxit} is the maximum allowed number of iterations; if @var{maxit}
## is omitted or empty then a value of 20 is used.
##
## @item
## @var{M1}, @var{M2} are the preconditioners.  The preconditioner @var{M} is
## given as @code{@var{M} = @var{M1} * @var{M2}}.  Both @var{M1} and @var{M2}
## can be passed as a matrix or as a function handle or inline function
## @code{g} such that @w{@code{g (@var{x}, "notransp") = @var{M1} \ @var{x}}}
## or @w{@code{g (@var{x}, "notransp") = @var{M2} \ @var{x}}} and
## @w{@code{g (@var{x}, "transp") = @var{M1}' \ @var{x}}} or
## @w{@code{g (@var{x}, "transp") = @var{M2}' \ @var{x}}}.
## If @var{M1} is omitted or empty, then preconditioning is not applied.
## The preconditioned system is theoretically equivalent to applying the
## @code{bicg} method to the linear system
## @code{inv (@var{M1}) * A * inv (@var{M2}) * @var{y} = inv
## (@var{M1}) * @var{b}} and
## @code{inv (@var{M2'}) * A' * inv (@var{M1'}) * @var{z} =
## inv (@var{M2'}) * @var{b}} and then setting
## @code{@var{x} = inv (@var{M2}) * @var{y}}.
##
## @item
## @var{x0} is the initial guess.  If @var{x0} is omitted or empty then the
## function sets @var{x0} to a zero vector by default.
## @end itemize
##
## Any arguments which follow @var{x0} are treated as parameters, and passed in
## an appropriate manner to any of the functions (@var{Afcn} or @var{Mfcn}) or
## that have been given to @code{bicg}.
##
## The output parameters are:
##
## @itemize
##
## @item
## @var{x} is the computed approximation to the solution of
## @w{@code{@var{A} * @var{x} = @var{b}}}.  If the algorithm did not converge,
## then @var{x} is the iteration which has the minimum residual.
##
## @item
## @var{flag} indicates the exit status:
##
## @itemize
## @item 0: The algorithm converged to within the prescribed tolerance.
##
## @item 1: The algorithm did not converge and it reached the maximum number of
## iterations.
##
## @item 2: The preconditioner matrix is singular.
##
## @item 3: The algorithm stagnated, i.e., the absolute value of the
## difference between the current iteration @var{x} and the previous is less
## than @code{eps * norm (@var{x},2)}.
##
## @item 4: The algorithm could not continue because intermediate values
## became too small or too large for reliable computation.
## @end itemize
##
## @item
## @var{relres} is the ratio of the final residual to its initial value,
## measured in the Euclidean norm.
##
## @item
## @var{iter} is the iteration which @var{x} is computed.
##
## @item
## @var{resvec} is a vector containing the residual at each iteration.
## The total number of iterations performed is given by
## @code{length (@var{resvec}) - 1}.
## @end itemize
##
## Consider a trivial problem with a tridiagonal matrix
##
## @example
## @group
## n = 20;
## A = toeplitz (sparse ([1, 1], [1, 2], [2, 1] * n ^ 2, 1, n)) + ...
##     toeplitz (sparse (1, 2, -1, 1, n) * n / 2, ...
##               sparse (1, 2, 1, 1, n) * n / 2);
## b = A * ones (n, 1);
## restart = 5;
## [M1, M2] = ilu (A);  # in this tridiag case, it corresponds to lu (A)
## M = M1 * M2;
## Afcn = @@(x, string) strcmp (string, "notransp") * (A * x) + ...
##                      strcmp (string, "transp") * (A' * x);
## Mfcn = @@(x, string) strcmp (string, "notransp") * (M \ x) + ...
##                      strcmp (string, "transp") * (M' \ x);
## M1fcn = @@(x, string) strcmp (string, "notransp") * (M1 \ x) + ...
##                      strcmp (string, "transp") * (M1' \ x);
## M2fcn = @@(x, string) strcmp (string, "notransp") * (M2 \ x) + ...
##                      strcmp (string, "transp") * (M2' \ x);
## @end group
## @end example
##
## @sc{Example 1:} simplest usage of @code{bicg}
##
## @example
## x = bicg (A, b)
## @end example
##
## @sc{Example 2:} @code{bicg} with a function that computes
## @code{@var{A}*@var{x}} and @code{@var{A'}*@var{x}}
##
## @example
## x = bicg (Afcn, b, [], n)
## @end example
##
## @sc{Example 3:} @code{bicg} with a preconditioner matrix @var{M}
##
## @example
## x = bicg (A, b, 1e-6, n, M)
## @end example
##
## @sc{Example 4:} @code{bicg} with a function as preconditioner
##
## @example
## x = bicg (Afcn, b, 1e-6, n, Mfcn)
## @end example
##
## @sc{Example 5:} @code{bicg} with preconditioner matrices @var{M1}
## and @var{M2}
##
## @example
## x = bicg (A, b, 1e-6, n, M1, M2)
## @end example
##
## @sc{Example 6:} @code{bicg} with functions as preconditioners
##
## @example
## x = bicg (Afcn, b, 1e-6, n, M1fcn, M2fcn)
## @end example
##
## @sc{Example 7:} @code{bicg} with as input a function requiring an argument
##
## @example
## @group
## function y = Ap (A, x, string, z)
##   ## compute A^z * x or (A^z)' * x
##   y = x;
##   if (strcmp (string, "notransp"))
##     for i = 1:z
##       y = A * y;
##     endfor
##   elseif (strcmp (string, "transp"))
##     for i = 1:z
##       y = A' * y;
##     endfor
##   endif
## endfunction
##
## Apfcn = @@(x, string, p) Ap (A, x, string, p);
## x = bicg (Apfcn, b, [], [], [], [], [], 2);
## @end group
## @end example
##
## Reference:
##
## @nospell{Y. Saad}, @cite{Iterative Methods for Sparse Linear Systems},
## Second edition, 2003, SIAM.
##
## @seealso{bicgstab, cgs, gmres, pcg, qmr, tfqmr}
## @end deftypefn

function [x_min, flag, relres, iter_min, resvec] = ...
         bicg (A, b, tol = [], maxit = [], M1 = [], M2 = [], x0 = [], varargin)

  [Afcn, M1fcn, M2fcn] =  __alltohandles__ (A, b, M1, M2, "bicg");

  [tol, maxit, x0] = __default__input__ ({1e-06, min(rows(b), 20), ...
                                          zeros(rows (b),1)}, tol, maxit, x0);

  if (columns (b) == 2)
    c = b(:,2);
    b = b(:,1);
  else
    c = b;
  endif
  norm_b = norm (b, 2);

  if (norm_b == 0)  # the only (only iff det (A) == 0) solution is x = 0
    if (nargout < 2)
      printf ("The right hand side vector is all zero so bicg\n")
      printf ("returned an all zero solution without iterating.\n")
    endif
    x_min = zeros (numel (b), 1);
    flag = 0;
    relres = 0;
    iter_min = 0;
    resvec = 0;
    return;
  endif

  x = x_min = x_pr = x0;
  iter = iter_min = 0;
  flag = 1;  # Default flag is "maximum number of iterations reached"
  resvec = zeros (maxit + 1, 1);
  r0 = b - Afcn (x, "notransp", varargin{:});  # Residual of the system
  s0 = c - Afcn (x, "transp", varargin{:});    # Res. of the "dual system"
  resvec(1) = norm (r0, 2);

  try
    warning ("error", "Octave:singular-matrix", "local");
    prec_r0 = M1fcn (r0, "notransp", varargin{:});  # r0 preconditioned
    prec_s0 = s0;
    prec_r0 = M2fcn (prec_r0, "notransp", varargin{:});
    prec_s0 = M2fcn (prec_s0, "transp", varargin{:});
    prec_s0 = M1fcn (prec_s0, "transp", varargin{:});  # s0 preconditioned
    p = prec_r0;  # Direction of the system
    q = prec_s0;  # Direction of the "dual system"
  catch
    flag = 2;
  end_try_catch

  while ((flag != 2) && (iter < maxit) && (resvec(iter+1) >= norm_b * tol))
    v = Afcn (p, "notransp", varargin{:});
    prod_qv = q' * v;
    alpha = (s0' * prec_r0);
    if (abs (prod_qv) <= eps * abs (alpha))
      flag = 4;
      break;
    endif
    alpha ./= prod_qv;
    x += alpha * p;
    prod_rs = (s0' * prec_r0);  # Product between r0 and s0
    r0 -= alpha * v;
    s0 -= conj (alpha) * Afcn (q, "transp", varargin{:});
    prec_r0 = M1fcn (r0, "notransp", varargin{:});
    prec_s0 = s0;
    prec_r0 = M2fcn (prec_r0, "notransp", varargin{:});
    beta = s0' * prec_r0;
    if (abs (prod_rs) <= abs (beta))
      flag = 4;
      break;
    endif
    beta ./= prod_rs;
    prec_s0 = M2fcn (prec_s0, "transp", varargin{:});
    prec_s0 = M1fcn (prec_s0, "transp", varargin{:});
    iter += 1;
    resvec(iter+1) = norm (r0);
    if (resvec(iter+1) <= resvec(iter_min+1))
      x_min = x;
      iter_min = iter;
    endif
    if (norm (x - x_pr) <= norm (x) * eps)
      flag = 3;
      break;
    endif
    p = prec_r0 + beta*p;
    q = prec_s0 + conj (beta) * q;
  endwhile
  resvec = resvec(1:iter+1,1);

  if (flag == 2)
    relres = 1;
  else
    relres = resvec(iter_min+1) / norm_b;
  endif

  if ((flag == 1) && (relres <= tol))
    flag = 0;
  endif

  if (nargout < 2)
    switch (flag)
      case 0
        printf ("bicg converged at iteration %i ", iter_min);
        printf ("to a solution with relative residual %e\n", relres);
      case 1
        printf ("bicg stopped at iteration %i ", iter);
        printf ("without converging to the desired tolerance %e\n", tol);
        printf ("because the maximum number of iterations was reached. ");
        printf ("The iterate returned (number %i) has ", iter_min);
        printf ("relative residual %e\n", relres);
      case 2
        printf ("bicg stopped at iteration %i ", iter);
        printf ("without converging to the desired tolerance %e\n", tol);
        printf ("because the preconditioner matrix is singular.\n");
        printf ("The iterate returned (number %i) ", iter_min);
        printf ("has relative residual %e\n", relres);
      case 3
        printf ("bicg stopped at iteration %i ", iter);
        printf ("without converging to the desired tolerance %e\n", tol);
        printf ("because the method stagnated.\n");
        printf ("The iterate returned (number %i) ", iter_min);
        printf ("has relative residual %e\n", relres);
      case 4
        printf ("bicg stopped at iteration %i ", iter);
        printf ("without converging to the desired tolerance %e\n", tol);
        printf ("because the method can't continue.\n");
        printf ("The iterate returned (number %i) ", iter_min);
        printf ("has relative residual %e\n", relres);
    endswitch
  endif

endfunction


%!demo
%! ## simplest use case
%! n = 20;
%! A = toeplitz (sparse ([1, 1], [1, 2], [2, 1] * n ^ 2, 1, n)) + ...
%!     toeplitz (sparse (1, 2, -1, 1, n) * n / 2, ...
%!               sparse (1, 2, 1, 1, n) * n / 2);
%! b = A * ones (n, 1);
%! [M1, M2] = ilu (A + 0.1 * eye (n));
%! M = M1 * M2;
%! x = bicg (A, b, [], n);
%! function y = Ap (A, x, string, z)
%!   ## compute A^z * x or (A^z)' * x
%!   y = x;
%!   if (strcmp (string, "notransp"))
%!     for i = 1:z
%!       y = A * y;
%!   endfor
%!   elseif (strcmp (string, "transp"))
%!     for i = 1:z
%!       y = A' * y;
%!     endfor
%!   endif
%! endfunction
%!
%! Afcn = @(x, string) Ap (A, x, string, 1);
%! x = bicg (Afcn, b, [], n);
%! x = bicg (A, b, 1e-6, n, M);
%! x = bicg (A, b, 1e-6, n, M1, M2);
%! function y = Mfcn (M, x, string)
%!   if (strcmp (string, "notransp"))
%!     y = M \ x;
%!   else
%!     y = M' \ x;
%!   endif
%! endfunction
%!
%! M1fcn = @(x, string) Mfcn (M, x, string);
%! x = bicg (Afcn, b, 1e-6, n, M1fcn);
%! M1fcn = @(x, string) Mfcn (M1, x, string);
%! M2fcn = @(x, string) Mfcn (M2, x, string);
%! x = bicg (Afcn, b, 1e-6, n, M1fcn, M2fcn);
%! Afcn = @(x, string, p) Ap (A, x, string, p);
%! ## Solution of A^2 * x = b
%! x = bicg (Afcn, b, [], 2*n, [], [], [], 2);

%!test
%! ## Check that all type of inputs work
%! A = sparse (toeplitz ([2, 1, 0, 0, 0], [2, -1, 0, 0, 0]));
%! b = A * ones (5, 1);
%! M1 = diag (sqrt (diag (A)));
%! M2 = M1;
%! Afcn = @(z, string) strcmp (string, "notransp") * (A * z) + ...
%!                     strcmp (string, "transp") * (A' * z);
%! M1_fcn = @(z, string) strcmp (string,"notransp") * (M1 \ z) + ...
%!                         strcmp (string, "transp") * (M1' \ z);
%! M2_fcn = @(z, string) strcmp (string, "notransp") * (M2 \ z) + ...
%!                         strcmp (string, "transp") * (M2' \ z);
%! [x, flag] = bicg (A, b);
%! assert (flag, 0);
%! [x, flag] = bicg (A, b, [], [], M1, M2);
%! assert (flag, 0);
%! [x, flag] = bicg (A, b, [], [], M1_fcn, M2_fcn);
%! assert (flag, 0);
%! [x, flag] = bicg (A, b,[], [], M1_fcn, M2);
%! assert (flag, 0);
%! [x, flag] = bicg (A, b,[], [], M1, M2_fcn);
%! assert (flag, 0);
%! [x, flag] = bicg (Afcn, b);
%! assert (flag, 0);
%! [x, flag] = bicg (Afcn, b,[], [], M1, M2);
%! assert (flag, 0);
%! [x, flag] = bicg (Afcn, b,[], [], M1_fcn, M2);
%! assert (flag, 0);
%! [x, flag] = bicg (Afcn, b,[], [], M1, M2_fcn);
%! assert (flag, 0);
%! [x, flag] = bicg (Afcn, b,[], [], M1_fcn, M2_fcn);
%! assert (flag, 0);

%!test
%! n = 100;
%! A = spdiags ([-2*ones(n,1) 4*ones(n,1) -ones(n,1)], -1:1, n, n);
%! b = sum (A, 2);
%! tol = 1e-8;
%! maxit = 15;
%! M1 = spdiags ([ones(n,1)/(-2) ones(n,1)],-1:0, n, n);
%! M2 = spdiags ([4*ones(n,1) -ones(n,1)], 0:1, n, n);
%! [x, flag, relres, iter, resvec] = bicg (A, b, tol, maxit, M1, M2);
%! assert (norm (b - A*x) / norm (b), 0, tol);

%!function y = afcn (x, t, a)
%!  switch (t)
%!    case "notransp"
%!      y = a * x;
%!    case "transp"
%!      y = a' * x;
%!  endswitch
%!endfunction
%!
%!test
%! n = 100;
%! A = spdiags ([-2*ones(n,1) 4*ones(n,1) -ones(n,1)], -1:1, n, n);
%! b = sum (A, 2);
%! tol = 1e-8;
%! maxit = 15;
%! M1 = spdiags ([ones(n,1)/(-2) ones(n,1)],-1:0, n, n);
%! M2 = spdiags ([4*ones(n,1) -ones(n,1)], 0:1, n, n);
%!
%! [x, flag, relres, iter, resvec] = bicg (@(x, t) afcn (x, t, A),
%!                                         b, tol, maxit, M1, M2);
%! assert (x, ones (size (b)), 1e-7);

%!test
%! n = 100;
%! tol = 1e-8;
%! a = sprand (n, n, .1);
%! A = a' * a + 100 * eye (n);
%! b = sum (A, 2);
%! [x, flag, relres, iter, resvec] = bicg (A, b, tol, [], diag (diag (A)));
%! assert (x, ones (size (b)), 1e-7);

%!test
%! ## Check that if the preconditioner is singular, the method doesn't work
%! A = sparse (toeplitz ([2, 1, 0, 0, 0], [2, -1, 0, 0, 0]));
%! b = ones (5,1);
%! M = ones (5);
%! [x, flag] = bicg (A, b, [], [], M);
%! assert (flag, 2);

%!test
%! ## If A singular, the algorithm doesn't work due to division by zero
%! A = ones (5);
%! b = [1:5]';
%! [x, flag] = bicg (A, b);
%! assert (flag, 4);

%!test
%! ## test for a complex linear system
%! A = sparse (toeplitz ([2, 1, 0, 0, 0], [2, -1, 0, 0, 0]) + ...
%!             1i * toeplitz ([2, 1, 0, 0, 0], [2, -1, 0, 0, 0]));
%! b = sum (A, 2);
%! [x, flag] = bicg (A, b);
%! assert (flag, 0);

%!test
%! A = single (1);
%! b = 1;
%! [x, flag] = bicg (A, b);
%! assert (class (x), "single");

%!test
%! A = 1;
%! b = single (1);
%! [x, flag] = bicg (A, b);
%! assert (class (x), "single");

%!test
%! A = single (1);
%! b = single (1);
%! [x, flag] = bicg (A, b);
%! assert (class (x), "single");

%!test
%!function y = Afcn (x, trans)
%!  A = sparse (toeplitz ([2, 1, 0, 0], [2, -1, 0, 0]));
%!  if (strcmp (trans, "notransp"))
%!     y = A * x;
%!  else
%!     y = A' * x;
%!  endif
%!endfunction
%!
%! [x, flag] = bicg ("Afcn", [1; 2; 2; 3]);
%! assert (x, ones (4, 1), 1e-6);

%!test
%! ## unpreconditioned residual
%! A = sparse (toeplitz ([2, 1, 0, 0, 0], [2, -1, 0, 0, 0]));
%! b = sum (A, 2);
%! M = magic (5);
%! [x, flag, relres] = bicg (A, b, [], 2, M);
%! assert (norm (b - A * x) / norm (b), 0, relres + eps);

## Preconditioned technique
%!testif HAVE_UMFPACK
%! A = sparse (toeplitz ([2, 1, 0, 0, 0], [2, -1, 0, 0, 0]));
%! b = sum (A, 2);
%! warning ("off", "Octave:lu:sparse_input", "local");
%! [M1, M2] = lu (A + eye (5));
%! [x, flag] = bicg (A, b, [], 1, M1, M2);
%! ## b has two columns!
%! [y, flag]  = bicg (M1 \ A / M2, [M1 \ b, M2' \ b], [], 1);
%! assert (x, M2 \ y, 8 * eps);

%!test <*63860>
%! ## additional input argument
%! n = 10;
%! s = 1e-3;
%! tol = 1e-6;
%! a = ones (n, 1);
%! a = a / norm (a);
%! y = zeros (n, 1);
%! y(1:2:n) = 1;
%! Amat = @(x, type, s) x + (s * a) * (a' * x);
%! [x, flag] = bicg (Amat, y, tol, [], [], [], y, s);
%! assert (y, Amat (x, [], s), tol * norm (y));
