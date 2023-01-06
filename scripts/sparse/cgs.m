########################################################################
##
## Copyright (C) 2008-2023 The Octave Project Developers
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
## @deftypefn  {} {@var{x} =} cgs (@var{A}, @var{b}, @var{tol}, @var{maxit}, @var{M1}, @var{M2}, @var{x0}, @dots{})
## @deftypefnx {} {@var{x} =} cgs (@var{A}, @var{b}, @var{tol}, @var{maxit}, @var{M}, [], @var{x0}, @dots{})
## @deftypefnx {} {[@var{x}, @var{flag}, @var{relres}, @var{iter}, @var{resvec}] =} cgs (@var{A}, @var{b}, @dots{})
## Solve @code{A x = b}, where @var{A} is a square matrix, using the
## Conjugate Gradients Squared method.
##
## The input arguments are:
##
## @itemize @minus
##
## @item @var{A} is the matrix of the linear system and it must be square.
## @var{A} can be passed as a matrix, function handle, or inline
## function @code{Afcn} such that @code{Afcn(x) = A * x}.  Additional
## parameters to @code{Afcn} are passed after @var{x0}.
##
## @item @var{b} is the right hand side vector.  It must be a column vector
## with same number of rows of @var{A}.
##
## @item @var{tol} is the relative tolerance, if not given or set to [] the
## default value 1e-6 is used.
##
## @item @var{maxit} the maximum number of outer iterations, if not given or
## set to [] the default value @code{min (20, numel (b))} is used.
##
## @item @var{M1}, @var{M2} are the preconditioners.  The preconditioner
## matrix is given as @code{M = M1 * M2}.  Both @var{M1}
## and @var{M2} can be passed as a matrix or as a function handle or inline
## function @code{g} such that @code{g(x) = M1 \ x} or @code{g(x) = M2 \ x}.
## If M1 is empty or not passed then no preconditioners are applied.
## The technique used is the right preconditioning, i.e., it is solved
## @code{@var{A}*inv(@var{M})*y = b} and then @code{@var{x} = inv(@var{M})*y}.
##
## @item @var{x0} the initial guess, if not given or set to [] the default
## value @code{zeros (size (b))} is used.
## @end itemize
##
## The arguments which follow @var{x0} are treated as parameters, and passed in
## a proper way to any of the functions (@var{A} or @var{P}) which are passed
## to @code{cgs}.
##
## The output parameters are:
##
## @itemize @minus
##
## @item @var{x} is the approximation computed.  If the method doesn't
## converge then it is the iterated with the minimum residual.
##
## @item @var{flag} indicates the exit status:
##
## @itemize @minus
## @item 0: iteration converged to the within the chosen tolerance
##
## @item 1: the maximum number of iterations was reached before convergence
##
## @item 2: the preconditioner matrix is singular
##
## @item 3: the algorithm reached stagnation
##
## @item 4: the algorithm can't continue due to a division by zero
## @end itemize
##
## @item @var{relres} is the relative residual obtained with as
## @code{(@var{A}*@var{x}-@var{b}) / @code{norm(@var{b})}}.
##
## @item @var{iter} is the iteration which @var{x} is computed.
##
## @item @var{resvec} is a vector containing the residual at each iteration.
## Doing @code{length(@var{resvec}) - 1} is possible to see the total number
## of iterations performed.
## @end itemize
##
## Let us consider a trivial problem with a tridiagonal matrix
##
## @example
## @group
## n = 20;
## A = toeplitz (sparse ([1, 1], [1, 2], [2, 1] * n ^ 2, 1, n))  + ...
##     toeplitz (sparse (1, 2, -1, 1, n) * n / 2, ...
##     sparse (1, 2, 1, 1, n) * n / 2);
## b = A * ones (n, 1);
## restart = 5;
## [M1, M2] = ilu (A); # in this tridiag case it corresponds to chol (A)'
## M = M1 * M2;
## Afcn = @@(x) A * x;
## Mfcn = @@(x) M \ x;
## M1fcn = @@(x) M1 \ x;
## M2fcn = @@(x) M2 \ x;
## @end group
## @end example
##
## @sc{Example 1:} simplest usage of @code{cgs}
##
## @example
## x = cgs (A, b, [], n)
## @end example
##
## @sc{Example 2:} @code{cgs} with a function which computes
## @code{@var{A} * @var{x}}
##
## @example
## x = cgs (Afcn, b, [], n)
## @end example
##
## @sc{Example 3:} @code{cgs} with a preconditioner matrix @var{M}
##
## @example
## x = cgs (A, b, [], 1e-06, n, M)
## @end example
##
## @sc{Example 4:} @code{cgs} with a function as preconditioner
##
## @example
## x = cgs (Afcn, b, 1e-6, n, Mfcn)
## @end example
##
## @sc{Example 5:} @code{cgs} with preconditioner matrices @var{M1}
## and @var{M2}
##
## @example
## x = cgs (A, b, [], 1e-6, n, M1, M2)
## @end example
##
## @sc{Example 6:} @code{cgs} with functions as preconditioners
##
## @example
## x = cgs (Afcn, b, 1e-6, n, M1fcn, M2fcn)
## @end example
##
## @sc{Example 7:} @code{cgs} with as input a function requiring an argument
##
## @example
## @group
## function y = Ap (A, x, z) # compute A^z * x
##    y = x;
##    for i = 1:z
##      y = A * y;
##    endfor
##  endfunction
## Apfcn = @@(x, string, p) Ap (A, x, string, p);
## x = cgs (Apfcn, b, [], [], [], [], [], 2);
## @end group
## @end example
##
## @sc{Example 8:} explicit example to show that @code{cgs} uses a
## right preconditioner
##
## @example
## @group
## [M1, M2] = ilu (A + 0.3 * eye (n)); # factorization of A perturbed
## M = M1 * M2;
##
## ## reference solution computed by cgs after one iteration
## [x_ref, fl] = cgs (A, b, [], 1, M)
##
## ## right preconditioning
## [y, fl] = cgs (A / M, b, [], 1)
## x = M \ y # compare x and x_ref
##
## @end group
## @end example
##
## References:
##
## @nospell{Y. Saad}, @cite{Iterative Methods for Sparse Linear Systems},
## Second edition, 2003, SIAM
##
## @seealso{pcg, bicgstab, bicg, gmres, qmr, tfqmr}
## @end deftypefn

function [x_min, flag, relres, iter_min, resvec] = ...
         cgs (A, b, tol = [], maxit = [], M1 = [] , M2 = [], x0 = [], varargin)

  [Afcn, M1fcn, M2fcn] = __alltohandles__ (A, b, M1, M2, "cgs");

  [tol, maxit, x0] = __default__input__ ({1e-06, min( rows(b), 20), ...
                                          zeros(size (b))}, tol, maxit, x0);

  norm_b = norm (b, 2);
  if (norm_b == 0)
    if (nargout < 2)
      printf ("The right hand side vector is all zero so cgs \n")
      printf ("returned an all zero solution without iterating.\n")
    endif
    x_min = zeros (numel (b), 1);
    iter_min = 0;
    flag = 0;
    resvec = 0;
    relres = 0;
    return;
  endif

  resvec = zeros (maxit, 1); # Preallocation of resvec

  flag = 1; # Default flag is 1, i.e. maximum number of iterations reached
  iter = iter_min = 0;
  x = x_min = x_pr = x0;
  ## x approximation at the actual iteration
  ## x_min approximation with the minimum residual
  ## x_pr approximation at the previous iteration (to check stagnation)

  r0 = rr = u = p = b - feval (Afcn, x, varargin{:});
  resvec (1) = norm (r0, 2);
  rho_1 = rr' * r0;

  try
    warning ("error","Octave:singular-matrix","local");
    p_hat = feval (M1fcn, p, varargin{:});
    p_hat = feval (M2fcn, p_hat, varargin {:});
  catch
    flag = 2;
  end_try_catch

  while ((flag != 2) && (iter < maxit) && ...
         (resvec (iter + 1) >= tol * norm_b))
    v = feval (Afcn, p_hat, varargin{:});
    prod_tmp = (rr' * v);
    if (prod_tmp == 0)
      flag = 4;
      break;
    endif
    alpha = rho_1 / prod_tmp;
    q = u - alpha * v;
    u_hat = feval(M1fcn, u + q, varargin{:});
    u_hat = feval (M2fcn, u_hat, varargin{:});
    x += alpha*u_hat;
    r0 -= alpha* feval (Afcn, u_hat, varargin{:});
    iter += 1;
    resvec (iter + 1) = norm (r0, 2);
    if (norm (x - x_pr, 2) <= norm (x, 2) * eps) # Stagnation
      flag = 3;
      break;
    endif
    if (resvec (iter + 1) <= resvec (iter_min + 1)) # Check min residual
      x_min = x;
      iter_min = iter;
    endif
    x_pr = x;
    rho_2 = rho_1;
    rho_1 = rr' * r0;
    if (rho_1 == 0)
      flag = 4;
      break;
    endif
    beta = rho_1 / rho_2;
    u = r0 + beta * q;
    p = u + beta * (q + beta * p);
    p_hat = feval (M1fcn, p, varargin {:});
    p_hat = feval (M2fcn, p_hat, varargin{:});
  endwhile
  resvec = resvec (1: (iter + 1));

  relres = resvec (iter_min + 1) / norm_b;
  if (relres <= tol) && (flag = 1)
    flag = 0;
  endif

  if (nargout < 2)
    switch (flag)
      case {0}
        printf ("cgs converged at iteration %i ", iter_min);
        printf ("to a solution with relative residual %e\n", relres);
      case {1}
        printf ("cgs stopped at iteration %i ", iter);
        printf ("without converging to the desired tolerance %e\n", tol);
        printf ("because the maximum number of iterations was reached.\n");
        printf ("The iterate returned (number %i) ", iter_min);
        printf ("has relative residual %e\n", relres);
      case {2}
        printf ("cgs stopped at iteration %i ", iter);
        printf ("without converging to the desired tolerance %e\n", tol);
        printf ("because the preconditioner matrix is singular.\n");
        printf ("The iterate returned (number %i) ", iter_min);
        printf ("has relative residual %e\n", relres);
      case {3}
        printf ("cgs stopped at iteration %i ", iter);
        printf ("without converging to the desired tolerance %e\n", tol);
        printf ("because the method stagnated.\n");
        printf ("The iterate returned (number %i) ", iter_min);
        printf ("has relative residual %e\n", relres);
      case {4}
        printf ("cgs stopped at iteration %i ", iter);
        printf ("without converging to the desired tolerance %e\n", tol);
        printf ("because the method can't continue.\n");
        printf ("The iterate returned (number %i) ", iter_min);
        printf ("has relative residual %e\n", relres);
    endswitch
  endif

endfunction


%!demo
%! ## Solve system of A*x=b
%! A = [5 -1 3;-1 2 -2;3 -2 3];
%! b = [7;-1;4];
%! [a,b,c,d,e] = cgs (A,b)

%!demo
%! ## simplest use case
%! n = 20;
%! A = toeplitz (sparse ([1, 1], [1, 2], [2, 1] * n ^ 2, 1, n))  + ...
%!     toeplitz (sparse (1, 2, -1, 1, n) * n / 2, ...
%!               sparse (1, 2, 1, 1, n) * n / 2);
%! b = A * ones (n, 1);
%! [M1, M2] = ilu (A + 0.1 * eye (n));
%! M = M1 * M2;
%! x = cgs (A, b, [], n);
%! Afcn = @(x) A * x;
%! x = cgs (Afcn, b, [], n);
%! x = cgs (A, b, 1e-6, n, M);
%! x = cgs (A, b, 1e-6, n, M1, M2);
%! Mfcn = @(z) M \ z;
%! x = cgs (Afcn, b, 1e-6, n, Mfcn);
%! M1fcn = @(z) M1 \ z;
%! M2fcn = @(z) M2 \ z;
%! x = cgs (Afcn, b, 1e-6, n, M1fcn, M2fcn);
%! function y = Ap (A, x, z)
%!   ## compute A^z * x or (A^z)' * x
%!   y = x;
%!   for i = 1:z
%!     y = A * y;
%!   endfor
%! endfunction
%! Afcn = @(x, p) Ap (A, x, p);
%! x = cgs (Afcn, b, [], 2*n, [], [], [], 2); # solution of A^2 * x = b

%!demo
%! n = 10;
%! A = toeplitz (sparse ([1, 1], [1, 2], [2, 1] * n ^ 2, 1, n))  + ...
%!     toeplitz (sparse (1, 2, -1, 1, n) * n / 2, ...
%!               sparse (1, 2, 1, 1, n) * n / 2);
%! b = A * ones (n, 1);
%! [M1, M2] = ilu (A + 0.3 * speye (n));  # factorization of A perturbed
%! M = M1 * M2;
%!
%! ## Reference solution computed by cgs after one iteration
%! [x_ref, fl] = cgs (A, b, [], 1, M);
%! x_ref
%!
%! ## right preconditioning
%! [y, fl] = cgs (A / M, b, [], 1);
%! ## Compare x and x_ref
%! x = M \ y

%!test
%! ## Check that all type of inputs work
%! A = sparse (toeplitz ([2, 1, 0, 0, 0], [2, -1, 0, 0, 0]));
%! b = sum (A, 2);
%! M1 = diag (sqrt (diag (A)));
%! M2 = M1;
%! maxit = 10;
%! Afcn = @(z) A * z;
%! M1_fcn = @(z) M1 \ z;
%! M2_fcn = @(z) M2 \ z;
%! [x, flag] = cgs (A,b);
%! assert (flag, 0);
%! [x, flag] = cgs (A, b, [], maxit, M1, M2);
%! assert (flag, 0);
%! [x, flag] = cgs (A, b, [], maxit, M1_fcn, M2_fcn);
%! assert (flag, 0);
%! [x, flag] = cgs (A, b, [], maxit, M1_fcn, M2);
%! assert (flag, 0);
%! [x, flag] = cgs (A, b, [], maxit, M1, M2_fcn);
%! assert (flag, 0);
%! [x, flag] = cgs (Afcn, b);
%! assert (flag, 0);
%! [x, flag] = cgs (Afcn, b, [], maxit, M1, M2);
%! assert (flag, 0);
%! [x, flag] = cgs (Afcn, b, [], maxit, M1_fcn, M2);
%! assert (flag, 0);
%! [x, flag] = cgs (Afcn, b, [], maxit, M1, M2_fcn);
%! assert (flag, 0);
%! [x, flag] = cgs (Afcn, b, [], maxit, M1_fcn, M2_fcn);
%! assert (flag, 0);

%!shared n, A, b, tol, maxit, M
%!
%!test
%! n = 100;
%! A = spdiags ([-ones(n,1) 4*ones(n,1) -ones(n,1)], -1:1, n, n);
%! b = sum (A, 2);
%! tol = 1e-8;
%! maxit = 1000;
%! M = 4 * eye (n);
%! [x, flag, relres, iter, resvec] = cgs (A, b, tol, maxit, M);
%! assert (norm (b - A*x) / norm (b), 0, tol);

%!
%!test
%! maxit = 15;
%! [x, flag, relres, iter, resvec] = cgs (@(x) A * x, b, tol, maxit, M);
%! assert (norm (b - A*x) / norm (b), 0, tol);

%!test
%! a = sprand (n, n, .1);
%! A = a'*a + 100 * eye (n);
%! b = sum (A, 2);
%! [x, flag, relres, iter, resvec] = cgs (A, b, tol, [], diag (diag (A)));
%! assert (norm (b - A*x) / norm (b), 0, tol);

%!test
%! n = 5;
%! A = sparse (toeplitz ([2, 1, 0, 0, 0], [2, -1, 0, 0, 0]));
%! b = sum (A, 2);
%! M = ones (n);
%! [x, flag] = cgs (A, b, [], [], M);
%! assert (flag, 2);

%!test
%! A = single (1);
%! b = 1;
%! [x, flag] = cgs (A, b);
%! assert (class (x), "single");

%!test
%! A = 1;
%! b = single (1);
%! [x, flag] = cgs (A, b);
%! assert (class (x), "single");

%!test
%! A = single (1);
%! b = single (1);
%! [x, flag] = cgs (A, b);
%! assert (class (x), "single");

%!test
%!function y = Afcn (x)
%!  A = toeplitz ([2, 1, 0, 0], [2, -1, 0, 0]);
%!  y = A * x;
%!endfunction
%! [x, flag] = cgs ("Afcn", [1; 2; 2; 3]);
%! assert (norm (b - A*x) / norm (b), 0, 1e-6);

%!test
%! ## test a complex linear system
%! A = toeplitz (sparse ([2, 1, 0, 0, 0]), sparse ([2, -1, 0, 0, 0])) + ...
%! 1i * toeplitz (sparse ([2, 1, 0, 0, 0]), sparse ([2, -1, 0, 0, 0]));
%! b = sum (A, 2);
%! [x, flag] = cgs (A, b);
%! assert (flag, 0);

%!test
%! ## unpreconditioned residual
%! A = toeplitz (sparse ([2, 1, 0, 0, 0]), sparse ([2, -1, 0, 0, 0]));
%! b = sum (A, 2);
%! M = magic (5);
%! [x, flag, relres] = cgs (A, b, [], 3, M);
%! assert (norm (b - A * x) / norm (b), relres, 8 * eps);
