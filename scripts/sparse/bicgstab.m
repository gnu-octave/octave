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
## @deftypefn  {} {@var{x} =} bicgstab (@var{A}, @var{b}, @var{tol}, @var{maxit}, @var{M1}, @var{M2}, @var{x0}, @dots{})
## @deftypefnx {} {@var{x} =} bicgstab (@var{A}, @var{b}, @var{tol}, @var{maxit}, @var{M}, [], @var{x0}, @dots{})
## @deftypefnx {} {[@var{x}, @var{flag}, @var{relres}, @var{iter}, @var{resvec}] =} bicgstab (@var{A}, @var{b}, @dots{})
## Solve @code{A x = b} using the stabilized Bi-conjugate gradient iterative
## method.
##
## The input parameters are:
##
## @itemize @minus
##
## @item @var{A} is the matrix of the linear system and it must be square.
## @var{A} can be passed as a matrix, function handle, or inline
## function @code{Afcn} such that @code{Afcn(x) = A * x}.  Additional
## parameters to @code{Afcn} are passed after @var{x0}.
##
## @item @var{b} is the right hand side vector.  It must be a column vector
## with the same number of rows as @var{A}.
##
## @item
## @var{tol} is the required relative tolerance for the residual error,
## @w{@code{@var{b} - @var{A} * @var{x}}}.  The iteration stops if
## @w{@code{norm (@var{b} - @var{A} * @var{x})} @leq{}
## @w{@code{@var{tol} * norm (@var{b})}}}.
## If @var{tol} is omitted or empty, then a tolerance of 1e-6 is used.
##
## @item @var{maxit} the maximum number of outer iterations, if not given or
## set to [] the default value @code{min (20, numel (b))} is used.
##
## @item @var{M1}, @var{M2} are the preconditioners.  The preconditioner
## @var{M} is given as @code{@var{M} = @var{M1} * @var{M2}}.
## Both @var{M1} and @var{M2} can be passed as a matrix or as a function
## handle or inline function @code{g} such that
## @code{g(@var{x}) = @var{M1} \ @var{x}} or
## @code{g(@var{x}) = @var{M2} \ @var{x}}.
## The technique used is the right preconditioning, i.e., it is
## solved @code{@var{A} * inv (@var{M}) * @var{y} = @var{b}} and then
## @code{@var{x} = inv (@var{M}) * @var{y}}.
##
## @item @var{x0} the initial guess, if not given or set to [] the default
## value @code{zeros (size (@var{b}))} is used.
##
## @end itemize
##
## The arguments which follow @var{x0} are treated as parameters, and passed in
## a proper way to any of the functions (@var{A} or @var{M}) which are passed
## to @code{bicstab}.
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
## @item @var{iter} is the (possibly half) iteration which @var{x} is
## computed.  If it is an half iteration then it is @code{@var{iter} + 0.5}
##
## @item @var{resvec} is a vector containing the residual of each half and
## total iteration (There are also the half iterations since @var{x} is
## computed in two steps at each iteration).
## Doing @code{(length(@var{resvec}) - 1) / 2} is possible to see the
## total number of (total) iterations performed.
##
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
## [M1, M2] = ilu (A); # in this tridiag case, it corresponds to lu (A)
## M = M1 * M2;
## Afcn = @@(x) A * x;
## Mfcn = @@(x) M \ x;
## M1fcn = @@(x) M1 \ x;
## M2fcn = @@(x) M2 \ x;
## @end group
## @end example
##
## @sc{Example 1:} simplest usage of @code{bicgstab}
##
## @example
## x = bicgstab (A, b, [], n)
## @end example
##
## @sc{Example 2:} @code{bicgstab} with a function which computes
## @code{@var{A} * @var{x}}
##
## @example
## x = bicgstab (Afcn, b, [], n)
## @end example
##
## @sc{Example 3:} @code{bicgstab} with a preconditioner matrix @var{M}
##
## @example
## x = bicgstab (A, b, [], 1e-06, n, M)
## @end example
##
## @sc{Example 4:} @code{bicgstab} with a function as preconditioner
##
## @example
## x = bicgstab (Afcn, b, 1e-6, n, Mfcn)
## @end example
##
## @sc{Example 5:} @code{bicgstab} with preconditioner matrices @var{M1}
## and @var{M2}
##
## @example
## x = bicgstab (A, b, [], 1e-6, n, M1, M2)
## @end example
##
## @sc{Example 6:} @code{bicgstab} with functions as preconditioners
##
## @example
## x = bicgstab (Afcn, b, 1e-6, n, M1fcn, M2fcn)
## @end example
##
## @sc{Example 7:} @code{bicgstab} with as input a function requiring
## an argument
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
## x = bicgstab (Apfcn, b, [], [], [], [], [], 2);
## @end group
## @end example
##
## @sc{Example 8:} explicit example to show that @code{bicgstab} uses a
## right preconditioner
##
## @example
## @group
## [M1, M2] = ilu (A + 0.1 * eye (n)); # factorization of A perturbed
## M = M1 * M2;
##
## ## reference solution computed by bicgstab after one iteration
## [x_ref, fl] = bicgstab (A, b, [], 1, M)
##
## ## right preconditioning
## [y, fl] = bicgstab (A / M, b, [], 1)
## x = M \ y # compare x and x_ref
##
## @end group
## @end example
##
## Reference:
##
## @nospell{Y. Saad}, @cite{Iterative Methods for Sparse Linear
## Systems}, Second edition, 2003, SIAM
##
## @seealso{bicg, cgs, gmres, pcg, qmr, tfqmr}
##
## @end deftypefn

function [x_min, flag, relres, iter_min, resvec] = ...
         bicgstab (A, b, tol = [], maxit = [], M1 = [], M2 = [], ...
                   x0 = [], varargin)

  ## Check consistency and  type of A, M1, M2
  [Afcn, M1fcn, M2fcn] =  __alltohandles__ (A, b, M1, M2, "bicgstab");

  ## Check if input tol are empty (set them to default if necessary)
  [tol, maxit, x0] = __default__input__ ({1e-06, min(rows(b), 20), ...
                    zeros(rows (b), 1)}, tol, maxit, x0);

  norm_b = norm (b, 2);
  if (norm_b == 0)
    if (nargout < 2)
      printf ("The right hand side vector is all zero so bicgstab \n")
      printf ("returned an all zero solution without iterating.\n")
    endif
    x_min = zeros (numel (b), 1);
    iter_min = 0;
    flag = 0;
    resvec = 0;
    relres = 0;
    return;
  endif

  ## Double maxit to mind also the "half iterations"
  d_maxit = 2 * maxit;
  iter = iter_min = 0;
  resvec = zeros (d_maxit,1);
  x = x_min = x_pr = x0;
  iter = iter_min = 0;
  ## default setting of flag is 1 (i.e. max number of iterations reached)
  flag = 1;

  res = b - feval (Afcn, x, varargin{:});
  rr = p = res; # rr is r_star
  rho_1 = rr' * res;
  resvec (1) = norm (res,2);
  real_tol = norm_b * tol;

  ## To check if the preconditioners are singular or they have some NaN
  try
    warning ("error", "Octave:singular-matrix", "local");
    p_hat = feval (M1fcn, p, varargin{:});
    p_hat = feval (M2fcn, p_hat, varargin{:});
  catch
    flag = 2;
  end_try_catch

  while (flag !=2) && (iter < d_maxit) && (resvec (iter + 1) >= real_tol)
    v = feval (Afcn, p_hat, varargin{:});
    prod_tmp = (rr' * v);
    if (prod_tmp == 0)
      flag = 4;
      break;
    endif
    alpha = rho_1 / (prod_tmp);
    x += alpha * p_hat;
    s = res - alpha * v;
    iter += 1;
    resvec (iter+1) = norm (s,2);
    if (resvec (iter + 1) <= real_tol) # reached the tol
      x_min = x;
      iter_min = iter;
      break;
    elseif (resvec (iter + 1) <= resvec (iter_min + 1)) # Found min residual
      x_min = x;
      iter_min = iter;
    endif
    s_hat = feval (M1fcn, s, varargin{:});
    s_hat = feval (M2fcn, s_hat, varargin{:});
    t = feval (Afcn, s_hat, varargin{:});
    omega = (t' * s) / (t' * t);
    if (omega == 0) # x and residual don't change and the next it will be NaN
      flag = 4;
      break;
    endif
    x += omega * s_hat;
    res = s - omega * t;
    iter += 1;
    resvec (iter + 1) = norm (res);
    if (resvec (iter + 1) <= resvec (iter_min + 1))
      x_min = x;
      iter_min = iter;
    endif
    if (norm (x - x_pr) <= norm (x) * eps)
      flag = 3;
      break;
    endif
    x_pr = x;
    rho_2 = rho_1;
    rho_1 = rr' * res;
    if (rho_1 == 0) # x and residual don't change and the next it will be NaN
      flag = 4;
      break;
    endif
    beta = (rho_1 / rho_2) * (alpha / omega);
    p = res + beta * (p - omega*  v);
    p_hat = feval (M1fcn, p, varargin{:});
    p_hat = feval (M2fcn, p_hat, varargin{:});
  endwhile
  resvec = resvec (1:iter+1,1);

  relres = resvec (iter_min + 1) / norm_b;  # I set the relative residual
  iter /=  2;
  iter_min /= 2;

  if (flag == 1) && (relres <= tol)
    flag = 0;
  endif

  ## output strings to print when the outputs requested are less than 2
  if (nargout < 2)
    switch (flag)
      case {0}
        printf ("bicgstab converged at iteration %i ", iter_min);
        printf ("to a solution with relative residual %e\n", relres);
      case {1}
        printf ("bicgstab stopped at iteration %i ", iter);
        printf ("without converging to the desired tolerance %e\n", tol);
        printf ("because the maximum number of iterations was reached.\n");
        printf ("The iterate returned (number %i) ", iter_min);
        printf ("has relative residual %e\n", relres);
      case {2}
        printf ("bicgstab stopped at iteration %i ", iter);
        printf ("without converging to the desired tolerance %e\n", tol);
        printf ("because the preconditioner matrix is singular.\n");
        printf ("The iterate returned (number %i) ", iter_min);
        printf ("has relative residual %e\n", relres);
      case {3}
        printf ("bicgstab stopped at iteration %i ", iter);
        printf ("without converging to the desired tolerance %e\n", tol);
        printf ("because the method stagnated.\n");
        printf ("The iterate returned (number %i) ", iter_min);
        printf ("has relative residual %e\n", relres);
      case {4}
        printf ("bicgstab stopped at iteration %i ", iter);
        printf ("without converging to the desired tolerance %e\n", tol);
        printf ("because the method can't continue.\n");
        printf ("The iterate returned (number %i) ", iter_min);
        printf ("has relative residual %e\n", relres);
    endswitch
  endif

endfunction


%!demo # simplest use
%! n = 20;
%! A = toeplitz (sparse ([1, 1], [1, 2], [2, 1] * n ^ 2, 1, n))  + ...
%!     toeplitz (sparse (1, 2, -1, 1, n) * n / 2, ...
%!               sparse (1, 2, 1, 1, n) * n / 2);
%! b = A * ones (n, 1);
%! [M1, M2] = ilu (A + 0.1 * eye (n));
%! M = M1 * M2;
%! x = bicgstab (A, b, [], n);
%! Afcn = @(x) A * x;
%! x = bicgstab (Afcn, b, [], n);
%! x = bicgstab (A, b, 1e-6, n, M);
%! x = bicgstab (A, b, 1e-6, n, M1, M2);
%! Mfcn = @(z) M \ z;
%! x = bicgstab (Afcn, b, 1e-6, n, Mfcn);
%! M1fcn = @(z) M1 \ z;
%! M2fcn = @(z) M2 \ z;
%! x = bicgstab (Afcn, b, 1e-6, n, M1fcn, M2fcn);
%! function y = Ap (A, x, z)
%!   ## compute A^z * x or (A^z)' * x
%!   y = x;
%!   for i = 1:z
%!     y = A * y;
%!   endfor
%! endfunction
%! Afcn = @(x, p) Ap (A, x, p);
%! x = bicgstab (Afcn, b, [], 2 * n, [], [], [], 2); # solution of A^2 * x = b

%!demo
%! n = 10;
%! A = toeplitz (sparse ([1, 1], [1, 2], [2, 1] * n ^ 2, 1, n))  + ...
%!     toeplitz (sparse (1, 2, -1, 1, n) * n / 2, ...
%!               sparse (1, 2, 1, 1, n) * n / 2);
%! b = A * ones (n, 1);
%! [M1, M2] = ilu (A + 0.3 * eye (n));  # factorization of A perturbed
%! M = M1 * M2;
%!
%! ## Reference solution computed by bicgstab after one iteration
%! [x_ref, fl] = bicgstab (A, b, [], 1, M);
%! x_ref
%!
%! ## right preconditioning
%! [y, fl] = bicgstab (A / M, b, [], 1);
%! ## Compare x and x_ref
%! x = M \ y

%!test
%! ## Check that all type of inputs work
%! A = toeplitz (sparse ([2, 1, 0, 0, 0]), sparse ([2, -1, 0, 0, 0]));
%! b = sum (A, 2);
%! M1 = diag (sqrt (diag (A)));
%! M2 = M1;
%! maxit = 20;
%! Afcn = @(z) A*z;
%! M1_fcn = @(z) M1 \ z;
%! M2_fcn = @(z) M2 \ z;
%! [x, flag] = bicgstab (A,b );
%! assert (flag, 0);
%! [x, flag] = bicgstab (A, b, [], maxit, M1, M2);
%! assert (flag, 0);
%! [x, flag] = bicgstab (A, b, [], maxit, M1_fcn, M2_fcn);
%! assert (flag, 0);
%! [x, flag] = bicgstab (A, b, [], maxit, M1_fcn, M2);
%! assert (flag, 0);
%! [x, flag] = bicgstab (A, b, [], maxit, M1, M2_fcn);
%! assert (flag, 0);
%! [x, flag] = bicgstab (Afcn, b);
%! assert (flag, 0);
%! [x, flag] = bicgstab (Afcn, b, [], maxit, M1, M2);
%! assert (flag, 0);
%! [x, flag] = bicgstab (Afcn, b, [], maxit, M1_fcn, M2);
%! assert (flag, 0);
%! [x, flag] = bicgstab (Afcn, b, [], maxit, M1, M2_fcn);
%! assert (flag, 0);
%! [x, flag] = bicgstab (Afcn, b, [], maxit, M1_fcn, M2_fcn);
%! assert (flag, 0);

%!shared n, A, b, tol, maxit, M1, M2
%! n = 100;
%! A = spdiags ([-2*ones(n,1) 4*ones(n,1) -ones(n,1)], -1:1, n, n);
%! b = sum (A, 2);
%! tol = 1e-8;
%! maxit = 15;
%! M1 = spdiags ([ones(n,1)/(-2) ones(n,1)],-1:0, n, n);
%! M2 = spdiags ([4*ones(n,1) -ones(n,1)], 0:1, n, n);

%!test
%! [x, flag, relres, iter, resvec] = bicgstab (A, b, tol, maxit, M1, M2);
%! assert (norm (b - A*x) / norm (b), 0, tol);

%!function y = afcn (x, a)
%!  y = a * x;
%!endfunction
%!
%!test
%! [x, flag, relres, iter, resvec] = bicgstab (@(x) afcn (x, A), b,
%!                                             tol, maxit, M1, M2);
%! assert (norm (b - A*x) / norm (b), 0, tol);

%!test
%! a = sprand (n, n, .1);
%! A = a'*a + 100 * speye (n);
%! b = sum (A, 2);
%! [x, flag, relres, iter, resvec] = bicgstab (A, b, tol, [], diag (diag (A)));
%! assert (norm (b - A*x) / norm (b), 0, tol);

%!test
%! ## bicgstab solves complex linear systems
%! A = [1 + 1i, 1 + 1i; 2 - 1i, 2 + 1i];
%! b = A * [1; 1];
%! [x, flag, relres, iter, resvec] = bicgstab (A, b);
%! assert (norm (b - A*x) / norm (b), 0, 1e-6);

%!test
%! ## test with a non-symmetric matrix
%! A = diag (1:50);
%! A(1,50) = 10000;
%! b = ones (50,1);
%! [x, flag, relres, iter, resvec] = bicgstab (A, b, [], 100);
%! assert (flag, 0);
%! assert (norm (b - A*x) / norm (b), 0, 1e-6);

%!test
%! ## test that bicgstab detects a singular preconditioner
%! M = ones (50);
%! M(1,1) = 0;
%! [x, flag] = bicgstab (A, b, [], 100, M);
%! assert (flag, 2);

%!test
%! A = single (1);
%! b = 1;
%! [x, flag] = bicgstab (A, b);
%! assert (class (x), "single");

%!test
%! A = 1;
%! b = single (1);
%! [x, flag] = bicgstab (A, b);
%! assert (class (x), "single");

%!test
%! A = single (1);
%! b = single (1);
%! [x, flag] = bicgstab (A, b);
%! assert (class (x), "single");

%!test
%!function y = Afcn (x)
%!  A = sparse (toeplitz ([2, 1, 0, 0], [2, -1, 0, 0]));
%!  y = A * x;
%!endfunction
%!
%! b = [1; 2; 2; 3];
%! [x, flag] = bicgstab ("Afcn", b);
%! assert (norm (b - A*x) / norm (b), 0, 1e-6);

%!test
%! ## unpreconditioned residual
%! A = sparse (toeplitz ([2, 1, 0, 0, 0], [2, -1, 0, 0, 0]));
%! b = sum (A, 2);
%! M = magic (5);
%! [x, flag, relres] = bicgstab (A, b, [], 2, M);
%! assert (norm (b - A * x) / norm (b), relres, 8*eps);
