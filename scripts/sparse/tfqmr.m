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
## @deftypefn  {} {@var{x} =} tfqmr (@var{A}, @var{b}, @var{tol}, @var{maxit}, @var{M1}, @var{M2}, @var{x0}, @dots{})
## @deftypefnx {} {@var{x} =} tfqmr (@var{A}, @var{b}, @var{tol}, @var{maxit}, @var{M}, [], @var{x0}, @dots{})
## @deftypefnx {} {[@var{x}, @var{flag}, @var{relres}, @var{iter}, @var{resvec}] =} tfqmr (@var{A}, @var{b}, @dots{})
## Solve @code{A x = b} using the Transpose-Tree qmr method, based on the cgs.
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
## @item @var{tol} is the relative tolerance, if not given or set to [] the
## default value 1e-6 is used.
##
## @item @var{maxit} the maximum number of outer iterations, if not given or
## set to [] the default value @code{min (20, numel (b))} is used.  To be
## compatible, since the method as different behaviors in the iteration
## number is odd or even, is considered as iteration in @code{tfqmr} the
## entire odd-even cycle.  That is, to make an entire iteration, the algorithm
## performs two sub-iterations: the odd one and the even one.
##
## @item @var{M1}, @var{M2} are the preconditioners.  The preconditioner
## @var{M} is given as @code{M = M1 * M2}.
## Both @var{M1} and @var{M2} can be passed as a matrix or as a function
## handle or inline function @code{g} such that @code{g(x) = M1 \ x} or
## @code{g(x) = M2 \ x}.
## The technique used is the right-preconditioning, i.e., it is solved
## @code{A*inv(M)*y = b} and then @code{x = inv(M)*y}, instead of
## @code{A x = b}.
##
## @item @var{x0} the initial guess, if not given or set to [] the default
## value @code{zeros (size (b))} is used.
##
## @end itemize
##
## The arguments which follow @var{x0} are treated as parameters, and passed in
## a proper way to any of the functions (@var{A} or @var{M}) which are passed
## to @code{tfqmr}.
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
## @item @var{relres} is the relative residual obtained as
## @code{(@var{A}*@var{x}-@var{b}) / @code{norm (@var{b})}}.
##
## @item @var{iter} is the iteration which @var{x} is
## computed.
##
## @item @var{resvec} is a vector containing the residual at each iteration
## (including @code{norm (b - A x0)}).
## Doing @code{length (@var{resvec}) - 1} is possible to see the
## total number of iterations performed.
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
## [M1, M2] = ilu (A); # in this tridiag case it corresponds to chol (A)'
## M = M1 * M2;
## Afcn = @@(x) A * x;
## Mfcn = @@(x) M \ x;
## M1fcn = @@(x) M1 \ x;
## M2fcn = @@(x) M2 \ x;
## @end group
## @end example
##
## @sc{Example 1:} simplest usage of @code{tfqmr}
##
## @example
## x = tfqmr (A, b, [], n)
## @end example
##
## @sc{Example 2:} @code{tfqmr} with a function which computes
## @code{@var{A} * @var{x}}
##
## @example
## x = tfqmr (Afcn, b, [], n)
## @end example
##
## @sc{Example 3:} @code{tfqmr} with a preconditioner matrix @var{M}
##
## @example
## x = tfqmr (A, b, [], 1e-06, n, M)
## @end example
##
## @sc{Example 4:} @code{tfqmr} with a function as preconditioner
##
## @example
## x = tfqmr (Afcn, b, 1e-6, n, Mfcn)
## @end example
##
## @sc{Example 5:} @code{tfqmr} with preconditioner matrices @var{M1}
## and @var{M2}
##
## @example
## x = tfqmr (A, b, [], 1e-6, n, M1, M2)
## @end example
##
## @sc{Example 6:} @code{tfmqr} with functions as preconditioners
##
## @example
## x = tfqmr (Afcn, b, 1e-6, n, M1fcn, M2fcn)
## @end example
##
## @sc{Example 7:} @code{tfqmr} with as input a function requiring an argument
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
## x = tfqmr (Apfcn, b, [], [], [], [], [], 2);
## @end group
## @end example
##
## @sc{Example 8:} explicit example to show that @code{tfqmr} uses a
## right preconditioner
##
## @example
## @group
## [M1, M2] = ilu (A + 0.3 * eye (n)); # factorization of A perturbed
## M = M1 * M2;
##
## ## reference solution computed by tfqmr after one iteration
## [x_ref, fl] = tfqmr (A, b, [], 1, M)
##
## ## right preconditioning
## [y, fl] = tfqmr (A / M, b, [], 1)
## x = M \ y # compare x and x_ref
##
## @end group
## @end example
##
## Reference:
##
## @nospell{Y. Saad}, @cite{Iterative Methods for Sparse Linear Systems},
## Second edition, 2003, SIAM
##
## @seealso{bicg, bicgstab, cgs, gmres, pcg, qmr, pcr}
##
## @end deftypefn

function [x_min, flag, relres, iter_min, resvec] = ...
         tfqmr (A, b, tol = [], maxit = [], M1 = [], M2 = [], ...
                x0 = [], varargin)

  [Afcn, M1fcn, M2fcn] = __alltohandles__ (A, b, M1, M2, "tfqmr");

  [tol, maxit, x0] = __default__input__ ({1e-06, 2 * min(20, rows (b)), ...
                                          zeros(rows (b), 1)}, tol, ...
                                         maxit, x0);

  maxit = 2 * maxit; # To be compatible, since iteration = odd+even ones

  norm_b = norm (b, 2);
  if (norm_b == 0)
    if (nargout < 2)
      printf ("The right hand side vector is all zero so tfqmr \n")
      printf ("returned an all zero solution without iterating.\n")
    endif
    x_min = zeros (numel (b), 1);
    iter_min = 0;
    flag = 0;
    resvec = 0;
    relres = 0;
    return;
  endif

  x = x_pr = x_min = x0;
  iter = iter_min = m = 0;
  resvec = zeros (maxit, 1);
  flag = 1;

  w = u = r = r_star = b - feval (Afcn, x0, varargin{:});
  rho_1 = (r_star' * r);
  d = 0;
  tau = norm (r, 2);
  theta = eta = 0;
  resvec (1, 1) = norm (r, 2);
  it = 1;

  try
    warning ("error", "Octave:singular-matrix", "local");
    u_hat = feval (M1fcn, u, varargin{:});
    u_hat = feval (M2fcn, u_hat, varargin{:});
    v = feval (Afcn, u_hat, varargin{:});
  catch
    flag = 2;
  end_try_catch
  while ((flag != 2) && (iter < maxit) && ...
         (resvec (iter + 1, 1) >= norm_b * tol))
    if (it > 0) # iter is even
      v_r = r_star' * v; # inner prod between r_star and v
      if (v_r == 0)
        ## Essentially the next iteration doesn't change x,
        ## and the iter after this will have a division by zero
        flag = 4;
        break;
      endif
      alpha = rho_1 / v_r;
      u_1 = u - alpha * v;  # u at the after iteration
    endif
    u_hat = feval (M1fcn, u, varargin{:});
    u_hat = feval (M2fcn, u_hat, varargin{:});
    w -= alpha * feval (Afcn, u_hat, varargin{:});
    d = u_hat + ((theta * theta) / alpha) * eta * d;
    theta = norm (w, 2) / tau;
    c = 1 / sqrt (1 + theta * theta);
    tau *= theta * c;
    eta = (c * c) * alpha;
    x += eta * d;
    r -= eta * feval (Afcn, d, varargin{:});
    if (it < 0) # iter is odd
      rho_2 = rho_1;
      rho_1 = (r_star' * w);
      if (rho_1 == 0)
        ## Essentially the next iteration doesn't change x,
        ## and the iter after this will have a division by zero
        flag = 4;
        break;
      endif
      beta = rho_1 / rho_2;
      u_1 = w + beta * u; # u at the after iteration
      u1_hat = feval (M1fcn, u_1, varargin{:});
      u1_hat = feval (M2fcn, u1_hat, varargin{:});
      v = feval (Afcn, u1_hat, varargin{:}) + ...
          beta * (feval (Afcn, u_hat, varargin{:}) + beta * v);
    endif
    u = u_1;
    iter += 1;
    resvec (iter + 1, 1) = norm (r, 2);
    if (resvec (iter + 1, 1) <= resvec (iter_min + 1, 1))
      ## iter with min residual
      x_min = x;
      iter_min = iter;
    endif
    if (norm (x_pr - x) <= norm (x) * eps)
      flag = 3; # Stagnation
      break;
    endif
    x_pr = x;
    it = -it;
  endwhile
  resvec = resvec (1: (iter + 1));

  relres = resvec (iter_min + 1) / norm (b);
  iter_min = floor (iter_min / 2); # compatibility, since it
                                   # makes two times the effective iterations

  if (relres <= tol)
    flag = 0;
  endif

  if (nargout < 2) # Output strings
    switch (flag)
      case {0}
        printf ("tfqmr converged at iteration %i ", iter_min);
        printf ("to a solution with relative residual %e\n", relres);
      case {1}
        printf ("tfqmr stopped at iteration %i ", iter);
        printf ("without converging to the desired tolerance %e\n", tol);
        printf ("because the maximum number of iterations was reached.\n");
        printf ("The iterate returned (number %i) ", iter_min);
        printf ("has relative residual %e\n", relres);
      case {2}
        printf ("tfqmr stopped at iteration %i ", iter);
        printf ("without converging to the desired tolerance %e\n", tol);
        printf ("because the preconditioner matrix is singular.\n");
        printf ("The iterate returned (number %i) ", iter_min);
        printf ("has relative residual %e\n", relres);
      case {3}
        printf ("tfqmr stopped at iteration %i ", iter);
        printf ("without converging to the desired tolerance %e\n", tol);
        printf ("because the method stagnated.\n");
        printf ("The iterate returned (number %i) ", iter_min);
        printf ("has relative residual %e\n", relres);
      case {4}
        printf ("tfqmr stopped at iteration %i ", iter);
        printf ("without converging to the desired tolerance %e\n", tol);
        printf ("because the method can't continue.\n");
        printf ("The iterate returned (number %i) ", iter_min);
        printf ("has relative residual %e\n", relres);
    endswitch
  endif

endfunction


%!test
%! ## Check that all type of inputs work
%! A = toeplitz (sparse ([2, 1, 0, 0, 0]), sparse ([2, -1, 0, 0, 0]));
%! b = sum (A, 2);
%! M1 = diag (sqrt (diag (A)));
%! M2 = M1;
%! maxit = 10;
%! Afcn = @(z) A * z;
%! M1_fcn = @(z) M1 \ z;
%! M2_fcn = @(z) M2 \ z;
%! [x, flag] = tfqmr (A,b);
%! assert (flag, 0);
%! [x, flag] = tfqmr (A, b, [], maxit, M1, M2);
%! assert (flag, 0);
%! [x, flag] = tfqmr (A, b, [], maxit, M1_fcn, M2_fcn);
%! assert (flag, 0);
%! [x, flag] = tfqmr (A, b, [], maxit, M1_fcn, M2);
%! assert (flag, 0);
%! [x, flag] = tfqmr (A, b, [], maxit, M1, M2_fcn);
%! assert (flag, 0);
%! [x, flag] = tfqmr (Afcn, b);
%! assert (flag, 0);
%! [x, flag] = tfqmr (Afcn, b, [], maxit, M1, M2);
%! assert (flag, 0);
%! [x, flag] = tfqmr (Afcn, b, [], maxit, M1_fcn, M2);
%! assert (flag, 0);
%! [x, flag] = tfqmr (Afcn, b, [], maxit, M1, M2_fcn);
%! assert (flag, 0);
%! [x, flag] = tfqmr (Afcn, b, [], maxit, M1_fcn, M2_fcn);
%! assert (flag, 0);

%!shared A, b, n, M1, M2
%!
%!test
%! n = 100;
%! A = spdiags ([-2*ones(n,1) 4*ones(n,1) -ones(n,1)], -1:1, n, n);
%! b = sum (A, 2);
%! tol = 1e-8;
%! maxit = 15;
%! M1 = spdiags ([ones(n,1)/(-2) ones(n,1)],-1:0, n, n);
%! M2 = spdiags ([4*ones(n,1) -ones(n,1)], 0:1, n, n);
%! [x, flag, relres, iter, resvec] = tfqmr (A, b, tol, maxit, M1, M2);
%! assert (x, ones (size (b)), 1e-7);
%!
%!test
%!function y = afcn (x, a)
%!  y = a * x;
%!endfunction
%!
%! tol = 1e-8;
%! maxit = 15;
%!
%! [x, flag, relres, iter, resvec] = tfqmr (@(x) afcn (x, A), b,
%!                                          tol, maxit, M1, M2);
%! assert (x, ones (size (b)), 1e-7);

%!test
%! ## Jacobi preconditioner works
%! n = 10;
%! tol = 1e-8;
%! A = hilb (n) + 1i * hilb (n);
%! A(1,1) = 100;
%! A(n, n) = 100;
%! b = sum (A, 2);
%! [x, flag, relres, iter, resvec] = tfqmr (A, b, tol);
%! assert (x, ones (size (b)), 0.005);
%! assert (iter, 8);
%! [x, flag, relres, iter, resvec] = tfqmr (A, b, tol, [], diag (diag (A)));
%! assert (x, ones (size (b)), 0.002);
%! assert (iter, 6);

%!test
%! ## Solve complex linear system
%! A = [1 + 1i, 1 + 1i; 2 - 1i, 2 + 1i];
%! b = A * [1; 1];
%! [x, flag, relres, iter, resvec] = tfqmr (A, b, [], 3);
%! assert (x, [1; 1], 1e-6);

%!test
%! A = diag (1:50);
%! A (1,50) = 10000;
%! b = ones (50,1);
%! [x, flag, relres, iter, resvec] = tfqmr (A, b, [], 100);
%! assert (flag, 0);
%! assert (x, A \ b, 1e-05);
%! ## Detects a singular preconditioner
%! M = ones (50);
%! M(1, 1) = 0;
%! [x, flag] = tfqmr (A, b, [], 100, M);
%! assert (flag, 2);

%!test
%! A = single (1);
%! b = 1;
%! [x, flag] = tfqmr (A, b);
%! assert (class (x), "single");

%!test
%! A = 1;
%! b = single (1);
%! [x, flag] = tfqmr (A, b);
%! assert (class (x), "single");

%!test
%! A = single (1);
%! b = single (1);
%! [x, flag] = tfqmr (A, b);
%! assert (class (x), "single");

%!test
%!function y = Afcn (x)
%!  A = toeplitz ([2, 1, 0, 0], [2, -1, 0, 0]);
%!  y = A * x;
%!endfunction
%! [x, flag] = tfqmr ("Afcn", [1; 2; 2; 3]);
%! assert (x, ones (4, 1), 1e-6);

%!test # unpreconditioned residual
%! A = toeplitz (sparse ([2, 1, 0, 0, 0]), sparse ([2, -1, 0, 0, 0]));
%! b = sum (A, 2);
%! M = magic (5);
%! [x, flag, relres] = tfqmr (A, b, [], 3, M);
%! assert (relres, norm (b - A * x) / norm (b), 8 * eps);

%!demo # simplest use
%! n = 20;
%! A = toeplitz (sparse ([1, 1], [1, 2], [2, 1] * n ^ 2, 1, n))  + ...
%!     toeplitz (sparse (1, 2, -1, 1, n) * n / 2, ...
%!     sparse (1, 2, 1, 1, n) * n / 2);
%! b = A * ones (n, 1);
%! [M1, M2] = ilu (A + 0.1 * eye (n));
%! M = M1 * M2;
%! x = tfqmr (A, b, [], n);
%! Afcn = @(x) A * x;
%! x = tfqmr (Afcn, b, [], n);
%! x = tfqmr (A, b, 1e-6, n, M);
%! x = tfqmr (A, b, 1e-6, n, M1, M2);
%! Mfcn = @(z) M \ z;
%! x = tfqmr (Afcn, b, 1e-6, n, Mfcn);
%! M1fcn = @(z) M1 \ z;
%! M2fcn = @(z) M2 \ z;
%! x = tfqmr (Afcn, b, 1e-6, n, M1fcn, M2fcn);
%! function y = Ap (A, x, z) # compute A^z * x or (A^z)' * x
%!    y = x;
%!    for i = 1:z
%!      y = A * y;
%!    endfor
%!  endfunction
%! Afcn = @(x, p) Ap (A, x, p);
%! x = tfqmr (Afcn, b, [], 2*n, [], [], [], 2); # solution of A^2 * x = b

%!demo
%! n = 10;
%! A = toeplitz (sparse ([1, 1], [1, 2], [2, 1] * n ^ 2, 1, n))  + ...
%!     toeplitz (sparse (1, 2, -1, 1, n) * n / 2, ...
%!     sparse (1, 2, 1, 1, n) * n / 2);
%! b = A * ones (n, 1);
%! [M1, M2] = ilu (A + 0.3 * eye (n)); # factorization of A perturbed
%! M = M1 * M2;
%!
%! ## reference solution computed by tfqmr after one iteration
%! [x_ref, fl] = tfqmr (A, b, [], 1, M);
%! x_ref
%!
%! ## right preconditioning
%! [y, fl] = tfqmr (A / M, b, [], 1);
%! x = M \ y # compare x and x_ref
