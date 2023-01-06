########################################################################
##
## Copyright (C) 2004-2023 The Octave Project Developers
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
## @deftypefn  {} {@var{x} =} pcg (@var{A}, @var{b}, @var{tol}, @var{maxit}, @var{m1}, @var{m2}, @var{x0}, @dots{})
## @deftypefnx {} {@var{x} =} pcg (@var{A}, @var{b}, @var{tol}, @var{maxit}, @var{M}, [], @var{x0}, @dots{})
## @deftypefnx {} {[@var{x}, @var{flag}, @var{relres}, @var{iter}, @var{resvec}, @var{eigest}] =} pcg (@var{A}, @var{b}, @dots{})
##
## Solve the linear system of equations @w{@code{@var{A} * @var{x} = @var{b}}}
## by means of the Preconditioned Conjugate Gradient iterative method.
##
## The input arguments are:
##
## @itemize
## @item @var{A} is the matrix of the linear system and it must be square.
## @var{A} can be passed as a matrix, function handle, or inline function
## @code{Afcn} such that @code{Afcn(x) = A * x}.  Additional parameters to
## @code{Afcn} may be passed after @var{x0}.
##
## @var{A} has to be Hermitian and Positive Definite (@nospell{HPD})@.  If
## @code{pcg} detects @var{A} not to be positive definite, a warning is printed
## and the @var{flag} output is set.
##
## @item
## @var{b} is the right-hand side vector.
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
## @var{m} is a @nospell{HPD} preconditioning matrix.  For any decomposition
## @code{@var{m} = @var{p1} * @var{p2}} such that
## @w{@code{inv (@var{p1}) * @var{A} * inv (@var{p2})}} is @nospell{HPD}, the
## conjugate gradient method is formally applied to the linear system
## @w{@code{inv (@var{p1}) * @var{A} * inv (@var{p2}) * @var{y} = inv
## (@var{p1}) * @var{b}}},
## with @code{@var{x} = inv (@var{p2}) * @var{y}} (split preconditioning).
## In practice, at each iteration of the conjugate gradient method a
## linear system with matrix @var{m} is solved with @code{mldivide}.
## If a particular factorization
## @code{@var{m} = @var{m1} * @var{m2}} is available (for instance, an
## incomplete Cholesky factorization of @var{a}), the two matrices
## @var{m1} and @var{m2} can be passed and the relative linear systems
## are solved with the @code{mldivide} operator.
## Note that a proper choice of the preconditioner may dramatically improve
## the overall performance of the method.  Instead of matrices @var{m1} and
## @var{m2}, the user may pass two functions which return the results of
## applying the inverse of @var{m1} and @var{m2} to a vector.
## If @var{m1} is omitted or empty @code{[]}, then no preconditioning
## is applied.  If no factorization of @var{m} is available, @var{m2}
## can be omitted or left [], and the input variable @var{m1} can be
## used to pass the preconditioner @var{m}.
##
## @item
## @var{x0} is the initial guess.  If @var{x0} is omitted or empty then the
## function sets @var{x0} to a zero vector by default.
## @end itemize
##
## The arguments which follow @var{x0} are treated as parameters, and passed in
## an appropriate manner to any of the functions (@var{A} or @var{m1} or
## @var{m2}) that have been given to @code{pcg}.
## See the examples below for further details.
##
## The output arguments are:
##
## @itemize
## @item
## @var{x} is the computed approximation to the solution of
## @w{@code{@var{A} * @var{x} = @var{b}}}.  If the algorithm did not converge,
## then @var{x} is the iteration which has the minimum residual.
##
## @item
## @var{flag} reports on the convergence:
##
## @itemize
## @item 0: The algorithm converged to within the prescribed tolerance.
##
## @item 1: The algorithm did not converge and it reached the maximum
## number of iterations.
##
## @item 2: The preconditioner matrix is singular.
##
## @item 3: The algorithm stagnated, i.e., the absolute value of the
## difference between the current iteration @var{x} and the previous is less
## than @code{@var{eps} * norm (@var{x},2)}.
##
## @item 4: The algorithm detects that the input (preconditioned) matrix is not
## @nospell{HPD}.
## @end itemize
##
## @item
## @var{relres} is the ratio of the final residual to its initial value,
## measured in the Euclidean norm.
##
## @item
## @var{iter} indicates the iteration of @var{x} which it was
## computed.  Since the output @var{x} corresponds to the minimal
## residual solution, the total number of iterations that
## the method performed is given by @code{length(resvec) - 1}.
##
## @item
## @var{resvec} describes the convergence history of the method.
## @code{@var{resvec} (@var{i}, 1)} is the Euclidean norm of the residual, and
## @code{@var{resvec} (@var{i}, 2)} is the preconditioned residual
## norm, after the
## (@var{i}-1)-th iteration, @code{@var{i} = 1, 2, @dots{}, @var{iter}+1}.
## The preconditioned residual norm is defined as
## @code{@var{r}' * (@var{m} \ @var{r})} where
## @code{@var{r} = @var{b} - @var{A} * @var{x}}, see also the
## description of @var{m}.  If @var{eigest} is not required, only
## @code{@var{resvec} (:, 1)} is returned.
##
## @item
## @var{eigest} returns the estimate for the smallest @code{@var{eigest}(1)}
## and largest @code{@var{eigest}(2)} eigenvalues of the preconditioned matrix
## @w{@code{@var{P} = @var{m} \ @var{A}}}.  In particular, if no
## preconditioning is used, the estimates for the extreme eigenvalues of
## @var{A} are returned.  @code{@var{eigest}(1)} is an overestimate and
## @code{@var{eigest}(2)} is an underestimate, so that
## @code{@var{eigest}(2) / @var{eigest}(1)} is a lower bound for
## @code{cond (@var{P}, 2)}, which nevertheless in the limit should
## theoretically be equal to the actual value of the condition number.
## @end itemize
##
##
## Let us consider a trivial problem with a tridiagonal matrix
##
## @example
## @group
## n = 10;
## A = toeplitz (sparse ([1, 1], [1, 2], [2, 1], 1, n));
## b = A * ones (n, 1);
## M1 = ichol (A); # in this tridiagonal case it corresponds to chol (A)'
## M2 = M1';
## M = M1 * M2;
## Afcn = @@(x) A * x;
## Mfcn = @@(x) M \ x;
## M1fcn = @@(x) M1 \ x;
## M2fcn = @@(x) M2 \ x;
## @end group
## @end example
##
## @sc{Example 1:} Simplest use of @code{pcg}
##
## @example
## x = pcg (A, b)
## @end example
##
## @sc{Example 2:} @code{pcg} with a function which computes
## @code{@var{A} * @var{x}}
##
## @example
## x = pcg (Afcn, b)
## @end example
##
## @sc{Example 3:} @code{pcg} with a preconditioner matrix @var{M}
##
## @example
## x = pcg (A, b, 1e-06, 100, M)
## @end example
##
## @sc{Example 4:} @code{pcg} with a function as preconditioner
##
## @example
## x = pcg (Afcn, b, 1e-6, 100, Mfcn)
## @end example
##
## @sc{Example 5:} @code{pcg} with preconditioner matrices @var{M1}
## and @var{M2}
##
## @example
## x = pcg (A, b, 1e-6, 100, M1, M2)
## @end example
##
## @sc{Example 6:} @code{pcg} with functions as preconditioners
##
## @example
## x = pcg (Afcn, b, 1e-6, 100, M1fcn, M2fcn)
## @end example
##
## @sc{Example 7:} @code{pcg} with as input a function requiring an argument
##
## @example
## @group
##   function y = Ap (A, x, p) # compute A^p * x
##      y = x;
##      for i = 1:p
##        y = A * y;
##      endfor
##   endfunction
## Apfcn = @@(x, p) Ap (A, x, p);
## x = pcg (Apfcn, b, [], [], [], [], [], 2);
## @end group
## @end example
##
## @sc{Example 8:} explicit example to show that @code{pcg} uses a
## split preconditioner
##
## @example
## @group
## M1 = ichol (A + 0.1 * eye (n)); # factorization of A perturbed
## M2 = M1';
## M = M1 * M2;
##
## ## reference solution computed by pcg after two iterations
## [x_ref, fl] = pcg (A, b, [], 2, M)
##
## ## split preconditioning
## [y, fl] = pcg ((M1 \ A) / M2, M1 \ b, [], 2)
## x = M2 \ y # compare x and x_ref
##
## @end group
## @end example
##
## References:
##
## @enumerate
## @item
## C.T. Kelley, @cite{Iterative Methods for Linear and Nonlinear Equations},
## SIAM, 1995. (the base PCG algorithm)
##
## @item
## @nospell{Y. Saad}, @cite{Iterative Methods for Sparse Linear Systems},
## @nospell{PWS} 1996. (condition number estimate from PCG)
## Revised version of this book is available online at
## @url{https://www-users.cs.umn.edu/~saad/books.html}
## @end enumerate
##
## @seealso{sparse, pcr, gmres, bicg, bicgstab, cgs}
## @end deftypefn

function [x_min, flag, relres, iter_min, resvec, eigest] =...
         pcg (A, b, tol = [], maxit = [], M1 = [], M2 = [], x0 = [], varargin)

  ## Insert the default input (if necessary)
  [tol, maxit, x0] = __default__input__ ({1e-6, min(rows (b), 20),...
                                          zeros(size (b))}, tol, maxit, x0);

  if (tol >= 1)
    warning ("Input tol is bigger than 1. \n Try to use a smaller tolerance.");
  elseif (tol <= eps / 2)
    warning ("Input tol may not be achievable by pcg. \n Try to use a bigger tolerance");
  endif

  ## Check if the input data A,b,m1,m2 are consistent (i.e. if they are
  ## matrix or function handle)

  [Afcn, M1fcn, M2fcn] = __alltohandles__ (A, b, M1, M2, "pcg");

  maxit += 2;
  n_arg_out = nargout;

  ## Set Initial data
  b_norm = norm (b);
  if (b_norm == 0)
     if (n_arg_out < 2)
       printf ("The right hand side vector is all zero so pcg \n");
       printf ("returned an all zero solution without iterating.\n");
     endif
     x_min = b;
     flag = 0;
     relres = 0;
     resvec = 0;
     iter_min = 0;
     eigest = [NaN, NaN];
     return;
  endif

  x = x_pr = x_min = x0;

  ## x_pr (x previous) needs to check the stagnation
  ## x_min needs to save the iterated with minimum residual

  r = b - feval (Afcn, x, varargin{:});
  iter = 2;
  iter_min = 0;
  flag = 1;
  resvec = zeros (maxit + 1, 2);
  resvec(1, 1) = norm (r);
  p = zeros (size (b));
  alpha = old_tau = 1;

  if (n_arg_out > 5)
    T = zeros (maxit, maxit);
  else
    T = [];
  endif

  while (resvec(iter-1,1) > tol * b_norm && iter < maxit)
    if (iter == 2) # Check whether M1 or M2 are singular
      try
        warning ("error","Octave:singular-matrix","local");
        z = feval (M1fcn, r, varargin{:});
        z = feval (M2fcn, z, varargin{:});
      catch
        flag = 2;
        break;
      end_try_catch
    else
      z = feval (M1fcn, r, varargin{:});
      z = feval (M2fcn, z, varargin{:});
    endif

    tau = z' * r;
    resvec(iter - 1, 2) = sqrt (tau);
    beta = tau / old_tau;
    old_tau = tau;
    p = z + beta * p;
    w = feval (Afcn, p, varargin{:});

    ## Needed only for eigest.

    old_alpha = alpha;
    den = p' * w;
    alpha = tau / den;

    ## Check if alpha is negative and/or if it has a consistent
    ## imaginary part: if yes then A probably is not positive definite
    if ((abs (imag (tau)) >= abs (real (tau)) * tol) || ...
        real (tau) <= 0 || ...
        (abs (imag (den)) >= abs (real (den)) * tol) || ...
        (real (den) <= 0))
      flag = 4;
      break;
    endif

    x += alpha * p;
    r -= alpha * w;
    resvec(iter, 1) = norm (r);
    ## Check if the iterated has minimum residual
    if (resvec (iter,1) <= resvec (iter_min + 1,1))
      x_min = x;
      iter_min = iter - 1;
    endif
    if (n_arg_out > 5 && iter > 2)
      T(iter-1:iter, iter-1:iter) = T(iter-1:iter, iter-1:iter) + ...
                                    [1, sqrt(beta); sqrt(beta), beta] ./ ...
                                    old_alpha;
    endif
    iter += 1;
    if (norm (x - x_pr) <= eps * norm (x)) # Check the stagnation
      flag = 3;
      break;
    endif
    x_pr = x;
  endwhile

  if (n_arg_out > 5)
  ## Apply the preconditioner once more and finish with the precond
  ## residual.
    z = feval (M1fcn, r, varargin{:});
    z = feval (M2fcn, z, varargin{:});
  endif

  ## (Eventually) computes the eigenvalue of inv(m2)*inv(m1)*A
  if (n_arg_out > 5)
    if (flag != 4)
      if (iter > 3)
        T = T(2:iter-2,2:iter-2);
        l = eig (T);
        eigest = [min(l), max(l)];
      else
        eigest = [NaN, NaN];
        warning ("pcg: eigenvalue estimate failed: iteration converged too fast");
      endif
    else
      eigest = [NaN, NaN];
      warning ('pcg: eigenvalue estimate failed: matrix not positive definite?');
    endif
    resvec(iter - 1, 2) = sqrt (r' * z);
    resvec  = resvec (1:(iter-1), :);
  else
    eigest = [NaN, NaN];
    resvec = resvec(1:(iter-1),1);
  endif

  ## Set the last variables

  if (flag == 2)
    relres = 1;
  elseif (resvec (1, 1) == 0)
    relres = 0;
  else
    relres = resvec(iter_min+1, 1) ./ b_norm;
  endif

  iter -= 2; # compatibility

  ## Set the flag in the proper way if flag not 3, 4 or 2
  if (flag == 2)
    flag = 2;
  elseif (flag == 1) && (relres <= tol)
    flag = 0;
  endif

  if (n_arg_out < 2)
    switch (flag)
      case {0}
        printf ("pcg converged at iteration %d ", iter_min);
        printf ("with relative residual %d\n", relres);
      case {1}
        printf ("pcg stopped at iteration %d ", iter+1);
        printf ("without converging to the desired tolerance %d ", tol);
        printf ("because the maximum number of iteration was reached, \n");
        printf ("The iterated returned (number %d) ",iter_min);
        printf ("has relative residual %d \n", relres);
      case {2}
        printf ("pcg stopped at iteration %d ", iter+1)
        printf ("without converging to the desired tolerance %d ", tol);
        printf ("because the preconditioned matrix is singular.\n");
        printf ("The iterated returned (number %d) ", iter_min);
        printf ("has relative residual %d \n", relres);
      case {3}
        printf ("pcg stopped at iteration %d ", iter+1);
        printf ("without converging to the desired tolerance %d ", tol);
        printf ("because of stagnation. \n");
        printf ("The iterated returned (number %d) ", iter_min);
        printf ("has relative residual %d.\n", relres);
      case {4}
        printf ("pcg stopped at iteration %d ", iter + 1);
        printf ("without converging to the desired tolerance %d ",tol);
        printf ("because the (preconditioned) matrix is not positive definite. \n");
        printf ("The iterate returned (number %d) ", iter_min);
        printf ("has relative residual %d \n", relres);
    endswitch
  endif

endfunction


%!demo # simplest use
%! n = 10;
%! A = toeplitz (sparse ([1, 1], [1, 2], [2, 1], 1, n));
%! b = A * ones (n, 1);
%! M1 = ichol (A);  # for this tridiagonal case it corresponds to chol (A)'
%! M2 = M1';
%! M = M1 * M2;
%! x = pcg (A, b);
%! Afcn = @(x) A * x;
%! x = pcg (Afcn, b);
%! x = pcg (A, b, 1e-6, 100, M);
%! x = pcg (A, b, 1e-6, 100, M1, M2);
%! Mfcn = @(x) M \ x;
%! x = pcg (Afcn, b, 1e-6, 100, Mfcn);
%! M1fcn = @(x) M1 \ x;
%! M2fcn = @(x) M2 \ x;
%! x = pcg (Afcn, b, 1e-6, 100, M1fcn, M2fcn);
%! function y = Ap (A, x, p)  # compute A^p * x
%!    y = x;
%!    for i = 1:p
%!      y = A * y;
%!    endfor
%!  endfunction
%! Afcn = @(x, p) Ap (A, x, p);
%! ## solution of A^2 * x = b
%! x = pcg (Afcn, b, [], [], [], [], [], 2);

%!demo
%! n = 10;
%! A = toeplitz (sparse ([1, 1], [1, 2], [2, 1], 1, n));
%! b = A * ones (n, 1);
%! M1 = ichol (A + 0.1 * eye (n));  # Perturb the factorization of A
%! M2 = M1';
%! M = M1 * M2;
%!
%! ## Reference solution computed by pcg after two iterations
%! [x_ref, fl] = pcg (A, b, [], 2, M);
%! x_ref
%!
%! ## Split preconditioning
%! [y, fl] = pcg ((M1 \ A) / M2, M1 \ b, [], 2);
%! x = M2 \ y  # compare x and x_ref
%!test
%! ## Check that all type of inputs work
%! A = toeplitz (sparse ([2, 1 ,0, 0, 0]));
%! b = A * ones (5, 1);
%! M1 = diag (sqrt (diag (A)));
%! M2 = M1;  # M1 * M2 is the Jacobi preconditioner
%! Afcn = @(z) A*z;
%! M1_fcn = @(z) M1 \ z;
%! M2_fcn = @(z) M2 \ z;
%! [x, flag, ~, iter] = pcg (A,b);
%! assert (flag, 0);
%! [x, flag, ~ , iter] = pcg (A, b, [], [], M1 * M2);
%! assert (flag, 0);
%! [x, flag, ~ , iter] = pcg (A, b, [], [], M1, M2);
%! assert (flag, 0);
%! [x, flag] = pcg (A, b, [], [], M1_fcn, M2_fcn);
%! assert (flag, 0);
%! [x, flag] = pcg (A, b,[],[], M1_fcn, M2);
%! assert (flag, 0);
%! [x, flag] = pcg (A, b,[],[], M1, M2_fcn);
%! assert (flag, 0);
%! [x, flag] = pcg (Afcn, b);
%! assert (flag, 0);
%! [x, flag] = pcg (Afcn, b,[],[], M1 * M2);
%! assert (flag, 0);
%! [x, flag] = pcg (Afcn, b,[],[], M1, M2);
%! assert (flag, 0);
%! [x, flag] = pcg (Afcn, b,[],[], M1_fcn, M2);
%! assert (flag, 0);
%! [x, flag] = pcg (Afcn, b,[],[], M1, M2_fcn);
%! assert (flag, 0);
%! [x, flag] = pcg (Afcn, b,[],[], M1_fcn, M2_fcn);
%! assert (flag, 0);

%!test
%! ## solve a small diagonal system
%! N = 10;
%! A = diag ([1:N]);  b = rand (N, 1);
%! [x, flag] = pcg (A, b, [], N+1);
%! assert (flag, 0);
%! assert (norm (b - A*x) / norm (b), 0, 1e-6);

%!test
%! ## A is not positive definite
%! ## The indefiniteness of A is detected.
%! N = 10;
%! A = -diag ([1:N]);  b = sum (A, 2);
%! [x, flag] = pcg (A, b, [], N + 1);
%! assert (flag, 4);

%!test
%! ## solve tridiagonal system, do not converge in default 20 iterations
%! N = 100;
%! ## Form 1-D Laplacian matrix
%! A = 2 * eye (N,N);
%! A(2:(N+1):end) = -1;
%! A((N+1):(N+1):end) = -1;
%! b = ones (N, 1);
%! [x, flag, relres, iter, resvec, eigest] = pcg (A, b, 1e-12);
%! assert (flag);
%! assert (relres >= 1.0);

%!warning <iteration converged too fast>
%! ## solve tridiagonal system with "perfect" preconditioner which converges
%! ## in one iteration, so the eigest does not work and issues a warning.
%! N = 100;
%! ## Form 1-D Laplacian matrix
%! A = 2 * eye (N,N);
%! A(2:(N+1):end) = -1;
%! A((N+1):(N+1):end) = -1;
%! b = ones (N, 1);
%! [x, flag, relres, iter, resvec, eigest] = pcg (A, b, [], [], A, [], b);
%! assert (flag, 0);
%! assert (norm (b - A*x) / norm (b), 0, 1e-6);
%!
%! assert (isnan (eigest), isnan ([NaN, NaN]));

%!test
%! ## pcg detect a non-Hermitian matrix, with a considerable imaginary part.
%! ## In this example, Matlab does not recognize the wrong type of matrix and
%! ## makes iterations until it reaches maxit.
%! N = 10;
%! A = diag (1:N) + 1e-4*i;
%! b = ones (N, 1);
%! [x, flag] = pcg (A, b, []);
%! assert (flag, 4);

%!test
%! ## The imaginary part is not influent (it is too small), so pcg doesn't stop
%! N = 10;
%! A = diag (1:N) + 1e-10*i;
%! b = ones (N, 1);
%! [x, flag] = pcg (A, b, [], N+1);
%! assert (flag, 0);
%! assert (norm (b - A*x) / norm (b), 0, 1e-6);

%!test
%! ## pcg solves linear system with A Hermitian positive definite
%! N = 20;
%! A = sparse (toeplitz ([4, 1, zeros(1, 18)])) + ...
%!     i * sparse (toeplitz ([0, 1, zeros(1, 18)], [0, -1, zeros(1,18)]));
%! b = A * ones (N, 1);
%! Hermitian_A = ishermitian (A);
%! [x, flag] = pcg (A, b, [], 2*N);
%! assert (Hermitian_A, true);
%! assert (flag, 0);
%! assert (x, ones (N, 1), -1e-4);

%!testif HAVE_CHOLMOD
%! ## pcg solves preconditioned linear system with A HPD
%! N = 20;
%! A = sparse (toeplitz ([4, 1, zeros(1, 18)])) + ...
%!     i * sparse (toeplitz ([0, 1, zeros(1, 18)], [0, -1, zeros(1,18)]));
%! b = A * ones (N, 1);
%! M2 = chol (A + 0.1 * eye (N));  # Factor of a perturbed matrix
%! M = M2' * M2;
%! Hermitian_A = ishermitian (A);
%! Hermitian_M = ishermitian (M);
%! [x, flag] = pcg (A, b, [], 2*N, M);
%! assert (Hermitian_A, true);
%! assert (Hermitian_M, true);
%! assert (flag, 0);
%! assert (x, ones (N, 1), -1e-4);

%!test
%! ## pcg recognizes that the preconditioner matrix is singular
%! N = 3;
%! A = toeplitz ([2, 1, 0]);
%! M = [1 0 0; 0 1 0; 0 0 0];  # the last row is zero
%! [x, flag] = pcg (A, ones (3, 1), [], [], M);
%! assert (flag, 2);

%!test
%! A = rand (4);
%! A = A' * A;
%! [x, flag] = pcg (A, zeros (4, 1), [], [], [], [], ones (4, 1));
%! assert (x, zeros (4, 1));

## Test return types
%!test
%! A = single (1);
%! b = 1;
%! [x, flag] = pcg (A, b);
%! assert (class (x), "single");

%!test
%! A = 1;
%! b = single (1);
%! [x, flag] = pcg (A, b);
%! assert (class (x), "single");

%!test
%! A = single (1);
%! b = single (1);
%! [x, flag] = pcg (A, b);
%! assert (class (x), "single");

%!test
%!function y = Afcn (x)
%!   A = toeplitz ([2, 1, 0, 0]);
%!   y = A * x;
%!endfunction
%! [x, flag] = pcg ("Afcn", [3; 4; 4; 3]);
%! assert (x, ones (4, 1), 1e-6);

%!test
%! ## unpreconditioned residual
%! A = toeplitz (sparse ([4, 1, 0, 0, 0]));
%! b = sum (A, 2);
%! M = toeplitz (sparse ([2, 1, 0, 0, 0]));
%! [x, flag, relres] = pcg (A, b, [], 2, M);
%! assert (norm (b - A * x) / norm (b), relres,  8 * eps);

%!test <*59776>
%! A = [ 1.00000000  -0.00054274  -0.00066848;
%!      -0.00054274   1.00000000  -0.00060330;
%!      -0.00066848  -0.00060330   1.00000000];
%! b = [1 1 1]';
%! [x, flag, relres, iter, resvec] = pcg (A, b, 1e-6, 4, [], [], [1; 1; 1]);
%! assert (flag, 0);
%! assert (relres, resvec(2) / norm (b));
%! assert (iter, 1);

