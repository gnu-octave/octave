########################################################################
##
## Copyright (C) 2009-2023 The Octave Project Developers
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
## @deftypefn  {} {@var{x} =} gmres (@var{A}, @var{b}, @var{restart}, @var{tol}, @var{maxit}, @var{M1}, @var{M2}, @var{x0}, @dots{})
## @deftypefnx {} {@var{x} =} gmres (@var{A}, @var{b}, @var{restart}, @var{tol}, @var{maxit}, @var{M}, [], @var{x0}, @dots{})
## @deftypefnx {} {[@var{x}, @var{flag}, @var{relres}, @var{iter}, @var{resvec}] =} gmres (@var{A}, @var{b}, @dots{})
## Solve @code{A x = b} using the Preconditioned GMRES iterative method with
## restart, a.k.a. PGMRES(restart).
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
## with the same numbers of rows as @var{A}.
##
## @item @var{restart} is the number of iterations before that the
## method restarts.  If it is [] or N = numel (b), then the restart
## is not applied.
##
## @item @var{tol} is the required relative tolerance for the
## preconditioned residual error,
## @code{inv (@var{M}) * (@var{b} - @var{a} * @var{x})}.  The iteration
## stops if @code{norm (inv (@var{M}) * (@var{b} - @var{a} * @var{x}))
## @leq{} @var{tol} * norm (inv (@var{M}) * @var{B})}.  If @var{tol} is
## omitted or empty, then a tolerance of 1e-6 is used.
##
## @item @var{maxit} is the maximum number of outer iterations, if not given or
## set to [], then the default value @code{min (10, @var{N} / @var{restart})}
## is used.
## Note that, if @var{restart} is empty, then @var{maxit} is the maximum number
## of iterations.  If @var{restart} and @var{maxit} are not empty, then
## the maximum number of iterations is @code{@var{restart} * @var{maxit}}.
## If both @var{restart} and @var{maxit} are empty, then the maximum
## number of iterations is set to @code{min (10, @var{N})}.
##
## @item @var{M1}, @var{M2} are the preconditioners.  The preconditioner
## @var{M} is given as @code{M = M1 * M2}.  Both @var{M1} and @var{M2} can
## be passed as a matrix, function handle, or inline function @code{g} such
## that @code{g(x) = M1 \ x} or @code{g(x) = M2 \ x}.  If @var{M1} is [] or not
## given, then the preconditioner is not applied.
## The technique used is the left-preconditioning, i.e., it is solved
## @code{inv(@var{M}) * @var{A} * @var{x} = inv(@var{M}) * @var{b}} instead of
## @code{@var{A} * @var{x} = @var{b}}.
##
## @item @var{x0} is the initial guess,
## if not given or set to [], then the default value
## @code{zeros (size (@var{b}))} is used.
##
## @end itemize
##
## The arguments which follow @var{x0} are treated as parameters, and passed in
## a proper way to any of the functions (@var{A} or @var{M} or
## @var{M1} or @var{M2}) which are passed to @code{gmres}.
##
## The outputs are:
##
## @itemize @minus
##
## @item @var{x} the computed approximation.  If the method does not
## converge, then it is the iterated with minimum residual.
##
## @item @var{flag} indicates the exit status:
##
## @table @asis
## @item 0 : iteration converged to within the specified tolerance
##
## @item 1 : maximum number of iterations exceeded
##
## @item 2 : the preconditioner matrix is singular
##
## @item 3 : algorithm reached stagnation (the relative difference between two
## consecutive iterations is less than eps)
## @end table
##
## @item @var{relres} is the value of the relative preconditioned
## residual of the approximation @var{x}.
##
## @item @var{iter} is a vector containing the number of outer iterations and
## inner iterations performed to compute @var{x}.  That is:
##
## @itemize
## @item @var{iter(1)}: number of outer iterations, i.e., how many
## times the method restarted.  (if @var{restart} is empty or @var{N},
## then it is 1, if not 1 @leq{} @var{iter(1)} @leq{} @var{maxit}).
##
## @item @var{iter(2)}: the number of iterations performed before the
## restart, i.e., the method restarts when
## @code{@var{iter(2)} = @var{restart}}.  If @var{restart} is empty or
## @var{N}, then 1 @leq{} @var{iter(2)} @leq{} @var{maxit}.
## @end itemize
##
## To be more clear, the approximation @var{x} is computed at the iteration
## @code{(@var{iter(1)} - 1) * @var{restart} + @var{iter(2)}}.
## Since the output @var{x} corresponds to the minimal preconditioned
## residual solution, the total number of iterations that
## the method performed is given by @code{length (resvec) - 1}.
##
## @item @var{resvec} is a vector containing the preconditioned
## relative residual at each iteration, including the 0-th iteration
## @code{norm (@var{A} * @var{x0} - @var{b})}.
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
## @sc{Example 1:} simplest usage of @code{gmres}
##
## @example
## x = gmres (A, b, [], [], n)
## @end example
##
## @sc{Example 2:} @code{gmres} with a function which computes
## @code{@var{A} * @var{x}}
##
## @example
## x = gmres (Afcn, b, [], [], n)
## @end example
##
## @sc{Example 3:} usage of @code{gmres} with the restart
##
## @example
## x = gmres (A, b, restart);
## @end example
##
## @sc{Example 4:} @code{gmres} with a preconditioner matrix @var{M}
## with and without restart
##
## @example
## @group
## x = gmres (A, b, [], 1e-06, n, M)
## x = gmres (A, b, restart, 1e-06, n, M)
## @end group
## @end example
##
## @sc{Example 5:} @code{gmres} with a function as preconditioner
##
## @example
## x = gmres (Afcn, b, [], 1e-6, n, Mfcn)
## @end example
##
## @sc{Example 6:} @code{gmres} with preconditioner matrices @var{M1}
## and @var{M2}
##
## @example
## x = gmres (A, b, [], 1e-6, n, M1, M2)
## @end example
##
## @sc{Example 7:} @code{gmres} with functions as preconditioners
##
## @example
## x = gmres (Afcn, b, 1e-6, n, M1fcn, M2fcn)
## @end example
##
## @sc{Example 8:} @code{gmres} with as input a function requiring an argument
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
## x = gmres (Apfcn, b, [], [], [], [], [], [], 2);
## @end group
## @end example
##
## @sc{Example 9:} explicit example to show that @code{gmres} uses a
## left preconditioner
##
## @example
## @group
## [M1, M2] = ilu (A + 0.1 * eye (n)); # factorization of A perturbed
## M = M1 * M2;
##
## ## reference solution computed by gmres after two iterations
## [x_ref, fl] = gmres (A, b, [], [], 1, M)
##
## ## left preconditioning
## [x, fl] = gmres (M \ A, M \ b, [], [], 1)
## x # compare x and x_ref
##
## @end group
## @end example
##
## Reference:
##
## @nospell{Y. Saad}, @cite{Iterative Methods for Sparse Linear
## Systems}, Second edition, 2003, SIAM
##
## @seealso{bicg, bicgstab, cgs, pcg, pcr, qmr, tfqmr}
## @end deftypefn


function [x_min, flag, relres, it, resvec] = ...
         gmres (A, b, restart = [], tol = [], maxit = [], M1 = [],
                M2 = [], x0 = [], varargin)

  if (strcmp (class (A), "single") || strcmp (class (b), "single"))
    class_name = "single";
  else
    class_name = "double";
  endif

  [Afcn, M1fcn, M2fcn] = __alltohandles__ (A, b, M1, M2, "gmres");

  ## Check if the inputs are empty, and in case set them
  [tol, x0] = __default__input__ ({1e-06, zeros(size (b))}, tol, x0);

  empty_restart = isempty (restart);
  empty_maxit = isempty (maxit);
  size_b = rows (b);

  if (tol >= 1)
    warning ("Input tol is bigger than 1. \n Try to use a smaller tolerance.");
  elseif (tol <= eps / 2)
    warning ("Input tol may not be achievable by gmres. \n Try to use a bigger tolerance.");
  endif

  ## This big "if block" is to set maxit and restart in the proper way

  if ((empty_restart) && (empty_maxit))
    restart = size_b;
    maxit = 1;
    max_iter_number = min (size_b, 10);
  elseif (restart <= 0) || (maxit <= 0)
    error ("gmres: MAXIT and RESTART must be positive integers");
  elseif (restart < size_b) && (empty_maxit)
    maxit = min (size_b / restart, 10);
    max_iter_number = maxit * restart;
  elseif (restart == size_b) && (empty_maxit)
    maxit = 1;
    max_iter_number = min (size_b, 10);
  elseif (restart > size_b) && (empty_maxit)
    warning ("RESTART is %d but it should be bounded by SIZE(A,2).\n Setting restart to %d. \n", restart, size_b);
    restart = size_b;
    maxit = 1;
    max_iter_number = restart;
  elseif (empty_restart) && (maxit <= size_b)
    restart = size_b;
    max_iter_number = maxit;
  elseif (empty_restart) && (maxit > size_b)
    warning ("MAXIT is %d but it should be bounded by SIZE(A,2). \n Setting MAXIT to %d", maxit, size_b);
    restart = size_b;
    maxit = size_b;
    max_iter_number = size_b;
  elseif (restart > size_b) && (! empty_maxit)
    warning ("RESTART is %d but it should be bounded by SIZE(A,2).\n Setting restart to %d. \n", restart, size_b);
    restart = size_b;
    max_iter_number = restart * maxit;
  elseif (restart == size_b) && (maxit <= size_b)
    max_iter_number = maxit;
  else
    max_iter_number = restart*maxit;
  endif

  prec_b_norm = norm (b, 2);
  if (prec_b_norm == 0)
    if (nargout < 2)
      printf ("The right hand side vector is all zero so gmres\nreturned an all zero solution without iterating.\n")
    endif
    x_min = b;
    flag = 0;
    relres = 0;
    resvec = 0;
    it = [0, 0];
    return;
  endif

  ## gmres: function handle case

  x_old = x_pr = x_min = x = x0;
  B = zeros (restart + 1, 1);
  V = zeros (rows (x), restart, class_name);
  H = zeros (restart + 1, restart);

  iter = 1; # total number of iterations
  iter_min = 0; # iteration with minimum residual
  outer_it = 1; # number of outer iterations
  restart_it  =  1; # number of inner iterations
  it = zeros (1, 2);
  resvec = zeros (max_iter_number + 1, 1);
  flag = 1; # Default flag is maximum # of iterations exceeded

  ## begin loop
  u = feval (Afcn, x_old, varargin{:});
  try
    warning ("error", "Octave:singular-matrix", "local");
    prec_res = feval (M1fcn, b - u, varargin{:});  # M1*(b-u)
    prec_res = feval (M2fcn, prec_res, varargin{:});
    presn = norm (prec_res, 2);
    resvec(1) = presn;
    z = feval (M1fcn, b, varargin{:});
    z = feval (M2fcn, z, varargin{:});
    prec_b_norm = norm (z, 2);
    B (1) = presn;
    V(:, 1) = prec_res / presn;
  catch
    flag = 2;
  end_try_catch

  while (flag != 2) && (iter <= max_iter_number) && ...
        (presn > tol * prec_b_norm)
    ## restart
    if (restart_it > restart)
      restart_it = 1;
      outer_it += 1;
      x_old = x;
      u = feval (Afcn, x_old, varargin{:});
      prec_res = feval (M1fcn, b - u, varargin{:});
      prec_res = feval (M2fcn, prec_res, varargin{:});
      presn = norm (prec_res, 2);
      B(1) = presn;
      H(:) = 0;
      V(:, 1) = prec_res / presn;
    endif
    ## basic iteration
    u = feval (Afcn, V(:, restart_it), varargin{:});
    tmp = feval (M1fcn, u, varargin{:});
    tmp = feval (M2fcn, tmp, varargin{:});
    [V(:,restart_it + 1), H(1:restart_it + 1, restart_it)] = ...
      mgorth (tmp, V(:,1:restart_it));
    Y = (H(1:restart_it + 1, 1:restart_it) \ B(1:restart_it + 1));
    little_res = B(1:restart_it + 1) - ...
                 H(1:restart_it + 1, 1:restart_it) * Y(1:restart_it);
    presn = norm (little_res, 2);
    x = x_old + V(:, 1:restart_it) * Y(1:restart_it);
    resvec(iter + 1) = presn;
    if (norm (x - x_pr) <= eps*norm (x))
      flag = 3; # Stagnation: little change between iterations
      break;
    endif
    if (resvec (iter + 1) <= resvec (iter_min + 1))
      x_min = x;
      iter_min = iter;
      it = [outer_it, restart_it];
    endif
    x_pr = x;
    restart_it += 1;
    iter += 1;
  endwhile

  if (flag == 2)
    resvec = norm (b);
    relres = 1;
  else
    resvec = resvec (1:iter);
    relres = resvec (iter) / prec_b_norm;
  endif

  if ((relres <= tol) && (flag == 1))
    flag = 0;  # Converged to solution within tolerance
  endif

  if ((nargout < 2) && (restart != size_b)) # restart applied
    switch (flag)
      case {0} # gmres converged
        printf ("gmres (%d) converged at outer iteration %d (inner iteration %d) ",restart, it (1), it (2));
        printf ("to a solution with relative residual %d \n", relres);
      case {1} # max number of iteration reached
        printf ("gmres (%d) stopped at outer iteration %d (inner iteration %d) ", restart, outer_it, restart_it-1);
        printf ("without converging to the desired tolerance %d ", tol);
        printf ("because the maximum number of iterations was reached \n");
        printf ("The iterated returned (number %d(%d)) ", it(1), it(2));
        printf ("has relative residual %d \n", relres);
      case {2} # preconditioner singular
        printf ("gmres (%d) stopped at outer iteration %d (inner iteration %d) ",restart, outer_it, restart_it-1);
        printf ("without converging to the desired tolerance %d ", tol);
        printf ("because the preconditioner matrix is singular \n");
        printf ("The iterated returned (number %d(%d)) ", it(1), it(2));
        printf ("has relative residual %d \n", relres);
      case {3} # stagnation
        printf ("gmres (%d) stopped at outer iteration %d (inner iteration %d) ", restart, outer_it, restart_it - 1);
        printf ("without converging to the desired tolerance %d", tol);
        printf ("because it stagnates. \n");
        printf ("The iterated returned (number %d(%d)) ", it(1), it(2));
        printf ("has relative residual %d \n", relres);
    endswitch
  elseif ((nargout < 2) && (restart == size_b)) # no restart
    switch (flag)
      case {0} # gmres converged
        printf ("gmres converged at iteration %d ", it(2));
        printf ("to a solution with relative residual %d \n", relres);
      case {1} # max number of iteration reached
        printf ("gmres stopped at iteration %d ", restart_it - 1);
        printf ("without converging to the desired tolerance %d ", tol);
        printf ("because the maximum number of iterations was reached \n");
        printf ("The iterated returned (number %d) ", it(2));
        printf ("has relative residual %d \n", relres);
      case {2} # preconditioner ill-conditioned
        printf ("gmres stopped at iteration %d ", restart_it - 1);
        printf ("without converging to the desired tolerance %d ", tol);
        printf ("because the preconditioner matrix is singular \n")
        printf ("The iterated returned (number %d) ", it (2));
        printf ("has relative residual %d \n", relres);
      case {3} # stagnation
        printf ("gmres stopped at iteration %d ", restart_it - 1);
        printf ("without converging at the desired tolerance %d ", tol);
        printf ("because it stagnates\n");
        printf ("The iterated returned (number %d) ", it(2));
        printf ("has relative residual %d \n", relres);
    endswitch
  endif

endfunction


%!demo
%! dim = 20;
%! A = spdiags ([-ones(dim,1) 2*ones(dim,1) ones(dim,1)], [-1:1], dim, dim);
%! b = ones (dim, 1);
%! [x, flag, relres, iter, resvec] = ...
%!   gmres (A, b, 10, 1e-10, dim, @(x) x ./ diag (A), [], b)

%!demo # simplest use
%! n = 20;
%! A = toeplitz (sparse ([1, 1], [1, 2], [2, 1] * n ^ 2, 1, n))  + ...
%!     toeplitz (sparse (1, 2, -1, 1, n) * n / 2, ...
%!     sparse (1, 2, 1, 1, n) * n / 2);
%! b = A * ones (n, 1);
%! restart = 5;
%! [M1, M2] = ilu (A + 0.1 * eye (n));
%! M = M1 * M2;
%! x = gmres (A, b, [], [], n);
%! x = gmres (A, b, restart, [], n);  # gmres with restart
%! Afcn = @(x) A * x;
%! x = gmres (Afcn, b, [], [], n);
%! x = gmres (A, b, [], 1e-6, n, M);  # gmres without restart
%! x = gmres (A, b, [], 1e-6, n, M1, M2);
%! Mfcn = @(x) M \ x;
%! x = gmres (Afcn, b, [], 1e-6, n, Mfcn);
%! M1fcn = @(x) M1 \ x;
%! M2fcn = @(x) M2 \ x;
%! x = gmres (Afcn, b, [], 1e-6, n, M1fcn, M2fcn);
%! function y = Ap (A, x, p)  # compute A^p * x
%!    y = x;
%!    for i = 1:p
%!      y = A * y;
%!    endfor
%!  endfunction
%! Afcn = @(x, p) Ap (A, x, p);
%! x = gmres (Afcn, b, [], [], n, [], [], [], 2);  # solution of A^2 * x = b

%!demo
%! n = 10;
%! A = toeplitz (sparse ([1, 1], [1, 2], [2, 1] * n ^ 2, 1, n))  + ...
%!     toeplitz (sparse (1, 2, -1, 1, n) * n / 2, ...
%!     sparse (1, 2, 1, 1, n) * n / 2);
%! b = A * ones (n, 1);
%! [M1, M2] = ilu (A + 0.1 * eye (n));  # factorization of A perturbed
%! M = M1 * M2;
%!
%! ## reference solution computed by gmres after one iteration
%! [x_ref, fl] = gmres (A, b, [], [], 1, M);
%! x_ref
%!
%! ## left preconditioning
%! [x, fl] = gmres (M \ A, M \ b, [], [], 1);
%! x # compare x and x_ref

%!test
%! ## Check that all type of inputs work
%! A = toeplitz (sparse ([2, 1, 0, 0, 0]), sparse ([2, -1, 0, 0, 0]));
%! b = sum (A, 2);
%! M1 = diag (sqrt (diag (A)));
%! M2 = M1;
%! Afcn = @(z) A * z;
%! M1_fcn = @(z) M1 \ z;
%! M2_fcn = @(z) M2 \ z;
%! [x, flag] = gmres (A, b);
%! assert (flag, 0);
%! [x, flag] = gmres (A, b, [], [], [], M1, M2);
%! assert (flag, 0);
%! [x, flag] = gmres (A, b, [], [], [], M1_fcn, M2_fcn);
%! assert (flag, 0);
%! [x, flag] = gmres (A, b, [], [], [], M1_fcn, M2);
%! assert (flag, 0);
%! [x, flag] = gmres (A, b, [], [], [], M1, M2_fcn);
%! assert (flag, 0);
%! [x, flag] = gmres (Afcn, b);
%! assert (flag, 0);
%! [x, flag] = gmres (Afcn, b, [],[],[], M1, M2);
%! assert (flag, 0);
%! [x, flag] = gmres (Afcn, b, [],[],[], M1_fcn, M2);
%! assert (flag, 0);
%! [x, flag] = gmres (Afcn, b, [],[],[], M1, M2_fcn);
%! assert (flag, 0);
%! [x, flag] = gmres (Afcn, b, [],[],[], M1_fcn, M2_fcn);
%! assert (flag, 0);

%!test
%! dim = 100;
%! A = spdiags ([-ones(dim,1), 2*ones(dim,1), ones(dim,1)], [-1:1], dim, dim);
%! b = ones (dim, 1);
%! [x, flag] = gmres (A, b, 10, 1e-10, dim, @(x) x ./ diag (A), [], b);
%! assert (x, A\b, 1e-9*norm (x, Inf));
%! [x, flag] = gmres (A, b, dim, 1e-10, 1e4, @(x) diag (diag (A)) \ x, [], b);
%! assert (x, A\b, 1e-7*norm (x, Inf));

%!test
%! dim = 100;
%! A = spdiags ([[1./(2:2:2*(dim-1)) 0]; 1./(1:2:2*dim-1); ...
%!               [0 1./(2:2:2*(dim-1))]]', -1:1, dim, dim);
%! A = A'*A;
%! b = rand (dim, 1);
%! [x, resvec] = gmres (@(x) A*x, b, dim, 1e-10, dim, ...
%!                      @(x) x./diag (A), [], []);
%! assert (x, A\b, 1e-9*norm (x, Inf));
%! [x, flag] = gmres (@(x) A*x, b, dim, 1e-10, 1e5, ...
%!                    @(x) diag (diag (A)) \ x, [], []);
%! assert (x, A\b, 1e-9*norm (x, Inf));
%! [x, flag] = gmres (@(x) A*x, b, dim, 1e-10, 1e5, ...
%!                    @(x) x ./ diag (A), [], []);
%! assert (x, A\b, 1e-7*norm (x, Inf));

%!test
%! ## gmres solves complex linear systems
%! A = toeplitz (sparse ([2, 1, 0, 0, 0]), sparse ([2, -1, 0, 0, 0])) + ...
%!     1i * toeplitz (sparse ([2, 1, 0, 0, 0]), sparse ([2, -1, 0, 0, 0]));
%! b = sum (A, 2);
%! [x, flag] = gmres(A, b, [], [], 5);
%! assert (flag, 0);
%! assert (x, ones (5, 1), -1e-6);

%!test
%! ## Maximum number of iteration reached
%! A = hilb (100);
%! b = sum (A, 2);
%! [x, flag, relres, iter] = gmres (A, b, [], 1e-14);
%! assert (flag, 1);

%!test
%! ## gmres recognizes that the preconditioner matrix is singular
%! AA = 2 * eye (3);
%! bb = ones (3, 1);
%! I = eye (3);
%! M = [1 0 0; 0 1 0; 0 0 0];  # the last row is zero
%! [x, flag] = gmres (@(y) AA * y, bb, [], [], [], @(y) M \ y, @(y) y);
%! assert (flag, 2);

%!test
%! A = rand (4);
%! A = A' * A;
%! [x, flag] = gmres (A, zeros (4, 1), [], [], [], [], [], ones (4, 1));
%! assert (x, zeros (4, 1));

%!test
%! A = rand (4);
%! b = zeros (4, 1);
%! [x, flag, relres, iter] = gmres (A, b);
%! assert (relres, 0);

%!test
%! A = toeplitz (sparse ([2, 1, 0, 0, 0]), sparse ([2, -1, 0, 0, 0]));
%! b = A * ones (5, 1);
%! [x, flag, relres, iter] = gmres (A, b, [], [], [], [], [], ...
%!                                  ones (5, 1) + 1e-8);
%! assert (iter, [0, 0]);

%!test
%! A = rand (20);
%! b = A * ones (20, 1);
%! [x, flag, relres, iter, resvec] = gmres (A, b, [], [], 1);
%! assert (iter, [1, 1]);

%!test
%! A = hilb (20);
%! b = A * ones (20, 1);
%! [x, flag, relres, iter, resvec] = gmres (A, b ,5, 1e-14);
%! assert (iter, [4, 5]);

%!test
%! A = single (1);
%! b = 1;
%! [x, flag] = gmres (A, b);
%! assert (class (x), "single");

%!test
%! A = 1;
%! b = single (1);
%! [x, flag] = gmres (A, b);
%! assert (class (x), "single");

%!test
%! A = single (1);
%! b = single (1);
%! [x, flag] = gmres (A, b);
%! assert (class (x), "single");

%!test
%!function y = Afcn (x)
%!   A = toeplitz ([2, 1, 0, 0], [2, -1, 0, 0]);
%!   y = A * x;
%!endfunction
%! [x, flag] = gmres ("Afcn", [1; 2; 2; 3]);
%! assert (x, ones (4, 1), 1e-6);

%!test # preconditioned residual
%! A = toeplitz (sparse ([2, 1, 0, 0, 0]), sparse ([2, -1, 0, 0, 0]));
%! b = sum (A, 2);
%! M = magic (5);
%! [x, flag, relres] = gmres (A, b, [], [], 2, M);
%! assert (relres, norm (M \ (b - A * x)) / norm (M \ b), 8 * eps);
