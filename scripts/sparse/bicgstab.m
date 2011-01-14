## Copyright (C) 2008-2011 Radek Salac
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
## @deftypefn  {Function File} {} bicgstab (@var{A}, @var{b})
## @deftypefnx {Function File} {} bicgstab (@var{A}, @var{b}, @var{tol}, @var{maxit}, @var{M1}, @var{M2}, @var{x0})
## This procedure attempts to solve a system of linear equations A*x = b for x.
## The @var{A} must be square, symmetric and positive definite real matrix N*N.
## The @var{b} must be a one column vector with a length of N.
## The @var{tol} specifies the tolerance of the method, the default value is
## 1e-6.
## The @var{maxit} specifies the maximum number of iterations, the default value
## is min(20,N).
## The @var{M1} specifies a preconditioner, can also be a function handler which
## returns M\X.
## The @var{M2} combined with @var{M1} defines preconditioner as
## preconditioner=M1*M2.
## The @var{x0} is the initial guess, the default value is zeros(N,1).
##
## The value @var{x} is a computed result of this procedure.
## The value @var{flag} can be 0 when we reach tolerance in @var{maxit}
## iterations, 1 when
## we don't reach tolerance in @var{maxit} iterations and 3 when the procedure
## stagnates.
## The value @var{relres} is a relative residual - norm(b-A*x)/norm(b).
## The value @var{iter} is an iteration number in which x was computed.
## The value @var{resvec} is a vector of @var{relres} for each iteration.
##
## @end deftypefn

function [x, flag, relres, iter, resvec] = bicgstab (A, b, tol, maxit, M1, M2, x0)

  if (nargin < 2 || nargin > 7 || nargout > 5)
    print_usage ();
  elseif (!(isnumeric (A) && issquare (A)))
    error ("bicgstab: A must be a square numeric matrix");
  elseif (!isvector (b))
    error ("bicgstab: B must be a vector");
  elseif (!any (b))
    error ("bicgstab: B must not be a vector of all zeros");
  elseif (rows (A) != rows (b))
    error ("bicgstab: A and B must have the same number of rows");
  elseif (nargin > 2 && !isscalar (tol))
    error ("bicgstab: TOL must be a scalar");
  elseif (nargin > 3 && !isscalar (maxit))
    error ("bicgstab: MAXIT must be a scalar");
  elseif (nargin > 4 && ismatrix (M1) && (rows (M1) != rows (A) || columns (M1) != columns (A)))
    error ("bicgstab: M1 must have the same number of rows and columns as A");
  elseif (nargin > 5 && (!ismatrix (M2) || rows (M2) != rows (A) || columns (M2) != columns (A)))
    error ("bicgstab: M2 must have the same number of rows and columns as A");
  elseif (nargin > 6 && !isvector (x0))
    error ("bicgstab: X0 must be a vector");
  elseif (nargin > 6 && rows (x0) != rows (b))
    error ("bicgstab: X0 must have the same number of rows as B");
  endif

  ## Default tolerance.
  if (nargin < 3)
    tol = 1e-6;
  endif

  ## Default maximum number of iteration.
  if (nargin < 4)
    maxit = min (rows (b), 20);
  endif

  ## Left preconditioner.
  if (nargin == 5)
    if (isnumeric (M1))
      precon = @(x) M1 \ x;
    endif
  elseif (nargin > 5)
    if (issparse (M1) && issparse (M2))
      precon = @(x) M2 \ (M1 \ x);
    else
      M = M1*M2;
      precon = @(x) M \ x;
    endif
  else
    precon = @(x) x;
  endif

  ## specifies initial estimate x0
  if (nargin < 7)
    x = zeros (rows (b), 1);
  else
    x = x0;
  endif

  norm_b = norm (b);

  res = b - A*x;
  rr = res;

  ## Vector of the residual norms for each iteration.
  resvec = [norm(res)/norm_b];

  ## Default behaviour we don't reach tolerance tol within maxit iterations.
  flag = 1;

  for iter = 1:maxit
    rho_1 = res' * rr;

    if (iter == 1)
      p = res;
    else
      beta = (rho_1 / rho_2) * (alpha / omega);
      p = res + beta * (p - omega * v);
    endif

    phat = precon (p);

    v = A * phat;
    alpha = rho_1 / (rr' * v);
    s = res - alpha * v;

    shat = precon (s);

    t = A * shat; 
    omega = (t' * s) / (t' * t);
    x = x + alpha * phat + omega * shat;
    res = s - omega * t;
    rho_2 = rho_1;

    relres = norm (res) / norm_b;
    resvec = [resvec; relres];

    if (relres <= tol)
      ## We reach tolerance tol within maxit iterations.
      flag = 0;
      break;
    elseif (resvec (end) == resvec (end - 1)) 
      ## The method stagnates.
      flag = 3;
      break;
    endif
  endfor

  if (nargout < 2)
    if (flag == 0) 
      printf (["bicgstab converged at iteration %i ",
      "to a solution with relative residual %e\n"],iter,relres);
    elseif (flag == 3)
      printf (["bicgstab stopped at iteration %i ",
      "without converging to the desired tolerance %e\n",
      "because the method stagnated.\n",
      "The iterate returned (number %i) has relative residual %e\n"],iter,tol,iter,relres);
    else
      printf (["bicgstab stopped at iteration %i ",
      "without converging to the desired toleranc %e\n",
      "because the maximum number of iterations was reached.\n",
      "The iterate returned (number %i) has relative residual %e\n"],iter,tol,iter,relres);
    endif
  endif

endfunction

%!demo
%! % Solve system of A*x=b
%! A = [5 -1 3;-1 2 -2;3 -2 3]
%! b = [7;-1;4]
%! [x, flag, relres, iter, resvec] = bicgstab(A, b)

