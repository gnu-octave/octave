## Copyright (C) 2008, 2009 Radek Salac
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
## @deftypefn {Function File} {} cgs (@var{A}, @var{b})
## @deftypefnx {Function File} {} cgs (@var{A}, @var{b}, @var{tol}, @var{maxit}, @var{M1}, @var{M2}, @var{x0})
## This procedure attempts to solve a system of linear equations A*x = b for x.
## The @var{A} must be square, symmetric and positive definite real matrix N*N.
## The @var{b} must be a one column vector with a length of N.
## The @var{tol} specifies the tolerance of the method, default value is 1e-6.
## The @var{maxit} specifies the maximum number of iteration, default value is MIN(20,N).
## The @var{M1} specifies a preconditioner, can also be a function handler which returns M\X.
## The @var{M2} combined with @var{M1} defines preconditioner as preconditioner=M1*M2.
## The @var{x0} is initial guess, default value is zeros(N,1).
##
## @end deftypefn

function [x, flag, relres, iter, resvec] = cgs (A, b, tol, maxit, M1, M2, x0)

  if (nargin < 2 || nargin > 7 || nargout > 5)
    print_usage ();
  elseif (!isnumeric (A) || rows (A) != columns (A))
    error ("cgs: first argument must be a n-by-n matrix");
  elseif (!isvector (b))
    error ("cgs: b must be a vector");
  elseif (rows (A) != rows (b))
    error ("cgs: first and second argument must have the same number of rows");
  elseif (nargin > 2 && !isscalar (tol))
    error ("cgs: tol must be a scalar");
  elseif (nargin > 3 && !isscalar (maxit))
    error ("cgs: maxit must be a scalar");
  elseif (nargin > 4 && ismatrix (M1) && (rows (M1) != rows (A) || columns (M1) != columns (A)))
    error ("cgs: M1 must have the same number of rows and columns as A");
  elseif (nargin > 5 && (!ismatrix (M2) || rows (M2) != rows (A) || columns (M2) != columns (A)))
    error ("cgs: M2 must have the same number of rows and columns as A");
  elseif (nargin > 6 && !isvector (x0))
    error ("cgs: x0 must be a vector");
  elseif (nargin > 6 && rows (x0) != rows (b))
    error ("cgs: x0 must have the same number of rows as b");
  endif

  ## Default tolerance.
  if (nargin < 3)
    tol = 1e-6;
  endif

  ## Default maximum number of iteration.
  if (nargin < 4)
    maxit = min (rows (b),20);
  endif

  ## Left preconditioner.
  precon = [];
  if (nargin == 5)
    precon = M1;
  elseif (nargin > 5)
    if (isparse(M1) && issparse(M2))
      precon = @(x) M1 * (M2 * x);
    else
      precon = M1 * M2;
    endif
  endif

  ## Precon can also be a function.
  if (nargin > 4 && isnumeric (precon))

    ## We can compute inverse preconditioner and use quicker algorithm.
    if (det (precon) != 0)
     precon = inv (precon);
    else
     error ("cgs: preconditioner is ill conditioned");
    endif

    ## We must make test if preconditioner isn't ill conditioned.
    if (isinf (cond (precon))); 
      error ("cgs: preconditioner is ill conditioned");
    endif
  endif

  ## Specifies initial estimate x0.
  if (nargin < 7)
    x = zeros (rows (b), 1);
  else
    x = x0;
  endif

  relres = b - A * x;
  ## Vector of the residual norms for each iteration.
  resvec = [norm(relres)];
  ro = 0;
  norm_b = norm (b);
  ## Default behavior we don't reach tolerance tol within maxit iterations.
  flag = 1;
  for iter = 1 : maxit

    if (nargin > 4 && isnumeric (precon))
      ## We have computed inverse matrix so we can use quick algorithm.
      z = precon * relres;
    elseif (nargin > 4)
      ## Our preconditioner is a function.
      z = feval (precon, relres);
    else
      ## We don't use preconditioning.
      z = relres;
    endif

    ## Cache.
    ro_old = ro;
    ro = relres' * z;
    if (iter == 1)
      p = z;
    else
      beta = ro / ro_old;
      p = z + beta * p;
    endif
    ## Cache.
    q = A * p;
    alpha = ro / (p' * q);
    x = x + alpha * p;

    relres = relres - alpha * q;
    resvec = [resvec; norm(relres)];

    relres_distance = resvec (end) / norm_b;
    if (relres_distance <= tol)
      ## We reach tolerance tol within maxit iterations.
      flag = 0;
      break;
    elseif (resvec (end) == resvec (end - 1))
      ## The method stagnates.
      flag = 3;
      break;
    endif
  endfor;

  relres = relres_distance;
endfunction



%!demo
%! % Solve system of A*x=b
%! A=[5 -1 3;-1 2 -2;3 -2 3]
%! b=[7;-1;4]
%! [a,b,c,d,e]=cgs(A,b)
