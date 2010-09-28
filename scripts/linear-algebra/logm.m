## Copyright (C) 2010 Richard T. Guy <guyrt7@wfu.edu>
## Copyright (C) 2010 Marco Caliari <marco.caliari@univr.it>
## Copyright (C) 2008 N.J. Higham
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
## @deftypefn  {Function File} {@var{s} =} logm (@var{a})
## @deftypefnx {Function File} {@var{s} =} logm (@var{a}, @var{opt_iters})
## @deftypefnx {Function File} {[@var{s}, @var{iters}] =} logm (@dots{})
## Compute the matrix logarithm of the square matrix @var{a}.  The
## implementation utilizes a Pad@'e approximant and the identity
##
## @example
## logm(@var{a}) = 2^k * logm(@var{a}^(1 / 2^k))
## @end example
##
## The optional argument @var{opt_iters} is the maximum number of square roots
## to compute and defaults to 100.  The optional output @var{iters} is the
## number of square roots actually computed.
##
## @end deftypefn

## Reference: N. J. Higham, Functions of Matrices: Theory and Computation 
##            (SIAM, 2008.)
##

function [s, iters] = logm (a, opt_iters)
 
  if (nargin == 0)
    print_usage ();
  elseif (nargin < 2)
    opt_iters = 100;
  elseif (nargin > 2)
    print_usage ();
  endif

  if (! issquare (a))
    error ("logm: argument must be a square matrix.");
  endif

  [u, s] = schur (a);

  if (isreal (a))
    [u, s] = rsf2csf (u, s);
  endif

  if (any (diag (s) < 0))
    warning ("Octave:logm:non-principal",
    ["logm: Matrix has negative eigenvalues.", ...
     "  Principal matrix logarithm is not defined.", ...
     "  Computing non-principal logarithm."]);
  endif

  k = 0;
  ## Algorithm 11.9 in "Function of matrices", by N. Higham
  theta = [0, 0, 1.61e-2, 5.38e-2, 1.13e-1, 1.86e-1, 2.6429608311114350e-1];
  p = 0;
  m = 7;
  while (k < opt_iters)
    tau = norm (s - eye (size (s)),1);
    if (tau <= theta (7))
      p = p + 1;
      j(1) = find (tau <= theta, 1);
      j(2) = find (tau / 2 <= theta, 1);
      if (j(1) - j(2) <= 1 || p == 2)
        m = j(1);
        break
      endif
    endif
    k = k + 1;
    s = sqrtm (s);
  endwhile

  if (k >= opt_iters)
    warning ("logm: Maximum number of square roots exceeded.  Results may still be accurate.");
  endif

  s = logm_pade_pf (s - eye (size (s)), m);

  s = 2^k * u * s * u';

  if (nargout == 2)
    iters = k;
  endif

endfunction

################## ANCILLARY FUNCTIONS ################################
######  Taken from the mfttoolbox (GPL 3) by D. Higham.
######  Reference: 
######      D. Higham, Functions of Matrices: Theory and Computation 
######      (SIAM, 2008.).
#######################################################################

##LOGM_PADE_PF   Evaluate Pade approximant to matrix log by partial fractions.
##   Y = LOGM_PADE_PF(a,M) evaluates the [M/M] Pade approximation to
##   LOG(EYE(SIZE(a))+a) using a partial fraction expansion.

function s = logm_pade_pf(a,m)
  [nodes,wts] = gauss_legendre(m);
  ## Convert from [-1,1] to [0,1].
  nodes = (nodes + 1)/2;
  wts = wts/2;

  n = length(a);
  s = zeros(n);
  for j=1:m
    s = s + wts(j)*(a/(eye(n) + nodes(j)*a));
  endfor
endfunction

######################################################################
## GAUSS_LEGENDRE  Nodes and weights for Gauss-Legendre quadrature.
##   [X,W] = GAUSS_LEGENDRE(N) computes the nodes X and weights W
##   for N-point Gauss-Legendre quadrature.

## Reference:
## G. H. Golub and J. H. Welsch, Calculation of Gauss quadrature
## rules, Math. Comp., 23(106):221-230, 1969.

function [x,w] = gauss_legendre(n)
  i = 1:n-1;
  v = i./sqrt ((2*i).^2-1);
  [V,D] = eig ( diag(v,-1)+diag(v,1) );
  x = diag (D);
  w = 2*(V(1,:)'.^2);
endfunction


%!assert(norm(logm([1 -1;0 1]) - [0 -1; 0 0]) < 1e-5);
%!assert(norm(expm(logm([-1 2 ; 4 -1])) - [-1 2 ; 4 -1]) < 1e-5);
%!assert(logm([1 -1 -1;0 1 -1; 0 0 1]), [0 -1 -1.5; 0 0 -1; 0 0 0], 1e-5);
%!
%% Test input validation
%!error logm ();
%!error logm (1, 2, 3);
%!error <logm: argument must be a square matrix.> logm([1 0;0 1; 2 2]);
