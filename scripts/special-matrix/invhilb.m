## Copyright (C) 2002 Dirk Laurie
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2, or (at your option)
## any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, write to the Free
## Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301, USA.

## -*- texinfo -*-
## @deftypefn {Function File} {} invhilb (@var{n})
## Return the inverse of a Hilbert matrix of order @var{n}.  This can be 
## computed computed exactly using
## @tex
## $$\eqalign{
##   A_{ij} &= -1^{i+j} (i+j-1)
##              \left( \matrix{n+i-1 \cr n-j } \right)
##              \left( \matrix{n+j-1 \cr n-i } \right)
##              \left( \matrix{i+j-2 \cr i-2 } \right)^2 \cr
##          &= { p(i)p(j) \over (i+j-1) }
## }$$
## where
## $$
##   p(k) = -1^k \left( \matrix{ k+n-1 \cr k-1 } \right)
##               \left( \matrix{ n \cr k } \right)
##$$
## @end tex
## @ifinfo
## @example
##
##             (i+j)         /n+i-1\  /n+j-1\   /i+j-2\ 2
##  A(i,j) = -1      (i+j-1)(       )(       ) (       )
##                           \ n-j /  \ n-i /   \ i-2 /
##
##         = p(i) p(j) / (i+j-1)
##
## @end example
## where
## @example
##              k  /k+n-1\   /n\
##     p(k) = -1  (       ) (   )
##                 \ k-1 /   \k/
## @end example
## @end ifinfo
##
## The validity of this formula can easily be checked by expanding 
## the binomial coefficients in both formulas as factorials.  It can 
## be derived more directly via the theory of Cauchy matrices: 
## see J. W. Demmel, Applied Numerical Linear Algebra, page 92.
##
## Compare this with the numerical calculation of @code{inverse (hilb (n))},
## which suffers from the ill-conditioning of the Hilbert matrix, and the
## finite precision of your computer's floating point arithmetic.
##
## @end deftypefn
##
## @seealso{hankel, vander, sylvester_matrix, hilb, and toeplitz}

## Author: Dirk Laurie <dlaurie@na-net.ornl.gov>

function retval = invhilb (n)

  if (nargin != 1)
    usage ("invhilb (n)");
  endif

  nmax = length (n);
  if (nmax == 1)

    ## The point about the second formula above is that when vectorized,
    ## p(k) is evaluated for k=1:n which involves O(n) calls to bincoeff 
    ## instead of O(n^2).
    ##
    ## We evaluate the expression as (-1)^(i+j)*(p(i)*p(j))/(i+j-1) except
    ## when p(i)*p(j) would overflow.  In cases where p(i)*p(j) is an exact
    ## machine number, the result is also exact.  Otherwise we calculate
    ## (-1)^(i+j)*p(i)*(p(j)/(i+j-1)).
    ##
    ## The Octave bincoeff routine uses transcendental functions (gammaln
    ## and exp) rather than multiplications, for the sake of speed.  
    ## However, it rounds the answer to the nearest integer, which 
    ## justifies the claim about exactness made above.

    retval = zeros (n); 
    k = [1:n]; 
    p = k .* bincoeff (k+n-1, k-1) .* bincoeff (n, k);
    p(2:2:n) = -p(2:2:n);
    if (n < 203)
      for l = 1:n
        retval(l,:) = (p(l) * p) ./ [l:l+n-1];
      endfor
    else
      for l = 1:n
        retval(l,:) = p(l) * (p ./ [l:l+n-1]);
      endfor
    endif
  else
    error ("invhilb: expecting scalar argument, found something else");
  endif

endfunction
