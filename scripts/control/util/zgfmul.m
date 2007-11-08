## Copyright (C) 1996, 1998, 2000, 2004, 2005, 2007
##               Auburn University. All rights reserved.
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
## @deftypefn {Function File} {@var{y} =} zgfmul (@var{a}, @var{b}, @var{c}, @var{d}, @var{x})
## Compute product of @var{zgep} incidence matrix @math{F} with vector @var{x}.
## Used by @command{zgepbal} (in @command{zgscal}) as part of generalized conjugate gradient
## iteration.
## @end deftypefn

## References:
## ZGEP: Hodel, "Computation of Zeros with Balancing," 1992, submitted to  LAA
## Generalized CG: Golub and Van Loan, "Matrix Computations, 2nd ed" 1989

## Author: A. S. Hodel <a.s.hodel@eng.auburn.edu>
## Conversion to Octave July 3, 1994

function y = zgfmul (a, b, c, d, x)

  if (nargin != 5)
    print_usage ();
  endif 

  [n,m] = size(b);
  [p,m1] = size(c);
  nm = n+m;
  y = zeros(nm+p,1);

  ## construct F column by column
  for jj=1:n
    Fj = zeros(nm+p,1);

    ## rows 1:n: F1
    aridx = complement(jj,find(a(jj,:) != 0));
    acidx = complement(jj,find(a(:,jj) != 0));
    bidx = find(b(jj,:) != 0);
    cidx = find(c(:,jj) != 0);

    Fj(aridx) = Fj(aridx) - 1;      # off diagonal entries of F1
    Fj(acidx) = Fj(acidx) - 1;
    ## diagonal entry of F1
    Fj(jj) = length(aridx)+length(acidx) + length(bidx) + length(cidx);

    if(!isempty(bidx)) Fj(n+bidx) = 1;     endif # B' incidence
    if(!isempty(cidx)) Fj(n+m+cidx) = -1;  endif # -C incidence
    y = y + x(jj)*Fj;   # multiply by corresponding entry of x
  endfor

  for jj=1:m
    Fj = zeros(nm+p,1);
    bidx = find(b(:,jj) != 0);
    if(!isempty(bidx)) Fj(bidx) = 1; endif     # B incidence
    didx = find(d(:,jj) != 0);
    if(!isempty(didx)) Fj(n+m+didx) = 1; endif # D incidence
    Fj(n+jj) = length(bidx) + length(didx);         # F2 is diagonal
    y = y + x(n+jj)*Fj;   # multiply by corresponding entry of x
  endfor

  for jj=1:p
    Fj = zeros(nm+p,1);
    cidx = find(c(jj,:) != 0);
    if(!isempty(cidx)) Fj(cidx) = -1; endif  # -C' incidence
    didx = find(d(jj,:) != 0);
    if(!isempty(didx)) Fj(n+didx) = 1;  endif # D' incidence
    Fj(n+m+jj) = length(cidx) + length(didx);     # F2 is diagonal
    y = y + x(n+m+jj)*Fj;   # multiply by corresponding entry of x
  endfor

endfunction
