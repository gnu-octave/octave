## Copyright (C) 1996, 1997 John W. Eaton
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
## @deftypefn {Function File} {} residue (@var{b}, @var{a}, @var{tol})
## If @var{b} and @var{a} are vectors of polynomial coefficients, then
## residue calculates the partial fraction expansion corresponding to the
## ratio of the two polynomials.
## @cindex partial fraction expansion
##
## The function @code{residue} returns @var{r}, @var{p}, @var{k}, and
## @var{e}, where the vector @var{r} contains the residue terms, @var{p}
## contains the pole values, @var{k} contains the coefficients of a direct
## polynomial term (if it exists) and @var{e} is a vector containing the
## powers of the denominators in the partial fraction terms.
##
## Assuming @var{b} and @var{a} represent polynomials
## @iftex
## @tex
## $P(s)$ and $Q(s)$
## @end tex
## @end iftex
## @ifinfo
##  P (s) and Q(s)
## @end ifinfo
##  we have:
## @iftex
## @tex
## $$
## {P(s)\over Q(s)} = \sum_{m=1}^M {r_m\over (s-p_m)^e_m}
##   + \sum_{i=1}^N k_i s^{N-i}.
## $$
## @end tex
## @end iftex
## @ifinfo
##
## @example
##  P(s)    M       r(m)         N
##  ---- = SUM -------------  + SUM k(i)*s^(N-i)
##  Q(s)   m=1 (s-p(m))^e(m)    i=1
## @end example
## @end ifinfo
##
## @noindent
## where @math{M} is the number of poles (the length of the @var{r},
## @var{p}, and @var{e} vectors) and @math{N} is the length of the
## @var{k} vector.
##
## The argument @var{tol} is optional, and if not specified, a default
## value of 0.001 is assumed.  The tolerance value is used to determine
## whether poles with small imaginary components are declared real.  It is
## also used to determine if two poles are distinct.  If the ratio of the
## imaginary part of a pole to the real part is less than @var{tol}, the
## imaginary part is discarded.  If two poles are farther apart than
## @var{tol} they are distinct.  For example,
##
## @example
## @group
##  b = [1, 1, 1];
##  a = [1, -5, 8, -4];
##  [r, p, k, e] = residue (b, a);
##      @result{} r = [-2, 7, 3]
##      @result{} p = [2, 2, 1]
##      @result{} k = [](0x0)
##      @result{} e = [1, 2, 1]
## @end group
## @end example
##
## @noindent
## which implies the following partial fraction expansion
## @iftex
## @tex
## $$
## {s^2+s+1\over s^3-5s^2+8s-4} = {-2\over s-2} + {7\over (s-2)^2} + {3\over s-1}
## $$
## @end tex
## @end iftex
## @ifinfo
##
## @example
##         s^2 + s + 1       -2        7        3
##    ------------------- = ----- + ------- + -----
##    s^3 - 5s^2 + 8s - 4   (s-2)   (s-2)^2   (s-1)
## @end example
## @end ifinfo
## @end deftypefn
##
## @seealso{poly, roots, conv, deconv, polyval, polyderiv, and polyinteg}

## Author: Tony Richardson <arichard@stark.cc.oh.us>
## Created: June 1994
## Adapted-By: jwe

function [r, p, k, e] = residue (b, a, toler)

  ## Here's the method used to find the residues.
  ## The partial fraction expansion can be written as:
  ##
  ##
  ##   P(s)    D   M(k)      A(k,m)
  ##   ---- =  #    #    -------------
  ##   Q(s)   k=1  m=1   (s - pr(k))^m
  ##
  ## (# is used to represent a summation) where D is the number of
  ## distinct roots, pr(k) is the kth distinct root, M(k) is the
  ## multiplicity of the root, and A(k,m) is the residue cooresponding
  ## to the kth distinct root with multiplicity m.  For example,
  ##
  ##        s^2            A(1,1)   A(2,1)    A(2,2)
  ## ------------------- = ------ + ------ + -------
  ## s^3 + 4s^2 + 5s + 2    (s+2)    (s+1)   (s+1)^2
  ##
  ## In this case there are two distinct roots (D=2 and pr = [-2 -1]),
  ## the first root has multiplicity one and the second multiplicity
  ## two (M = [1 2]) The residues are actually stored in vector format as
  ## r = [ A(1,1) A(2,1) A(2,2) ].
  ##
  ## We then multiply both sides by Q(s).  Continuing the example:
  ##
  ## s^2 = r(1)*(s+1)^2 + r(2)*(s+1)*(s+2) + r(3)*(s+2)
  ##
  ## or
  ##
  ## s^2 = r(1)*(s^2+2s+1) + r(2)*(s^2+3s+2) +r(3)*(s+2)
  ##
  ## The coefficients of the polynomials on the right are stored in a row
  ## vector called rhs, while the coefficients of the polynomial on the
  ## left is stored in a row vector called lhs.  If the multiplicity of
  ## any root is greater than one we'll also need derivatives of this
  ## equation of order up to the maximum multiplicity minus one.  The
  ## derivative coefficients are stored in successive rows of lhs and
  ## rhs.
  ##
  ## For our example lhs and rhs would be:
  ##
  ##       | 1 0 0 |
  ## lhs = |       |
  ##       | 0 2 0 |
  ##
  ##       | 1 2 1 1 3 2 0 1 2 |
  ## rhs = |                   |
  ##       | 0 2 2 0 2 3 0 0 1 |
  ##
  ## We then form a vector B and a matrix A obtained by evaluating the
  ## polynomials in lhs and rhs at the pole values. If a pole has a
  ## multiplicity greater than one we also evaluate the derivative
  ## polynomials (successive rows) at the pole value.
  ##
  ## For our example we would have
  ##
  ## | 4|   | 1 0 0 |   | r(1) |
  ## | 1| = | 0 0 1 | * | r(2) |
  ## |-2|   | 0 1 1 |   | r(3) |
  ##
  ## We then solve for the residues using matrix division.

  if (nargin < 2 || nargin > 3)
    usage ("residue (b, a, toler)");
  endif

  if (nargin == 2)
    toler = .001;
  endif

  ## Make sure both polynomials are in reduced form.

  a = polyreduce (a);
  b = polyreduce (b);

  b = b / a(1);
  a = a / a(1);

  la = length (a);
  lb = length (b);

  ## Handle special cases here.

  if (la == 0 || lb == 0)
    k = r = p = e = [];
    return;
  elseif (la == 1)
    k = b / a;
    r = p = e = [];
    return;
  endif

  ## Find the poles.

  p = roots (a);
  lp = length (p);


  ## Determine if the poles are (effectively) zero.
  index = find (abs (p) < toler);
  if (length (index) != 0)
    p (index) = 0;
  endif

  ## Determine if the poles are (effectively) real.

  index = find (abs(p)>=toler && ( abs(imag(p)) ./ abs(p) < toler ));
  if (length (index) != 0)
    p (index) = real (p (index));
  endif

  ## Find the direct term if there is one.

  if (lb >= la)
    ## Also returns the reduced numerator.
    [k, b] = deconv (b, a);
    lb = length (b);
  else
    k = [];
  endif

  if (lp == 1)
    r = polyval (b, p);
    e = 1;
    return;
  endif


  ## We need to determine the number and multiplicity of the roots.
  ##
  ## D  is the number of distinct roots.
  ## M  is a vector of length D containing the multiplicity of each root.
  ## pr is a vector of length D containing only the distinct roots.
  ## e  is a vector of length lp which indicates the power in the partial
  ##    fraction expansion of each term in p.

  ## Set initial values.  We'll remove elements from pr as we find
  ## multiplicities.  We'll shorten M afterwards.

  e = ones (lp, 1);
  M = zeros (lp, 1);
  pr = p;
  D = 1;
  M(1) = 1;

  old_p_index = 1;
  new_p_index = 2;
  M_index = 1;
  pr_index = 2;

  while (new_p_index <= lp)
    if (abs (p (new_p_index) - p (old_p_index)) < toler)
      ## We've found a multiple pole.
      M (M_index) = M (M_index) + 1;
      e (new_p_index) = e (new_p_index-1) + 1;
      ## Remove the pole from pr.
      pr (pr_index) = [];
    else
      ## It's a different pole.
      D++;
      M_index++;
      M (M_index) = 1;
      old_p_index = new_p_index;
      pr_index++;
    endif
    new_p_index++;
  endwhile

  ## Shorten M to it's proper length

  M = M (1:D);

  ## Now set up the polynomial matrices.

  MM = max(M);

  ## Left hand side polynomial

  lhs = zeros (MM, lb);
  rhs = zeros (MM, lp*lp);
  lhs (1, :) = b;
  rhi = 1;
  dpi = 1;
  mpi = 1;
  while (dpi <= D)
    for ind = 1:M(dpi)
      if (mpi > 1 && (mpi+ind) <= lp)
        cp = [p(1:mpi-1); p(mpi+ind:lp)];
      elseif (mpi == 1)
        cp = p (mpi+ind:lp);
      else
        cp = p (1:mpi-1);
      endif
      rhs (1, rhi:rhi+lp-1) = prepad (poly (cp), lp, 0, 2);
      rhi = rhi + lp;
    endfor
    mpi = mpi + M (dpi);
    dpi++;
  endwhile
  if (MM > 1)
    for index = 2:MM
      lhs (index, :) = prepad (polyderiv (lhs (index-1, :)), lb, 0, 2);
      ind = 1;
      for rhi = 1:lp
        cp = rhs (index-1, ind:ind+lp-1);
        rhs (index, ind:ind+lp-1) = prepad (polyderiv (cp), lp, 0, 2);
        ind = ind + lp;
      endfor
    endfor
  endif

  ## Now lhs contains the numerator polynomial and as many derivatives as
  ## are required.  rhs is a matrix of polynomials, the first row
  ## contains the corresponding polynomial for each residue and
  ## successive rows are derivatives.

  ## Now we need to evaluate the first row of lhs and rhs at each
  ## distinct pole value.  If there are multiple poles we will also need
  ## to evaluate the derivatives at the pole value also.

  B = zeros (lp, 1);
  A = zeros (lp, lp);

  dpi = 1;
  row = 1;
  while (dpi <= D)
    for mi = 1:M(dpi)
      B (row) = polyval (lhs (mi, :), pr (dpi));
      ci = 1;
      for col = 1:lp
        cp = rhs (mi, ci:ci+lp-1);
        A (row, col) = polyval (cp, pr(dpi));
        ci = ci + lp;
      endfor
      row++;
    endfor
    dpi++;
  endwhile

  ## Solve for the residues.

  r = A \ B;

endfunction
