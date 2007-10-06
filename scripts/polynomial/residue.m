## Copyright (C) 1996, 1997 John W. Eaton
## Copyright (C) 2007 Ben Abbott
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
## @deftypefn {Function File} {[@var{r}, @var{p}, @var{k}] =} residue (@var{b}, @var{a})
## @deftypefnx {Function File} {[@var{b}, @var{a}] =} residue (@var{r}, @var{p}, @var{k})
## In the instance of two inputs, they are assumed to correspond to vectors
## of polynomial coefficients, @var{b} and @var{a}. From these polynomial
## coefficients, the function residue calculates the partial fraction 
## expansion corresponding to the ratio of the two polynomials.
## @cindex partial fraction expansion
##
## In this instance, the function @code{residue} returns @var{r}, @var{p},
## and @var{k}, where the vector @var{r} contains the residue terms,
## @var{p} contains the pole values, @var{k} contains the coefficients of a 
## direct polynomial term (if it exists).
## 
## In the instance of three inputs, the function 'residue' performs the
## reciprocal task. Meaning the partial fraction expansion is reconstituted
## into the corresponding pair of polynomial coefficients.
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
## @var{k} vector. The @var{e} vector specifies the multiplicity of the
## mth residue's pole.
##
## For example,
##
## @example
## @group
## b = [1, 1, 1];
## a = [1, -5, 8, -4];
## [r, p, k] = residue (b, a);
##      @result{} r = [-2, 7, 3]
##      @result{} p = [2, 2, 1]
##      @result{} k = [](0x0)
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
##
## @end ifinfo
## A similar, but reciprocal example, where the fraction's polynomials are 
## reconstituted from the residues, poles, and direct term is
##
## @example
## @group
## r = [-2, 7, 3];
## p = [2, 2, 1];
## k = [1 0];
## [b, a] = residue (r, p, k);
##      @result{} b = [1, -5, 9, -3, 1]
##      @result{} a = [1, -5, 8, 4]
## @end group
## @end example
##
## @noindent
## which implies the following partial fraction expansion
## @iftex
## @tex
## $$
## {s^4-5s^3+9s^2-3s+1\over s^3-5s^2+8s-4} = {-2\over s-2} + {7\over (s-2)^2} + {3\over s-1} + s
## $$
## @end tex
## @end iftex
## @ifinfo
##
## @example
##    s^4 - 5s^3 + 9s^2 - 3s + 1    -2        7        3
##    -------------------------- = ----- + ------- + ----- + s
##        s^3 - 5s^2 + 8s - 4      (s-2)   (s-2)^2   (s-1)
## @end example
## @end ifinfo
## @seealso{poly, roots, conv, deconv, mpoles, polyval, polyderiv, polyinteg}
## @end deftypefn

## Author: Tony Richardson <arichard@stark.cc.oh.us>
## Author: Ben Abbott <bpabbott@mac.com>
## Created: June 1994
## Adapted-By: jwe

function [r, p, k] = residue (b, a, varargin)

  if (nargin < 2 || nargin > 3)
    print_usage ();
  endif

  toler = .001;

  if (nargin == 3)
    ## The inputs are the residue, pole, and direct part. Solve for the
    ## corresponding numerator and denominator polynomials
    [r, p] = rresidue (b, a, varargin{1}, toler);
    return
  end

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

  small = max (abs (p));
  small = max ([small, 1] ) * 1e-8 * (1 + numel (p))^2;
  p(abs (p) < small) = 0;

  ## Determine if the poles are (effectively) real, or imaginary.

  index = (abs (imag (p)) < small);
  p(index) = real (p(index));
  index = (abs (real (p)) < small);
  p(index) = 1i * imag (p(index));

  ## Sort poles so that multiplicity loop will work.

  [e, indx] = mpoles (p, toler, 1);
  p = p (indx);

  ## Find the direct term if there is one.

  if (lb >= la)
    ## Also return the reduced numerator.
    [k, b] = deconv (b, a);
    lb = length (b);
  else
    k = [];
  endif

  if (lp == 1)
    r = polyval (b, p);
    return;
  endif

  ## Determine the order of the denominator and remaining numerator.
  ## With the direct term removed the potential order of the numerator
  ## is one less than the order of the denominator.

  aorder = numel (a) - 1;
  border = aorder - 1;

  ## Construct a system of equations relating the individual
  ## contributions from each residue to the complete numerator.

  A = zeros (border+1, border+1);
  B = prepad (reshape (b, [numel(b), 1]), border+1, 0);
  for ip = 1:numel(p)
    ri = zeros (size (p));
    ri(ip) = 1;
    A(:,ip) = prepad (rresidue (ri, p, [], toler), border+1, 0).';
  endfor

  ## Solve for the residues.

  r = A \ B;

endfunction

function [pnum, pden] = rresidue (r, p, k, toler)

  ## Reconstitute the numerator and denominator polynomials from the
  ## residues, poles, and direct term.

  if (nargin < 2 || nargin > 4)
    print_usage ();
  endif

  if (nargin < 4)
    toler = [];
  endif

  if (nargin < 3)
    k = [];
  endif
  
  [multp, indx] = mpoles (p, toler, 0);

  p = p (indx);
  r = r (indx);

  indx = 1:numel(p);

  for n = indx
    pn = [1, -p(n)];
    if n == 1
      pden = pn;
    else
      pden = conv (pden, pn);
    endif
  endfor

  ## D is the order of the denominator
  ## K is the order of the direct polynomial
  ## N is the order of the resulting numerator
  ## pnum(1:(N+1)) is the numerator's polynomial
  ## pden(1:(D+1)) is the denominator's polynomial
  ## pm is the multible pole for the nth residue
  ## pn is the numerator contribution for the nth residue

  D = numel (pden) - 1;
  K = numel (k) - 1;
  N = K + D;
  pnum = zeros (1, N+1);
  for n = indx(abs(r)>0)
    p1 = [1, -p(n)];
    for m = 1:multp(n)
      if m == 1
        pm = p1;
      else
        pm = conv (pm, p1);
      endif
    endfor
    pn = deconv (pden, pm);
    pn = r(n) * pn;
    pnum = pnum + prepad ( pn, N+1, 0);
  endfor

  ## Add the direct term.

  if (numel (k))
    pnum = pnum + conv (pden, k);
  endif

  ## Check for leading zeros and trim the polynomial coefficients.

  small = max ([max(abs(pden)), max(abs(pnum)), 1]) * eps;

  pnum (abs (pnum) < small) = 0;
  pden (abs (pden) < small) = 0;

  pnum = polyreduce (pnum);
  pden = polyreduce (pden);

endfunction
