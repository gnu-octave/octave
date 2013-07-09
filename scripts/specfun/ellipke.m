## Copyright (C) 2001 David Billinghurst
## Copyright (C) 2001 Paul Kienzle
## Copyright (C) 2003 Jaakko Ruohio
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
## @deftypefn  {Function File} {} ellipke (@var{m})
## @deftypefnx {Function File} {} ellipke (@var{m}, @var{tol})
## @deftypefnx {Function File} {[@var{k}, @var{e}] =} ellipke (@dots{})
## Compute complete elliptic integral of the first K(@var{m}) and second
## E(@var{m}) kind.
##
## @var{m} is either real array or scalar with 0 @leq{} m @leq{} 1.
##
## @var{tol} is currently ignored (@sc{matlab} uses this to allow faster,
## less accurate approximation).
##
## Ref: Abramowitz, Milton and Stegun, Irene A. Handbook of Mathematical
## Functions, Dover, 1965, Chapter 17.
## @seealso{ellipj}
## @end deftypefn

## Author: David Billinghurst <David.Billinghurst@riotinto.com>
## Author: Paul Kienzle <pkienzle@users.sf.net>
## Author: Jaakko Ruohio

function [k, e] = ellipke (m)

  if (nargin < 1 || nargin > 2)
    print_usage ();
  endif

  k = e = zeros (size (m));
  m = m(:);
  if (any (!isreal (m)))
    error ("ellipke must have real m");
  endif
  if (any (m > 1))
    error ("ellipke must have m <= 1");
  endif

  Nmax = 16;
  idx = find (m == 1);
  if (!isempty (idx))
    k(idx) = Inf;
    e(idx) = 1;
  endif

  idx = find (m == -Inf);
  if (!isempty (idx))
    k(idx) = 0;
    e(idx) = Inf;
  endif

  ## Arithmetic-Geometric Mean (AGM) algorithm
  ## ( Abramowitz and Stegun, Section 17.6 )
  idx = find (m != 1 & m != -Inf);
  if (!isempty (idx))
    idx_neg = find (m < 0 & m != -Inf);
    mult_k = 1./sqrt (1 - m(idx_neg));
    mult_e = sqrt (1 - m(idx_neg));
    m(idx_neg) = -m(idx_neg)./(1 - m(idx_neg));
    a = ones (length (idx), 1);
    b = sqrt (1 - m(idx));
    c = sqrt (m(idx));
    f = 0.5;
    sum = f*c.*c;
    for n = 2:Nmax
      t = (a + b)/2;
      c = (a - b)/2;
      b = sqrt (a.*b);
      a = t;
      f = f * 2;
      sum = sum + f*c.*c;
      if (all (c./a < eps)) break; endif
    endfor
    if (n >= Nmax) error ("ellipke: not enough workspace"); endif
    k(idx) = 0.5*pi./a;
    e(idx) = 0.5*pi.*(1 - sum)./a;
    k(idx_neg) = mult_k.*k(idx_neg);
    e(idx_neg) = mult_e.*e(idx_neg);
  endif

endfunction

%% Test complete elliptic functions of first and second kind
%% against "exact" solution from Mathematica 3.0
%!test
%! m = [0.0; 0.01; 0.1; 0.5; 0.9; 0.99; 1.0 ];
%! [k,e] = ellipke (m);
%!
%! # K(1.0) is really infinity - see below
%! k_exp = [1.5707963267948966192;
%!          1.5747455615173559527;
%!          1.6124413487202193982;
%!          1.8540746773013719184;
%!          2.5780921133481731882;
%!          3.6956373629898746778;
%!          0.0 ];
%! e_exp = [1.5707963267948966192;
%!          1.5668619420216682912;
%!          1.5307576368977632025;
%!          1.3506438810476755025;
%!          1.1047747327040733261;
%!          1.0159935450252239356;
%!          1.0 ];
%! if (k(7)==Inf), k(7)=0; endif;
%! assert (k, k_exp, 8*eps);
%! assert (e, e_exp, 8*eps);

%% Test against A&S Table 17.1
%!test
%! m = [0:5:50]'/100;
%! k_exp = [1.570796326794897;
%!          1.591003453790792;
%!          1.612441348720219;
%!          1.635256732264580;
%!          1.659623598610528;
%!          1.685750354812596;
%!          1.713889448178791;
%!          1.744350597225613;
%!          1.777519371491253;
%!          1.813883936816983;
%!          1.854074677301372 ];
%! e_exp = [1.570796327;
%!          1.550973352;
%!          1.530757637;
%!          1.510121831;
%!          1.489035058;
%!          1.467462209;
%!          1.445363064;
%!          1.422691133;
%!          1.399392139;
%!          1.375401972;
%!          1.350643881 ];
%! [k,e] = ellipke (m);
%! assert (k, k_exp, 1e-15);
%! assert (e, e_exp, 1e-8);

%% Test input validation
%!error ellipke ()
%!error ellipke (1,2,3)
