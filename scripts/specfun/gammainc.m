########################################################################
##
## Copyright (C) 2016-2023 The Octave Project Developers
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
## @deftypefn  {} {@var{y} =} gammainc (@var{x}, @var{a})
## @deftypefnx {} {@var{y} =} gammainc (@var{x}, @var{a}, @var{tail})
## Compute the normalized incomplete gamma function.
##
## This is defined as
## @tex
## $$
##  \gamma (x, a) = {1 \over {\Gamma (a)}}\displaystyle{\int_0^x t^{a-1} e^{-t} dt}
## $$
## @end tex
## @ifnottex
##
## @example
## @group
##                                 x
##                        1       /
## gammainc (x, a) = ---------    | exp (-t) t^(a-1) dt
##                   gamma (a)    /
##                             t=0
## @end group
## @end example
##
## @end ifnottex
## with the limiting value of 1 as @var{x} approaches infinity.
## The standard notation is @math{P(a,x)}, e.g., @nospell{Abramowitz} and
## @nospell{Stegun} (6.5.1).
##
## If @var{a} is scalar, then @code{gammainc (@var{x}, @var{a})} is returned
## for each element of @var{x} and vice versa.
##
## If neither @var{x} nor @var{a} is scalar then the sizes of @var{x} and
## @var{a} must agree, and @code{gammainc} is applied element-by-element.
## The elements of @var{a} must be non-negative.
##
## By default, @var{tail} is @qcode{"lower"} and the incomplete gamma function
## integrated from 0 to @var{x} is computed.  If @var{tail} is @qcode{"upper"}
## then the complementary function integrated from @var{x} to infinity is
## calculated.
##
## If @var{tail} is @qcode{"scaledlower"}, then the lower incomplete gamma
## function is multiplied by
## @tex
## $\Gamma(a+1)\exp(x)x^{-a}$.
## @end tex
## @ifnottex
## @math{gamma(a+1)*exp(x)/(x^a)}.
## @end ifnottex
## If @var{tail} is @qcode{"scaledupper"}, then the upper incomplete gamma
## function is multiplied by the same quantity.
##
## References:
##
## @nospell{M. Abramowitz and I.A. Stegun},
## @cite{Handbook of mathematical functions},
## @nospell{Dover publications, Inc.}, 1972.
##
## @nospell{W. Gautschi},
## @cite{A computational procedure for incomplete gamma functions},
## @nospell{ACM Trans.@: Math Software}, pp.@: 466--481, Vol 5, No.@: 4, 2012.
##
## @nospell{W. H. Press, S. A. Teukolsky, W. T. Vetterling, and B. P. Flannery},
## @cite{Numerical Recipes in Fortran 77}, ch.@: 6.2, Vol 1, 1992.
##
## @seealso{gamma, gammaincinv, gammaln}
## @end deftypefn

## P(a,x) = gamma(a,x)/Gamma(a), upper
## 1-P(a,x)=Q(a,x)=Gamma(a,x)/Gamma(a), lower

function y = gammainc (x, a, tail = "lower")

  if (nargin < 2)
    print_usage ();
  endif

  [err, x, a] = common_size (x, a);
  if (err > 0)
    error ("gammainc: X and A must be of common size or scalars");
  endif

  if (iscomplex (x) || iscomplex (a))
    error ("gammainc: all inputs must be real");
  endif

  ## Remember original shape of data, but convert to column vector for calcs.
  x_sz = size (x);
  x = x(:);
  a = a(:);

  if (any (a < 0))
    error ("gammainc: A must be non-negative");
  endif

  if (nargin == 3
      && ! any (strcmpi (tail, {"lower","upper","scaledlower","scaledupper"})))
    error ("gammainc: invalid value for TAIL");
  endif
  tail = tolower (tail);

  ## If any of the arguments is single then the output should be as well.
  if (strcmp (class (x), "single") || strcmp (class (a), "single"))
    x = single (x);
    a = single (a);
  endif

  ## Convert to floating point if necessary
  if (isinteger (x))
    x = double (x);
  endif
  if (isinteger (a))
    a = double (a);
  endif

  ## Initialize output array
  y = zeros (x_sz, class (x));

  ## Different x, a combinations are handled by different subfunctions.
  todo = true (size (x));  # Track which elements need to be calculated.

  ## Case 0: x == Inf, a == Inf
  idx = (x == Inf) & (a == Inf);
  if (any (idx))
    y(idx) = NaN;
    todo(idx) = false;
  endif

  ## Case 1: x == 0, a == 0.
  idx = (x == 0) & (a == 0);
  if (any (idx))
    y(idx) = gammainc_00 (tail);
    todo(idx) = false;
  endif

  ## Case 2: x == 0.
  idx = todo & (x == 0);
  if (any (idx))
    y(idx) = gammainc_x0 (tail);
    todo(idx) = false;
  endif

  ## Case 3: x = Inf
  idx = todo & (x == Inf);
  if (any (idx))
    y(idx) = gammainc_x_inf (tail);
    todo(idx) = false;
  endif

  ## Case 4: a = Inf
  idx = todo & (a == Inf);
  if (any (idx))
    y(idx) = gammainc_a_inf (tail);
    todo(idx) = false;
  endif

  ## Case 5: a == 0.
  idx = todo & (a == 0);
  if (any (idx))
    y(idx) = gammainc_a0 (x(idx), tail);
    todo(idx) = false;
  endif

  ## Case 6: a == 1.
  idx = todo & (a == 1);
  if (any (idx))
    y(idx) = gammainc_a1 (x(idx), tail);
    todo(idx) = false;
  endif

  ## Case 7: positive integer a; exp (x) and a! both under 1/eps.
  idx = (todo
         & (a == fix (a)) & (a > 1) & (a <= 18) & (x <= 36) & (abs (x) >= .1));
  if (any (idx))
    y(idx) = gammainc_an (x(idx), a(idx), tail);
    todo(idx) = false;
  endif

  ## For a < 2, x < 0, we increment a by 2 and use a recurrence formula after
  ## the computations.

  flag_a_small = todo & (abs (a) > 0) & (abs (a) < 2) & (x < 0);
  a(flag_a_small) += 2;

  flag_s = (((x + 0.25 < a) | (x < 0)) & (x > -20)) | (abs (x) < 1);

  ## Case 8: x, a relatively small.
  idx = todo & flag_s;
  if (any (idx))
    y(idx) = gammainc_s (x(idx), a(idx), tail);
    todo(idx) = false;
  endif

  ## Case 9: x positive and large relative to a.
  idx = todo;
  if (any (idx))
    y(idx) = gammainc_l (x(idx), a(idx), tail);
    todo(idx) = false;
  endif

  if (any (flag_a_small))
    if (strcmp (tail, "lower"))
      y(flag_a_small) += D (x(flag_a_small), a(flag_a_small) - 1) + ...
        D (x(flag_a_small), a(flag_a_small) - 2);
    elseif (strcmp (tail, "upper"))
      y(flag_a_small) -= D (x(flag_a_small), a(flag_a_small) - 1) + ...
           D (x(flag_a_small), a(flag_a_small) - 2);
    elseif (strcmp (tail, "scaledlower"))
      y(flag_a_small) = y(flag_a_small) .* (x(flag_a_small) .^ 2) ./ ...
        (a(flag_a_small) .* (a(flag_a_small) - 1)) + (x(flag_a_small) ./ ...
          (a(flag_a_small) - 1)) + 1;
    elseif (strcmp (tail, "scaledupper"))
      y(flag_a_small) = y(flag_a_small) .* (x(flag_a_small) .^ 2) ./ ...
        (a(flag_a_small) .* (a(flag_a_small) - 1)) - (x(flag_a_small) ./ ...
          (a(flag_a_small) - 1)) - 1;
     endif
  endif

endfunction

## Subfunctions to handle each case:

## x == 0, a == 0.
function y = gammainc_00 (tail)

  if (strcmp (tail, "upper") || strcmp (tail, "scaledupper"))
    y = 0;
  else
    y = 1;
  endif

endfunction

## x == 0.
function y = gammainc_x0 (tail)

  if (strcmp (tail, "lower"))
    y = 0;
  elseif (strcmp (tail, "upper") || strcmp (tail, "scaledlower"))
    y = 1;
  else
    y = Inf;
  endif

endfunction

## x == Inf.
function y = gammainc_x_inf (tail)

  if (strcmp (tail, "lower"))
    y = 1;
  elseif (strcmp (tail, "upper") || strcmp (tail, "scaledupper"))
    y = 0;
  else
    y = Inf;
  endif

endfunction

## a == Inf.
function y = gammainc_a_inf (tail)

  if (strcmp (tail, "lower"))
    y = 0;
  elseif (strcmp (tail, "upper") || strcmp (tail, "scaledlower"))
    y = 1;
  else
    y = Inf;
  endif

endfunction

## a == 0.
function y = gammainc_a0 (x, tail)

  if (strcmp (tail, "lower"))
    y = 1;
  elseif (strcmp (tail, "scaledlower"))
    y = exp (x);
  else
    y = 0;
  endif

endfunction

## a == 1.
function y = gammainc_a1 (x, tail)

  if (strcmp (tail, "lower"))
    if (abs (x) < 1/2)
      y = - expm1 (-x);
    else
      y = 1 - exp (-x);
    endif
  elseif (strcmp (tail, "upper"))
    y = exp (-x);
  elseif (strcmp (tail, "scaledlower"))
    if (abs (x) < 1/2)
      y = expm1 (x) ./ x;
    else
      y = (exp (x) - 1) ./ x;
    endif
  else
    y = 1 ./ x;
  endif

endfunction

## positive integer a; exp (x) and a! both under 1/eps
## uses closed-form expressions for nonnegative integer a
## -- http://mathworld.wolfram.com/IncompleteGammaFunction.html.
function y = gammainc_an (x, a, tail)

  y = t = ones (size (x), class (x));
  i = 1;
  while (any (a(:) > i))
    jj = (a > i);
    t(jj) .*= (x(jj) / i);
    y(jj) += t(jj);
    i++;
  endwhile
  if (strcmp (tail, "lower"))
    y = 1 - exp (-x) .* y;
  elseif (strcmp (tail, "upper"))
    y .*= exp (-x);
  elseif (strcmp (tail, "scaledlower"))
    y = (1 - exp (-x) .* y) ./ D(x, a);
  elseif (strcmp (tail, "scaledupper"))
    y .*= exp (-x) ./ D(x, a);
  endif

endfunction

## x + 0.25 < a | x < 0 | abs(x) < 1.
## Numerical Recipes in Fortran 77 (6.2.5)
## series
function y = gammainc_s (x, a, tail)

  if (strcmp (tail, "scaledlower") || strcmp (tail, "scaledupper"))
    y = ones (size (x), class (x));
    term = x ./ (a + 1);
  else
    ## Of course it is possible to scale at the end, but some tests fail.
    ## And try gammainc (1,1000), it take 0 iterations if you scale now.
    y = D (x,a);
    term = y .* x ./ (a + 1);
  endif
  n = 1;
  while (any (abs (term(:)) > (abs (y(:)) * eps)))
    ## y can be zero from the beginning (gammainc (1,1000))
    jj = abs (term) > abs (y) * eps;
    n += 1;
    y(jj) += term(jj);
    term(jj) .*= x(jj) ./ (a(jj) + n);
  endwhile
  if (strcmp (tail, "upper"))
    y = 1 - y;
  elseif (strcmp (tail, "scaledupper"))
    y = 1 ./ D (x,a) - y;
  endif

endfunction

## x positive and large relative to a
## NRF77 (6.2.7)
## Gamma (a,x)/Gamma (a)
## Lentz's algorithm
## __gammainc__ in libinterp/corefcn/__gammainc__.cc
function y = gammainc_l (x, a, tail)

  y = __gammainc__ (x, a);
  if (strcmp (tail,  "lower"))
    y = 1 - y .* D (x, a);
  elseif (strcmp (tail, "upper"))
    y .*= D (x, a);
  elseif (strcmp (tail, "scaledlower"))
    y = 1 ./ D (x, a) - y;
  endif

endfunction

## Compute exp(-x)*x^a/Gamma(a+1) in a stable way for x and a large.
##
## L. Knusel, Computation of the Chi-square and Poisson distribution,
## SIAM J. Sci. Stat. Comput., 7(3), 1986
## which quotes Section 5, Abramowitz&Stegun 6.1.40, 6.1.41.
function y = D (x, a)

  athresh = 10;  # FIXME: can this be better tuned?
  y = zeros (size (x), class (x));

  todo = true (size (x));
  todo(x == 0) = false;

  ii = todo & (x > 0) & (a > athresh) & (a >= x);
  if (any (ii))
    lnGa = log (2 * pi * a(ii)) / 2 + 1 ./ (12 * a(ii)) - ...
           1 ./ (360 * a(ii) .^ 3) + 1 ./ (1260 * a(ii) .^ 5) - ...
           1 ./ (1680 * a(ii) .^ 7) + 1 ./ (1188 * a(ii) .^ 9)- ...
           691 ./ (87360 * a(ii) .^ 11) + 1 ./ (156 * a(ii) .^ 13) - ...
           3617 ./ (122400 * a(ii) .^ 15) + ...
           43867 ./ (244188 * a(ii) .^ 17) - 174611 ./ (125400 * a(ii) .^ 19);
    lns = log1p ((a(ii) - x(ii)) ./ x(ii));
    y(ii) = exp ((a(ii) - x(ii)) - a(ii) .* lns - lnGa);
    todo(ii) = false;
  endif

  ii = todo & (x > 0) & (a > athresh) & (a < x);
  if (any (ii))
    lnGa = log (2 * pi * a(ii)) / 2 + 1 ./ (12 * a(ii)) - ...
           1 ./ (360 * a(ii) .^ 3) + 1 ./ (1260 * a(ii) .^ 5) - ...
           1 ./ (1680 * a(ii) .^ 7) + 1 ./ (1188 * a(ii) .^ 9)- ...
           691 ./ (87360 * a(ii) .^ 11) + 1 ./ (156 * a(ii) .^ 13) - ...
           3617 ./ (122400 * a(ii) .^ 15) + ...
           43867 ./ (244188 * a(ii) .^ 17) - 174611 ./ (125400 * a(ii) .^ 19);
    lns = -log1p ((x(ii) - a(ii)) ./ a(ii));
    y(ii) = exp ((a(ii) - x(ii)) - a(ii) .* lns - lnGa);
    todo(ii) = false;
  endif

  ii = todo & ((x <= 0) | (a <= athresh));
  if (any (ii))  # standard formula for a not so large.
    y(ii) = exp (a(ii) .* log (x(ii)) - x(ii) - gammaln (a(ii) + 1));
    todo(ii) = false;
  endif

  ii = (x < 0) & (a == fix (a));
  if (any (ii))  # remove spurious imaginary part.
    y(ii) = real (y(ii));
  endif

endfunction


## Test: case 1,2,5
%!assert (gammainc ([0, 0, 1], [0, 1, 0]), [1, 0, 1])
%!assert (gammainc ([0, 0, 1], [0, 1, 0], "upper"), [0, 1, 0])
%!assert (gammainc ([0, 0, 1], [0, 1, 0], "scaledlower"), [1, 1, exp(1)])
%!assert (gammainc ([0, 0, 1], [0, 1, 0], "scaledupper"), [0, Inf, 0])

## Test: case 3,4
%!assert (gammainc ([2, Inf], [Inf, 2]), [0, 1])
%!assert (gammainc ([2, Inf], [Inf, 2], "upper"), [1, 0])
%!assert (gammainc ([2, Inf], [Inf, 2], "scaledlower"), [1, Inf])
%!assert (gammainc ([2, Inf], [Inf, 2], "scaledupper"), [Inf, 0])

## Test: case 5
## Matlab fails for this test
%!assert (gammainc (-100,1,"upper"), exp (100), -eps)

## Test: case 6
%!assert (gammainc ([1, 2, 3], 1), 1 - exp (-[1, 2, 3]))
%!assert (gammainc ([1, 2, 3], 1, "upper"), exp (- [1, 2, 3]))
%!assert (gammainc ([1, 2, 3], 1, "scaledlower"), ...
%!        (exp ([1, 2, 3]) - 1) ./ [1, 2, 3])
%!assert (gammainc ([1, 2, 3], 1, "scaledupper"), 1 ./ [1, 2, 3])

## Test: case 7
%!assert (gammainc (2, 2, "lower"), 0.593994150290162, -2e-15)
%!assert (gammainc (2, 2, "upper"), 0.406005849709838, -2e-15)
%!assert (gammainc (2, 2, "scaledlower"), 2.194528049465325, -2e-15)
%!assert (gammainc (2, 2, "scaledupper"), 1.500000000000000, -2e-15)
%!assert (gammainc ([3 2 36],[2 3 18], "upper"), ...
%!        [4/exp(3) 5*exp(-2) (4369755579265807723 / 2977975)/exp(36)], -eps)
%!assert (gammainc (10, 10), 1 - (5719087 / 567) * exp (-10), -eps)
%!assert (gammainc (10, 10, "upper"), (5719087 / 567) * exp (-10), -eps)

## Test: case 8
%!assert (gammainc (-10, 10), 3.112658265341493126871617e7, -2*eps)
## Matlab fails this next one%!      %!
%!assert (isreal (gammainc (-10, 10)), true)
%!assert (gammainc (-10, 10.1, "upper"), ...
%!        -2.9582761911890713293e7-1i * 9.612022339061679758e6, -30*eps)
%!assert (gammainc (-10, 10, "upper"), -3.112658165341493126871616e7, ...
%!        -2*eps)
%!assert (gammainc (-10, 10, "scaledlower"), 0.5128019364747265, -1e-14)
%!assert (gammainc (-10, 10, "scaledupper"), -0.5128019200000000, -1e-14)
%!assert (gammainc (200, 201, "upper"), 0.518794309678684497, -2 * eps)
%!assert (gammainc (200, 201, "scaledupper"),
%!        18.4904360746560462660798514, -eps)
## Here we are very good (no D (x,a)) involved
%!assert (gammainc (1000, 1000.5, "scaledlower"), 39.48467539583672271, -2*eps)
%!assert (gammainc (709, 1000, "upper"), 0.99999999999999999999999954358, -eps)

## Test: case 9
%!test <*47800>
%! assert (gammainc (60, 6, "upper"), 6.18022358081160257327264261e-20,
%!         -10*eps);
## Matlab is better here than Octave
%!assert (gammainc (751, 750, "upper"), 0.4805914320558831327179457887, -12*eps)
%!assert (gammainc (200, 200, "upper"), 0.49059658199276367497217454, -6*eps)
%!assert (gammainc (200, 200), 0.509403418007236325027825459574527043, -5*eps)
%!assert (gammainc (200, 200, "scaledupper"), 17.3984438553791505135122900,
%!       -3*eps)
%!assert (gammainc (200, 200, "scaledlower"), 18.065406676779221643065, -8*eps)
%!assert (gammainc (201, 200, "upper"), 0.46249244908276709524913736667,
%!        -7*eps)
%!assert <*54550> (gammainc (77, 2), 1)

%!assert (gammainc (77, 2, "upper"), 0, -eps)
%!assert (gammainc (1000, 3.1), 1)
%!assert (gammainc (1000, 3.1, "upper"), 0)

## Test small argument
%!assert (gammainc ([1e-05, 1e-07,1e-10,1e-14], 0.1), ...
%!        [0.33239840504050, 0.20972940370977, 0.10511370061022, ...
%!        0.041846517936723], 1e-13);

%!assert (gammainc ([1e-05, 1e-07,1e-10,1e-14], 0.2), ...
%!        [0.10891226058559, 0.043358823442178, 0.010891244210402, ...
%!        0.0017261458806785], 1e-13);

%!test
%!assert (gammainc ([1e-02, 1e-03, 1e-5, 1e-9, 1e-14], 0.9), ...
%!        [0.016401189184068, 0.0020735998660840, 0.000032879756964708, ...
%!        8.2590606569241e-9, 2.6117443021738e-13], -1e-12);

%!test
%!assert (gammainc ([1e-02, 1e-03, 1e-5, 1e-9, 1e-14], 2), ...
%!        [0.0000496679133402659, 4.99666791633340e-7, 4.99996666679167e-11, ...
%!        4.99999999666667e-19, 4.99999999999997e-29], -1e-12);

%!test <*53543>
%! y_exp = 9.995001666250085e-04;
%! assert (gammainc (1/1000, 1), y_exp, -eps);

%!test <53612>
%! assert (gammainc (-20, 1.1, "upper"), ...
%!         6.50986687074979e8 + 2.11518396291149e8*i, -1e-13);

## Test conservation of the class (five tests for each subroutine).
%!assert (class (gammainc (0, 1)) == "double")
%!assert (class (gammainc (single (0), 1)) == "single")
%!assert (class (gammainc (int8 (0), 1)) == "double")
%!assert (class (gammainc (0, single (1))) == "single")
%!assert (class (gammainc (0, int8 (1))) == "double")
%!assert (class (gammainc (1, 0)) == "double")
%!assert (class (gammainc (single (1), 0)) == "single")
%!assert (class (gammainc (int8 (1), 0)) == "double")
%!assert (class (gammainc (1, single (0))) == "single")
%!assert (class (gammainc (1, int8 (0))) == "double")
%!assert (class (gammainc (1, 1)) == "double")
%!assert (class (gammainc (single (1), 1)) == "single")
%!assert (class (gammainc (int8 (1), 1)) == "double")
%!assert (class (gammainc (1, single (1))) == "single")
%!assert (class (gammainc (1, int8 (1))) == "double")
%!assert (class (gammainc (1, 2)) == "double")
%!assert (class (gammainc (single (1), 2)) == "single")
%!assert (class (gammainc (int8 (1), 2)) == "double")
%!assert (class (gammainc (1, single (2))) == "single")
%!assert (class (gammainc (1, int8 (2))) == "double")
%!assert (class (gammainc (-1, 0.5)) == "double")
%!assert (class (gammainc (single (-1), 0.5)) == "single")
%!assert (class (gammainc (int8 (-1), 0.5)) == "double")
%!assert (class (gammainc (-1, single (0.5))) == "single")
%!assert (class (gammainc (-1, int8 (0.5))) == "double")
%!assert (class (gammainc (1, 0.5)) == "double")
%!assert (class (gammainc (single (1), 0.5)) == "single")
%!assert (class (gammainc (int8 (1), 0.5)) == "double")
%!assert (class (gammainc (1, single (0.5))) == "single")
%!assert (class (gammainc (1, int8 (0.5))) == "double")

## Test input validation
%!error <Invalid call> gammainc ()
%!error <Invalid call> gammainc (1)
%!error <must be of common size or scalars> gammainc ([0, 0],[0; 0])
%!error <must be of common size or scalars> gammainc ([1 2 3], [1 2])
%!error <all inputs must be real> gammainc (2+i, 1)
%!error <all inputs must be real> gammainc (1, 2+i)
%!error <A must be non-negative> gammainc (1, [0, -1, 1])
%!error <A must be non-negative>
%! a = ones (2,2,2);
%! a(1,1,2) = -1;
%! gammainc (1, a);
%!error <invalid value for TAIL> gammainc (1,2, "foobar")
