%% Automatically generated from DejaGNU files

%% test/octave.test/arith/beta-1.m
%!test
%! a=[1, 1.5, 2, 3];
%! b=[4, 3, 2, 1];
%! v1=beta(a,b);
%! v2=beta(b,a);
%! v3=gamma(a).*gamma(b)./gamma(a+b);
%! assert(all(abs(v1-v2)<sqrt(eps)) && all(abs(v2-v3)<sqrt(eps)));

%% test/octave.test/arith/beta-2.m
%!error beta();

%% test/octave.test/arith/beta-3.m
%!error beta(1);

%% test/octave.test/arith/betainc-1.m
%!test
%! a=[1, 1.5, 2, 3];
%! b=[4, 3, 2, 1];
%! v1=betainc(1,a,b);
%! v2=[1,1,1,1];
%! x = [.2, .4, .6, .8];
%! v3=betainc(x, a, b);
%! v4 = 1-betainc(1.-x, b, a);
%! assert(all(abs(v1-v2)<sqrt(eps)) && all(abs(v3-v4)<sqrt(eps)));

%% test/octave.test/arith/betainc-2.m
%!error <Invalid call to betainc.*> betainc();

%% test/octave.test/arith/betainc-3.m
%!error <Invalid call to betainc.*> betainc(1);

%% test/octave.test/arith/betainc-4.m
%!error <Invalid call to betainc.*> betainc(1,2);

%% test/octave.test/arith/ceil-1.m
%!assert(all (ceil ([2, 1.1, -1.1, -1]) == [2, 2, -1, -1]));

%% test/octave.test/arith/ceil-2.m
%!assert(all (ceil ([2+2i, 1.1+1.1i, -1.1-1.1i, -1-i]) == [2+2i, 2+2i, -1-i, -1-i]));

%% test/octave.test/arith/ceil-3.m
%!error ceil ();

%% test/octave.test/arith/ceil-4.m
%!error ceil (1, 2);

%% test/octave.test/arith/erf-1.m
%!test
%! x=[0,.5,1];
%! v=[0, .520499877813047, .842700792949715];
%! assert(all(abs(erf(x)-v)<1.e-10) &&  all(abs(erf(-x)+v)<1.e-10) && all(abs(erfc(x)+v-1)<1.e-10) && all(abs(erfinv(v)-x)<1.e-10));

%% test/octave.test/arith/erf-2.m
%!error erf();

%% test/octave.test/arith/erf-3.m
%!error erf(1,2);

%% test/octave.test/arith/exp-1.m
%!assert(all (abs (exp ([0, 1, -1, -1000]) - [1, e, 1/e, 0]) < sqrt (eps)));

%% test/octave.test/arith/exp-2.m
%!assert(abs (exp (1+i) - e * (cos (1) + sin (1) * i)) < sqrt (eps));

%% test/octave.test/arith/exp-3.m
%!error exp ();

%% test/octave.test/arith/exp-4.m
%!error exp (1, 2);

%% test/octave.test/arith/exp-5.m
%!assert(exp (Inf) == Inf && exp (-Inf) == 0 && isnan (exp (NaN)));

%% test/octave.test/arith/fix-1.m
%!assert(all (fix ([1.1, 1, -1.1, -1]) == [1, 1, -1, -1]));

%% test/octave.test/arith/fix-2.m
%!assert(all (fix ([1.1+1.1i, 1+i, -1.1-1.1i, -1-i]) == [1+i, 1+i, -1-i, -1-i]));

%% test/octave.test/arith/fix-3.m
%!error fix ();

%% test/octave.test/arith/fix-4.m
%!error fix (1, 2);

%% test/octave.test/arith/floor-1.m
%!assert(all (floor ([2, 1.1, -1.1, -1]) == [2, 1, -2, -1]));

%% test/octave.test/arith/floor-2.m
%!assert(all (floor ([2+2i, 1.1+1.1i, -1.1-1.1i, -1-i]) == [2+2i, 1+i, -2-2i, -1-i]));

%% test/octave.test/arith/floor-3.m
%!error floor ();

%% test/octave.test/arith/floor-4.m
%!error floor (1, 2);

%% test/octave.test/arith/gamma-1.m
%!test
%! x = [.5, 1, 1.5, 2, 3, 4, 5];
%! v = [sqrt(pi), 1, .5*sqrt(pi), 1, 2, 6, 24];
%! assert(all(abs(gamma(x) - v) < sqrt(eps)) && all(abs(lgamma(x) - log(v)) < sqrt(eps))
%! );

%% test/octave.test/arith/gamma-2.m
%!error gamma();

%% test/octave.test/arith/gamma-3.m
%!error gamma(1,2);

%% test/octave.test/arith/gammainc-1.m
%!test
%! a = [.5 .5 .5 .5 .5];
%! x = [0 1 2 3 4];
%! v1 = sqrt(pi)*erf(x)./gamma(a);
%! v3 = gammainc(x.*x,a);
%! assert(all (abs(v1 - v3) < sqrt(eps)));

%% test/octave.test/arith/gcd-1.m
%!assert((gcd (200, 300, 50, 35) == gcd ([200, 300, 50, 35])
%! && gcd ([200, 300, 50, 35]) == 5));

%% test/octave.test/arith/gcd-2.m
%!error <Invalid call to gcd.*> gcd ();

%% test/octave.test/arith/gcd-3.m
%!test
%! s.a = 1;
%! fail("gcd (s)");

%% test/octave.test/arith/lcm-1.m
%!assert(lcm (3, 5, 7, 15) == lcm ([3, 5, 7, 15]) && lcm ([3, 5, 7,15]) == 105);

%% test/octave.test/arith/lcm-2.m
%!error lcm ();

%% test/octave.test/arith/lcm-3.m
%!test
%! s.a = 1;
%! fail("lcm (s)");

%% test/octave.test/arith/max-1.m
%!assert (max ([1, 4, 2, 3]) == 4);
%!assert (max ([1; -10; 5; -2]) == 5);

%% test/octave.test/arith/max-2.m
%!assert(all (max ([4, i 4.999; -2, 2, 3+4i]) == [4, 2, 3+4i]));

%% test/octave.test/arith/max-3.m
%!error <Invalid call to max.*> max ();

%% test/octave.test/arith/max-4.m
%!error <Invalid call to max.*> max (1, 2, 3, 4);

%% test/octave.test/arith/min-1.m
%!assert (min ([1, 4, 2, 3]) == 1);
%!assert (min ([1; -10; 5; -2]) == -10);

%% test/octave.test/arith/min-2.m
%!assert(all (min ([4, i; -2, 2]) == [-2, i]));

%% test/octave.test/arith/min-3.m
%!error <Invalid call to min.*> min ();

%% test/octave.test/arith/min-4.m
%!error <Invalid call to min.*> min (1, 2, 3, 4);

%% test/octave.test/arith/pow2-1.m
%!test
%! x = [3, 0, -3];
%! v = [8, 1, .125];
%! assert(all (abs (pow2 (x) - v) < sqrt (eps)));

%% test/octave.test/arith/pow2-2.m
%!test
%! x = [3, 0, -3, 4, 0, -4, 5, 0, -5];
%! y = [-2, -2, -2, 1, 1, 1, 3, 3, 3];
%! z = x .* (2 .^ y);
%! assert(all (abs (pow2 (x,y) - z) < sqrt (eps))
%! );

%% test/octave.test/arith/pow2-3.m
%!error pow2();

%% test/octave.test/arith/rem-1.m
%!assert(all (all (rem ([1, 2, 3; -1, -2, -3], 2) == [1, 0, 1; -1, 0, -1])));

%% test/octave.test/arith/rem-2.m
%!assert(all (all (rem ([1, 2, 3; -1, -2, -3], 2 * ones (2, 3))
%! == [1, 0, 1; -1, 0, -1])));

%% test/octave.test/arith/rem-3.m
%!error rem ();

%% test/octave.test/arith/rem-4.m
%!error rem (1, 2, 3);

%% test/octave.test/arith/rem-5.m
%!error rem ([1, 2], [3, 4, 5]);

%% test/octave.test/arith/rem-6.m
%!error rem (i, 1);

%% test/octave.test/arith/round-1.m
%!assert((round (1) == 1 && round (1.1) == 1 && round (5.5) == 6
%! && round (i) == i && round (2.5+3.5i) == 3+4i && round (-2.6) == -3));

%% test/octave.test/arith/round-2.m
%!assert(all (all (round ([1.1, -2.4; -3.7, 7.1]) == [1, -2; -4, 7])));

%% test/octave.test/arith/round-3.m
%!error round ();

%% test/octave.test/arith/round-4.m
%!error round (1, 2);

%% test/octave.test/arith/sign-1.m
%!assert(sign (-2) == -1 && sign (3) == 1 && sign (0) == 0);

%% test/octave.test/arith/sign-2.m
%!assert(all (all (sign ([1, -pi; e, 0]) == [1, -1; 1, 0])));

%% test/octave.test/arith/sign-3.m
%!error sign ();

%% test/octave.test/arith/sign-4.m
%!error sign (1, 2);

%% test/octave.test/arith/sqrt-1.m
%!assert((sqrt (4) == 2 && sqrt (-1) == i
%! && abs (sqrt (1+i) - exp (0.5 * log (1+i))) < sqrt (eps)));

%% test/octave.test/arith/sqrt-2.m
%!test
%! t1 = exp (0.5 * log (i));
%! t2 = exp (0.5 * log (1-i));
%! assert(all (all (abs (sqrt ([4, -4; i, 1-i]) - [2, 2i; t1, t2]) < sqrt (eps))));

%% test/octave.test/arith/sqrt-3.m
%!error sqrt ();

%% test/octave.test/arith/sqrt-4.m
%!error sqrt (1, 2);

%% test/octave.test/arith/abs-1.m
%!assert(abs (1) == 1 && abs (-3.5) == 3.5 && abs (3+4i) == 5 && abs (3-4i) == 5);

%% test/octave.test/arith/abs-2.m
%!assert(all (all (abs ([1.1, 3i; 3+4i, -3-4i]) == [1.1, 3; 5, 5])));

%% test/octave.test/arith/abs-3.m
%!error abs ();

%% test/octave.test/arith/abs-4.m
%!error abs (1, 2);

%% test/octave.test/arith/xor-1.m
%!assert((xor ([1, 1, 0, 0], [0, 1, 0, 1]) == [1, 0, 0, 1]
%! && xor ([i, i, 0, 0], [1, 0, 1, 0]) == [0, 1, 1, 0]));

%% test/octave.test/arith/xor-2.m
%!assert(all (all (xor (eye (2), fliplr (eye (2))) == ones (2))));

%% test/octave.test/arith/xor-3.m
%!error xor ();

%% test/octave.test/arith/xor-4.m
%!error xor (1, 2, 3);

%% test/octave.test/arith/arg-1.m
%!assert(arg (1) == 0 && arg (i) == pi/2 && arg (-1) == pi && arg (-i) == -pi/2);

%% test/octave.test/arith/arg-2.m
%!assert(all (all (arg ([1, i; -1, -i]) == [0, pi/2; pi, -pi/2])));

%% test/octave.test/arith/arg-3.m
%!error arg ();

%% test/octave.test/arith/arg-4.m
%!error arg (1, 2);

%% test/octave.test/arith/conj-1.m
%!assert(conj (1) == 1 && conj (i) == -i && conj (1+i) == 1-i && conj (1-i) == 1+i);

%% test/octave.test/arith/conj-2.m
%!assert(all (all (conj ([-1, -i; -1+i, -1-i]) == [-1, i; -1-i, -1+i])));

%% test/octave.test/arith/conj-3.m
%!error conj ();

%% test/octave.test/arith/conj-4.m
%!error conj (1, 2);

%% test/octave.test/arith/imag-1.m
%!assert(imag (1) == 0 && imag (i) == 1 && imag (1+i) == 1);

%% test/octave.test/arith/imag-2.m
%!assert(all (all (imag ([i, 1; 1, i]) == eye (2))));

%% test/octave.test/arith/imag-3.m
%!error imag ();

%% test/octave.test/arith/imag-4.m
%!error imag (1, 2);

%% test/octave.test/arith/real-1.m
%!assert(real (1) == 1 && real (i) == 0 && real (1+i) == 1);

%% test/octave.test/arith/real-2.m
%!assert(all (all (real ([1, i; i, 1]) == eye (2))));

%% test/octave.test/arith/real-3.m
%!error real ();

%% test/octave.test/arith/real-4.m
%!error real (1, 2);

%% test/octave.test/arith/log-1.m
%!assert(all (abs (log ([1, e, e^2]) - [0, 1, 2]) < sqrt (eps)));

%% test/octave.test/arith/log-2.m
%!error log ();

%% test/octave.test/arith/log-3.m
%!error log (1, 2);

%% test/octave.test/arith/log10-1.m
%!assert(all (abs (log10 ([0.01, 0.1, 1, 10, 100]) - [-2, -1, 0, 1, 2]) < sqrt (eps)));

%% test/octave.test/arith/log10-2.m
%!error log10 ();

%% test/octave.test/arith/log10-3.m
%!error log10 (1, 2);

%% test/octave.test/arith/log2-1.m
%!assert(all (abs (log2 ([1/4, 1/2, 1, 2, 4]) - [-2, -1, 0, 1, 2]) < sqrt (eps)));

%% test/octave.test/arith/log2-2.m
%!error log2 ();

%% test/octave.test/arith/log2-3.m
%!error log2 (1, 2);

%% test/octave.test/arith/sin-1.m
%!test
%! rt2 = sqrt (2);
%! rt3 = sqrt (3);
%! x = [0, pi/6, pi/4, pi/3, pi/2, 2*pi/3, 3*pi/4, 5*pi/6, pi];
%! v = [0, 1/2, rt2/2, rt3/2, 1, rt3/2, rt2/2, 1/2, 0];
%! assert(all (abs (sin (x) - v) < sqrt (eps)));

%% test/octave.test/arith/sin-2.m
%!error sin ();

%% test/octave.test/arith/sin-3.m
%!error sin (1, 2);

%% test/octave.test/arith/cos-1.m
%!test
%! rt2 = sqrt (2);
%! rt3 = sqrt (3);
%! x = [0, pi/6, pi/4, pi/3, pi/2, 2*pi/3, 3*pi/4, 5*pi/6, pi];
%! v = [1, rt3/2, rt2/2, 1/2, 0, -1/2, -rt2/2, -rt3/2, -1];
%! assert(all (abs (cos (x) - v) < sqrt (eps)));

%% test/octave.test/arith/cos-2.m
%!error cos ();

%% test/octave.test/arith/cos-3.m
%!error cos (1, 2);

%% test/octave.test/arith/tan-1.m
%!test
%! rt2 = sqrt (2);
%! rt3 = sqrt (3);
%! x = [0, pi/6, pi/4, pi/3, 2*pi/3, 3*pi/4, 5*pi/6, pi];
%! v = [0, rt3/3, 1, rt3, -rt3, -1, -rt3/3, 0];
%! assert(all (abs (tan (x) - v) < sqrt (eps)));

%% test/octave.test/arith/tan-2.m
%!error tan ();

%% test/octave.test/arith/tan-3.m
%!error tan (1, 2);

%% test/octave.test/arith/sec-1.m
%!test
%! rt2 = sqrt (2);
%! rt3 = sqrt (3);
%! x = [0, pi/6, pi/4, pi/3, 2*pi/3, 3*pi/4, 5*pi/6, pi];
%! v = [1, 2*rt3/3, rt2, 2, -2, -rt2, -2*rt3/3, -1];
%! assert(all (abs (sec (x) - v) < sqrt (eps)));

%% test/octave.test/arith/sec-2.m
%!error sec ();

%% test/octave.test/arith/sec-3.m
%!error sec (1, 2);

%% test/octave.test/arith/csc-1.m
%!test
%! rt2 = sqrt (2);
%! rt3 = sqrt (3);
%! x = [pi/6, pi/4, pi/3, pi/2, 2*pi/3, 3*pi/4, 5*pi/6];
%! v = [2, rt2, 2*rt3/3, 1, 2*rt3/3, rt2, 2];
%! assert(all (abs (csc (x) - v) < sqrt (eps)));

%% test/octave.test/arith/csc-2.m
%!error csc ();

%% test/octave.test/arith/csc-3.m
%!error csc (1, 2);

%% test/octave.test/arith/cot-1.m
%!test
%! rt2 = sqrt (2);
%! rt3 = sqrt (3);
%! x = [pi/6, pi/4, pi/3, pi/2, 2*pi/3, 3*pi/4, 5*pi/6];
%! v = [rt3, 1, rt3/3, 0, -rt3/3, -1, -rt3];
%! assert(all (abs (cot (x) - v) < sqrt (eps)));

%% test/octave.test/arith/cot-2.m
%!error cot ();

%% test/octave.test/arith/cot-3.m
%!error cot (1, 2);

%% test/octave.test/arith/asin-1.m
%!test
%! rt2 = sqrt (2);
%! rt3 = sqrt (3);
%! v = [0, pi/6, pi/4, pi/3, pi/2, pi/3, pi/4, pi/6, 0];
%! x = [0, 1/2, rt2/2, rt3/2, 1, rt3/2, rt2/2, 1/2, 0];
%! assert(all (abs (asin (x) - v) < sqrt (eps))
%! 
%! );

%% test/octave.test/arith/asin-2.m
%!error asin ();

%% test/octave.test/arith/asin-3.m
%!error asin (1, 2);

%% test/octave.test/arith/acos-1.m
%!test
%! rt2 = sqrt (2);
%! rt3 = sqrt (3);
%! v = [0, pi/6, pi/4, pi/3, pi/2, 2*pi/3, 3*pi/4, 5*pi/6, pi];
%! x = [1, rt3/2, rt2/2, 1/2, 0, -1/2, -rt2/2, -rt3/2, -1];
%! assert(all (abs (acos (x) - v) < sqrt (eps))
%! 
%! );

%% test/octave.test/arith/acos-2.m
%!error acos ();

%% test/octave.test/arith/acos-3.m
%!error acos (1, 2);

%% test/octave.test/arith/atan-1.m
%!test
%! rt2 = sqrt (2);
%! rt3 = sqrt (3);
%! v = [0, pi/6, pi/4, pi/3, -pi/3, -pi/4, -pi/6, 0];
%! x = [0, rt3/3, 1, rt3, -rt3, -1, -rt3/3, 0];
%! assert(all (abs (atan (x) - v) < sqrt (eps))
%! 
%! );

%% test/octave.test/arith/atan-2.m
%!error atan ();

%% test/octave.test/arith/atan-3.m
%!error atan (1, 2);

%% test/octave.test/arith/asec-1.m
%!test
%! rt2 = sqrt (2);
%! rt3 = sqrt (3);
%! v = [0, pi/6, pi/4, pi/3, 2*pi/3, 3*pi/4, 5*pi/6, pi];
%! x = [1, 2*rt3/3, rt2, 2, -2, -rt2, -2*rt3/3, -1];
%! assert(all (abs (asec (x) - v) < sqrt (eps))
%! 
%! );

%% test/octave.test/arith/asec-2.m
%!error asec ();

%% test/octave.test/arith/asec-3.m
%!error asec (1, 2);

%% test/octave.test/arith/acsc-1.m
%!test
%! rt2 = sqrt (2);
%! rt3 = sqrt (3);
%! v = [pi/6, pi/4, pi/3, pi/2, pi/3, pi/4, pi/6];
%! x = [2, rt2, 2*rt3/3, 1, 2*rt3/3, rt2, 2];
%! assert(all (abs (acsc (x) - v) < sqrt (eps))
%! );

%% test/octave.test/arith/acsc-2.m
%!error acsc ();

%% test/octave.test/arith/acsc-3.m
%!error acsc (1, 2);

%% test/octave.test/arith/acot-1.m
%!test
%! rt2 = sqrt (2);
%! rt3 = sqrt (3);
%! x = [rt3, 1, rt3/3, 0, -rt3/3, -1, -rt3];
%! v = [pi/6, pi/4, pi/3, pi/2, -pi/3, -pi/4, -pi/6];
%! assert(all (abs (acot (x) - v) < sqrt (eps))
%! 
%! );

%% test/octave.test/arith/acot-2.m
%!error acot ();

%% test/octave.test/arith/acot-3.m
%!error acot (1, 2);

%% test/octave.test/arith/sinh-1.m
%!test
%! x = [0, pi/2*i, pi*i, 3*pi/2*i];
%! v = [0, i, 0, -i];
%! assert(all (abs (sinh (x) - v) < sqrt (eps)));

%% test/octave.test/arith/sinh-2.m
%!error sinh ();

%% test/octave.test/arith/sinh-3.m
%!error sinh (1, 2);

%% test/octave.test/arith/cosh-1.m
%!test
%! x = [0, pi/2*i, pi*i, 3*pi/2*i];
%! v = [1, 0, -1, 0];
%! assert(all (abs (cosh (x) - v) < sqrt (eps)));

%% test/octave.test/arith/cosh-2.m
%!error cosh ();

%% test/octave.test/arith/cosh-3.m
%!error cosh (1, 2);

%% test/octave.test/arith/tanh-1.m
%!test
%! x = [0, pi*i];
%! v = [0, 0];
%! assert(all (abs (tanh (x) - v) < sqrt (eps)));

%% test/octave.test/arith/tanh-2.m
%!error tanh ();

%% test/octave.test/arith/tanh-3.m
%!error tanh (1, 2);

%% test/octave.test/arith/sech-1.m
%!test
%! x = [0, pi*i];
%! v = [1, -1];
%! assert(all (abs (sech (x) - v) < sqrt (eps)));

%% test/octave.test/arith/sech-2.m
%!error sech ();

%% test/octave.test/arith/sech-3.m
%!error sech (1, 2);

%% test/octave.test/arith/csch-1.m
%!test
%! x = [pi/2*i, 3*pi/2*i];
%! v = [-i, i];
%! assert(all (abs (csch (x) - v) < sqrt (eps)));

%% test/octave.test/arith/csch-2.m
%!error csch ();

%% test/octave.test/arith/csch-3.m
%!error csch (1, 2);

%% test/octave.test/arith/coth-1.m
%!test
%! x = [pi/2*i, 3*pi/2*i];
%! v = [0, 0];
%! assert(all (abs (coth (x) - v) < sqrt (eps)));

%% test/octave.test/arith/coth-2.m
%!error coth ();

%% test/octave.test/arith/coth-3.m
%!error coth (1, 2);

%% test/octave.test/arith/asinh-1.m
%!test
%! v = [0, pi/2*i, 0, -pi/2*i];
%! x = [0, i, 0, -i];
%! assert(all (abs (asinh (x) - v) < sqrt (eps))
%! );

%% test/octave.test/arith/asinh-2.m
%!error asinh ();

%% test/octave.test/arith/asinh-3.m
%!error asinh (1, 2);

%% test/octave.test/arith/acosh-1.m
%!test
%! v = [0, pi/2*i, pi*i, pi/2*i];
%! x = [1, 0, -1, 0];
%! assert(all (abs (acosh (x) - v) < sqrt (eps))
%! );

%% test/octave.test/arith/acosh-2.m
%!error acosh ();

%% test/octave.test/arith/acosh-3.m
%!error acosh (1, 2);

%% test/octave.test/arith/atanh-1.m
%!test
%! v = [0, 0];
%! x = [0, 0];
%! assert(all (abs (atanh (x) - v) < sqrt (eps))
%! );

%% test/octave.test/arith/atanh-2.m
%!error atanh ();

%% test/octave.test/arith/atanh-3.m
%!error atanh (1, 2);

%% test/octave.test/arith/asech-1.m
%!test
%! v = [0, pi*i];
%! x = [1, -1];
%! assert(all (abs (asech (x) - v) < sqrt (eps))
%! );

%% test/octave.test/arith/asech-2.m
%!error asech ();

%% test/octave.test/arith/asech-3.m
%!error asech (1, 2);

%% test/octave.test/arith/acsch-1.m
%!test
%! v = [pi/2*i, -pi/2*i];
%! x = [-i, i];
%! assert(all (abs (acsch (x) - v) < sqrt (eps))
%! );

%% test/octave.test/arith/acsch-2.m
%!error acsch ();

%% test/octave.test/arith/acsch-3.m
%!error acsch (1, 2);

%% test/octave.test/arith/acoth-1.m
%!test
%! rt2 = sqrt (2);
%! rt3 = sqrt (3);
%! v = -i*[pi/6, pi/4, pi/3, -pi/3, -pi/4, -pi/6];
%! x = i*[rt3, 1, rt3/3, -rt3/3, -1, -rt3];
%! assert(all (abs (acoth (x) - v) < sqrt (eps)));

%% test/octave.test/arith/acoth-2.m
%!error acoth ();

%% test/octave.test/arith/acoth-3.m
%!error acoth (1, 2);

%% test/octave.test/arith/atan2-1.m
%!test
%! rt2 = sqrt (2);
%! rt3 = sqrt (3);
%! v = [0, pi/6, pi/4, pi/3, -pi/3, -pi/4, -pi/6, 0];
%! y = [0, rt3, 1, rt3, -rt3, -1, -rt3, 0];
%! x = [1, 3, 1, 1, 1, 1, 3, 1];
%! assert(all (abs (atan2 (y, x) - v) < sqrt (eps))
%! );

%% test/octave.test/arith/atan2-2.m
%!error <Invalid call to atan2.*> atan2 ();

%% test/octave.test/arith/atan2-3.m
%!error <Invalid call to atan2.*> atan2 (1, 2, 3);

%% test/octave.test/arith/sum-1.m
%!assert((sum ([1, 2, 3]) == 6 && sum ([-1; -2; -3]) == -6
%! && sum ([i, 2+i, -3+2i, 4]) == 3+4i));

%% test/octave.test/arith/sum-2.m
%!assert(all (all (sum ([1, 2, 3; i, 2i, 3i; 1+i, 2+2i, 3+3i]) == [2+2i, 4+4i, 6+6i])));

%% test/octave.test/arith/sum-3.m
%!error <Invalid call to sum.*> sum ();

%% test/octave.test/arith/sum-4.m
%!assert (all (sum ([1, 2; 3, 4], 1) == [4, 6]));
%!assert (all (sum ([1, 2; 3, 4], 2) == [3; 7]));
%!assert (sum (zeros (1, 0)) == 0);
%!assert (all (size (sum (zeros (1, 0), 1)) == [1, 0]));
%!assert (sum (zeros (1, 0), 2) == 0);
%!assert (sum (zeros (0, 1)) == 0);
%!assert (sum (zeros (0, 1), 1) == 0);
%!assert (all (size (sum (zeros (0, 1), 2)) == [0, 1]));
%!assert (all (size (sum (zeros (2, 0))) == [1, 0]));
%!assert (all (size (sum (zeros (2, 0), 1)) == [1, 0]));
%!assert (all (sum (zeros (2, 0), 2) == [0; 0]));
%!assert (all (sum (zeros (0, 2)) == [0, 0]));
%!assert (all (sum (zeros (0, 2), 1) == [0, 0]));
%!assert (all (size (sum (zeros (0, 2), 2)) == [0, 1]));
%!assert (all (size (sum (zeros (2, 2, 0, 3))) == [1, 2, 0, 3]));
%!assert (all (size (sum (zeros (2, 2, 0, 3), 2)) == [2, 1, 0, 3]));
%!assert (all (size (sum (zeros (2, 2, 0, 3), 3)) == [2, 2, 1, 3]));
%!assert (all (size (sum (zeros (2, 2, 0, 3), 4)) == [2, 2, 0]));
%!assert (all (size (sum (zeros (2, 2, 0, 3), 7)) == [2, 2, 0, 3]));

%% test/octave.test/arith/prod-1.m
%!assert (prod ([1, 2, 3]) == 6 && prod ([-1; -2; -3]) == -6);
%!assert (prod ([i, 2+i, -3+2i, 4]) == -4-32i);

%% test/octave.test/arith/prod-2.m
%!assert(all (all (prod ([1, 2, 3; i, 2i, 3i; 1+i, 2+2i, 3+3i])
%! == [-1+i, -8+8i, -27+27i])));

%% test/octave.test/arith/prod-3.m
%!error <Invalid call to prod.*> prod ();

%% test/octave.test/arith/prod-4.m
%!assert (all (prod ([1, 2; 3, 4], 1) == [3, 8]));
%!assert (all (prod ([1, 2; 3, 4], 2) == [2; 12]));
%!assert (prod (zeros (1, 0)) == 1);
%!assert (all (size (prod (zeros (1, 0), 1)) == [1, 0]));
%!assert (prod (zeros (1, 0), 2) == 1);
%!assert (prod (zeros (0, 1)) == 1);
%!assert (prod (zeros (0, 1), 1) == 1);
%!assert (all (size (prod (zeros (0, 1), 2)) == [0, 1]));
%!assert (all (size (prod (zeros (2, 0))) == [1, 0]));
%!assert (all (size (prod (zeros (2, 0), 1)) == [1, 0]));
%!assert (all (prod (zeros (2, 0), 2) == [1; 1]));
%!assert (all (prod (zeros (0, 2)) == [1, 1]));
%!assert (all (prod (zeros (0, 2), 1) == [1, 1]));
%!assert (all (size (prod (zeros (0, 2), 2)) == [0, 1]));

%% test/octave.test/arith/cumsum-1.m
%!assert (cumsum ([1, 2, 3]) == [1, 3, 6]);
%!assert (cumsum ([-1; -2; -3]) == [-1; -3; -6]);
%!assert (cumsum ([i, 2+i, -3+2i, 4]) == [i, 2+2i, -1+4i, 3+4i]);

%% test/octave.test/arith/cumsum-2.m
%!assert(all (all (cumsum ([1, 2, 3; i, 2i, 3i; 1+i, 2+2i, 3+3i])
%! == [1, 2, 3; 1+i, 2+2i, 3+3i; 2+2i, 4+4i, 6+6i])));

%% test/octave.test/arith/cumsum-3.m
%!error <Invalid call to cumsum.*> cumsum ();

%% test/octave.test/arith/cumsum-4.m
%!assert (all (cumsum ([1, 2; 3, 4], 1) == [1, 2; 4, 6]));
%!assert (all (cumsum ([1, 2; 3, 4], 2) == [1, 3; 3, 7]));

%% test/octave.test/arith/cumprod-1.m
%!assert (cumprod ([1, 2, 3]) == [1, 2, 6]);
%!assert (cumprod ([-1; -2; -3]) == [-1; 2; -6]);
%!assert (cumprod ([i, 2+i, -3+2i, 4]) == [i, -1+2i, -1-8i, -4-32i]);

%% test/octave.test/arith/cumprod-2.m
%!assert(all (all (cumprod ([1, 2, 3; i, 2i, 3i; 1+i, 2+2i, 3+3i])
%! == [1, 2, 3; i, 4i, 9i; -1+i, -8+8i, -27+27i])));

%% test/octave.test/arith/cumprod-3.m
%!error <Invalid call to cumprod.*> cumprod ();

%% test/octave.test/arith/cumprod-4.m
%!assert (all (cumprod ([2, 3; 4, 5], 1) == [2, 3; 8, 15]));
%!assert (all (cumprod ([2, 3; 4, 5], 2) == [2, 6; 4, 20]));

%% test/octave.test/arith/sumsq-1.m
%!assert(sumsq ([1, 2, 3]) == 14 && sumsq ([-1; -2; 4i]) == 21);

%% test/octave.test/arith/sumsq-2.m
%!assert(all (all (sumsq ([1, 2, 3; 2, 3, 4; 4i, 6i, 2]) == [21, 49, 29])));

%% test/octave.test/arith/sumsq-3.m
%!error <Invalid call to sumsq.*> sumsq ();

%% test/octave.test/arith/sumsq-4.m
%!assert (all (sumsq ([1, 2; 3, 4], 1) == [10, 20]));
%!assert (all (sumsq ([1, 2; 3, 4], 2) == [5; 25]));

%% test/octave.test/arith/bincoeff-1.m
%!assert(bincoeff (5, 2) == 10 && bincoeff (50, 6) == 15890700);

%% test/octave.test/arith/bincoeff-2.m
%!error bincoeff ();

%% test/octave.test/arith/bincoeff-3.m
%!error bincoeff (1, 2, 3);

