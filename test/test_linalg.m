%% Automatically generated from DejaGNU files

%% test/octave.test/linalg/cond-1.m
%!assert(abs (cond ([1, 2; 2, 1]) - 3) < sqrt (eps));

%% test/octave.test/linalg/cond-2.m
%!assert(cond ([1, 2, 3; 4, 5, 6; 7, 8, 9]) > 1.0e+16);

%% test/octave.test/linalg/cond-3.m
%!error cond ();

%% test/octave.test/linalg/cond-4.m
%!error cond (1, 2);

%% test/octave.test/linalg/det-1.m
%!assert(det ([1, 2; 3, 4]) == -2);

%% test/octave.test/linalg/det-2.m
%!error <... det:.*> det ();

%% test/octave.test/linalg/det-3.m
%!error <... det:.*> det (1, 2);

%% test/octave.test/linalg/det-4.m
%!error det ([1, 2; 3, 4; 5, 6]);

%% test/octave.test/linalg/eig-1.m
%!assert(all (abs (eig ([1, 2; 2, 1]) - [-1; 3]) < sqrt (eps)));

%% test/octave.test/linalg/eig-2.m
%!test
%! [v, d] = eig ([1, 2; 2, 1]);
%! x = 1 / sqrt (2);
%! assert(((abs (d - [-1, 0; 0, 3]) < sqrt (eps))
%! && (abs (v - [-x, x; x, x]) < sqrt (eps))));

%% test/octave.test/linalg/eig-3.m
%!error <... eig:.*> eig ();

%% test/octave.test/linalg/eig-4.m
%!error <... eig:.*> eig ([1, 2; 3, 4], 2);

%% test/octave.test/linalg/eig-5.m
%!error eig ([1, 2; 3, 4; 5, 6]);

%% test/octave.test/linalg/expm-1.m
%!test
%! arg = [-49, 24; -64, 31];
%! result = [-0.735758758144742, 0.551819099658089;
%! -1.471517599088239, 1.103638240715556];
%! assert(all (all (abs (expm (arg) - result) < 128*eps)));

%% test/octave.test/linalg/expm-2.m
%!test
%! arg = [1, 1; 0, 1];
%! result = [2.718281828459045, 2.718281828459045;
%! 0.000000000000000, 2.718281828459045];
%! assert(all (all (abs (expm (arg) - result) < 4*eps)));

%% test/octave.test/linalg/expm-3.m
%!test
%! arg = diag ([6, 6, 6], 1);
%! result = [1, 6, 18, 36;
%! 0, 1,  6, 18;
%! 0, 0,  1,  6;
%! 0, 0,  0,  1];
%! assert(all (all (expm (arg) == result)));

%% test/octave.test/linalg/expm-4.m
%!error <expm:> expm();

%% test/octave.test/linalg/expm-5.m
%!error <expm:> expm(1,2);

%% test/octave.test/linalg/expm-6.m
%% test/octave.test/linalg/inv-1.m
%!assert(all (all (abs (inv ([1, 2; 3, 4]) - [-2, 1; 1.5, -0.5]) < sqrt (eps))));

%% test/octave.test/linalg/inv-2.m
%!error <... inv:.*> inv ();

%% test/octave.test/linalg/inv-3.m
%!error <... inv:.*> inv ([1, 2; 3, 4], 2);

%% test/octave.test/linalg/inv-4.m
%!error inv ([1, 2; 3, 4; 5, 6]);

%% test/octave.test/linalg/trace-1.m
%!assert(trace ([1, 2; 3, 4]) == 5);

%% test/octave.test/linalg/trace-2.m
%!assert(trace ([1, 2; 3, 4; 5, 6]) == 5);

%% test/octave.test/linalg/trace-3.m
%!assert(trace ([1, 3, 5; 2, 4, 6]) == 5);

%% test/octave.test/linalg/trace-4.m
%!error trace ();

%% test/octave.test/linalg/trace-5.m
%!error trace (1, 2);

%% test/octave.test/linalg/chol-1.m
%!test
%! rt2 = sqrt (2);
%! assert(all (all (abs (chol ([2, 1; 1, 1]) - [rt2, 1/rt2; 0, 1/rt2]) < sqrt (eps))));

%% test/octave.test/linalg/chol-2.m
%!error chol ([1, 2; 3, 4]);

%% test/octave.test/linalg/chol-3.m
%!error chol ([1, 2; 3, 4; 5, 6]);

%% test/octave.test/linalg/chol-4.m
%!error <... chol:.*> chol ();

%% test/octave.test/linalg/chol-5.m
%!error <... chol:.*> chol (1, 2);

%% test/octave.test/linalg/hess-1.m
%!test
%! a = [1, 2, 3; 5, 4, 6; 8, 7, 9];
%! [p, h] = hess (a);
%! assert(size (p) == [3, 3] && size (h) == [3, 3] && abs (a - p * h * p') < sqrt (eps));

%% test/octave.test/linalg/hess-2.m
%!error <... hess:.*> hess ();

%% test/octave.test/linalg/hess-3.m
%!error <... hess:.*> hess ([1, 2; 3, 4], 2);

%% test/octave.test/linalg/hess-4.m
%!error hess ([1, 2; 3, 4; 5, 6]);

%% test/octave.test/linalg/lu-1.m
%!assert(all (all (lu ([1, 2; 3, 4]) - [1/3, 1; 1, 0] < eps)));

%% test/octave.test/linalg/lu-2.m
%!test
%! [l, u] = lu ([1, 2; 3, 4]);
%! assert(((abs (l - [1/3, 1; 1, 0]) < sqrt (eps))
%! && abs (u - [3, 4; 0, 2/3]) < sqrt (eps)));

%% test/octave.test/linalg/lu-3.m
%!test
%! [l, u, p] = lu ([1, 2; 3, 4]);
%! assert((abs (l - [1, 0; 1/3, 1]) < sqrt (eps)
%! && abs (u - [3, 4; 0, 2/3]) < sqrt (eps)
%! && abs (p - [0, 1; 1, 0]) < sqrt (eps)));

%% test/octave.test/linalg/lu-4.m
%!error <... lu:.*> lu ();

%% test/octave.test/linalg/lu-5.m
%!error <... lu:.*> lu ([1, 2; 3, 4], 2);

%% test/octave.test/linalg/lu-6.m
%!test
%! [l u p] = lu ([1, 2; 3, 4; 5, 6]);
%! assert((abs (l - [1, 0; 1/5, 1; 3/5, 1/2]) < sqrt (eps)
%! && abs (u - [5, 6; 0, 4/5]) < sqrt (eps)
%! && abs (p - [0, 0, 1; 1, 0, 0; 0 1 0]) < sqrt (eps)));

%% test/octave.test/linalg/qr-1.m
%!test
%! a = [0, 2, 1; 2, 1, 2];
%! 
%! [q, r] = qr (a);
%! 
%! [qe, re] = qr (a, 0);
%! 
%! assert((size (q) == [2, 2] && size (r) == [2, 3]
%! && abs (q * r - a) < sqrt (eps)
%! && size (qe) == [2, 2] && size (re) == [2, 3]
%! && abs (qe * re - a) < sqrt (eps)));

%% test/octave.test/linalg/qr-2.m
%!test
%! a = [0, 2, 1; 2, 1, 2];
%! 
%! [q, r, p] = qr (a);  # not giving right dimensions. XXX FIXME XXX
%! 
%! [qe, re, pe] = qr (a, 0);
%! 
%! assert((size (q) == [2, 2] && size (r) == [2, 3] && size (p) == [3, 3]
%! && abs (q * r - a * p) < sqrt (eps)
%! && size (qe) == [2, 2] && size (re) == [2, 3] && size (pe) == [1, 3]
%! && abs (qe * re - a(:,pe)) < sqrt (eps)));

%% test/octave.test/linalg/qr-3.m
%!test
%! a = [0, 2; 2, 1; 1, 2];
%! 
%! [q, r] = qr (a);
%! 
%! [qe, re] = qr (a, 0);
%! 
%! assert((size (q) == [3, 3] && size (r) == [3, 2]
%! && abs (a - q * r) < sqrt (eps)
%! && size (qe) == [3, 2] && size (re) == [2, 2]
%! && abs (a - qe * re) < sqrt (eps)));

%% test/octave.test/linalg/qr-4.m
%!test
%! a = [0, 2; 2, 1; 1, 2];
%! 
%! [q, r, p] = qr (a);
%! 
%! [qe, re, pe] = qr (a, 0);
%! 
%! assert((size (q) == [3, 3] && size (r) == [3, 2] && size (p) == [2, 2]
%! && abs (a * p - q * r) < sqrt (eps)
%! && size (qe) == [3, 2] && size (re) == [2, 2] && size (pe) == [1, 2]
%! && abs (a(:,pe) - qe * re) < sqrt (eps)));

%% test/octave.test/linalg/qr-5.m
%!error <... qr:.*> qr ();

%% test/octave.test/linalg/qr-6.m
%!error <... qr:.*> qr ([1, 2; 3, 4], 0, 2);

%% test/octave.test/linalg/qr-7.m
%!function retval = testqr (q, r, a, p)
%!  tol = 10*eps;
%!  retval = 0;
%!  if (nargin == 3)
%!    n1 = norm (q*r-a);
%!    n2 = norm (q'*q-eye(columns(q)));
%!    retval = (n1 < tol && n2 < tol);
%!  else
%!    n1 = norm (q'*q-eye(columns(q)));
%!    retval = (n1 < tol);
%!    if (isvector (p))
%!      n2 = norm (q*r-a(:,p));
%!      retval = (retval && n2 < tol);
%!    else
%!      n2 = norm (q*r - a*p);
%!      retval = (retval && n2 < tol);
%!    endif
%!  endif
%!test
%! 
%! t = ones (24, 1);
%! j = 1;
%! 
%! if 0 # eliminate big matrix tests
%! a = rand(5000,20);
%! [q,r]=qr(a,0); t(j++) = testqr(q,r,a);
%! [q,r]=qr(a',0); t(j++) = testqr(q,r,a');
%! [q,r,p]=qr(a,0); t(j++) = testqr(q,r,a,p);
%! [q,r,p]=qr(a',0); t(j++) = testqr(q,r,a',p);
%! 
%! a = a+1i*eps;
%! [q,r]=qr(a,0); t(j++) = testqr(q,r,a);
%! [q,r]=qr(a',0); t(j++) = testqr(q,r,a');
%! [q,r,p]=qr(a,0); t(j++) = testqr(q,r,a,p);
%! [q,r,p]=qr(a',0); t(j++) = testqr(q,r,a',p);
%! endif
%! 
%! a = [ ones(1,15); sqrt(eps)*eye(15) ];
%! [q,r]=qr(a); t(j++) = testqr(q,r,a);
%! [q,r]=qr(a'); t(j++) = testqr(q,r,a');
%! [q,r,p]=qr(a); t(j++) = testqr(q,r,a,p);
%! [q,r,p]=qr(a'); t(j++) = testqr(q,r,a',p);
%! 
%! a = a+1i*eps;
%! [q,r]=qr(a); t(j++) = testqr(q,r,a);
%! [q,r]=qr(a'); t(j++) = testqr(q,r,a');
%! [q,r,p]=qr(a); t(j++) = testqr(q,r,a,p);
%! [q,r,p]=qr(a'); t(j++) = testqr(q,r,a',p);
%! 
%! a = [ ones(1,15); sqrt(eps)*eye(15) ];
%! [q,r]=qr(a,0); t(j++) = testqr(q,r,a);
%! [q,r]=qr(a',0); t(j++) = testqr(q,r,a');
%! [q,r,p]=qr(a,0); t(j++) = testqr(q,r,a,p);
%! [q,r,p]=qr(a',0); t(j++) = testqr(q,r,a',p);
%! 
%! a = a+1i*eps;
%! [q,r]=qr(a,0); t(j++) = testqr(q,r,a);
%! [q,r]=qr(a',0); t(j++) = testqr(q,r,a');
%! [q,r,p]=qr(a,0); t(j++) = testqr(q,r,a,p);
%! [q,r,p]=qr(a',0); t(j++) = testqr(q,r,a',p);
%! 
%! a = [
%! 611   196  -192   407    -8   -52   -49    29
%! 196   899   113  -192   -71   -43    -8   -44
%! -192   113   899   196    61    49     8    52
%! 407  -192   196   611     8    44    59   -23
%! -8   -71    61     8   411  -599   208   208
%! -52   -43    49    44  -599   411   208   208
%! -49    -8     8    59   208   208    99  -911
%! 29   -44    52   -23   208   208  -911    99
%! ];
%! [q,r] = qr(a);
%! 
%! assert(all (t) && norm(q*r-a) < 5000*eps);

%% test/octave.test/linalg/schur-1.m
%!test
%! a = [1, 2, 3; 4, 5, 9; 7, 8, 6];
%! [u, s] = schur (a);
%! assert(size (u) == [3, 3] && size (s) == [3, 3] && abs (s - u' * a * u) < sqrt (eps));

%% test/octave.test/linalg/schur-2.m
%!error <... schur:.*> schur ();

%% test/octave.test/linalg/schur-3.m
%!test
%! warn_num_to_str = 1;
%! fail("schur ([1, 2; 3, 4], 2)","warning");

%% test/octave.test/linalg/schur-4.m
%!error schur ([1, 2, 3; 4, 5, 6]);

%% test/octave.test/linalg/svd-1.m
%!assert(all (abs (svd ([1, 2; 2, 1]) - [3; 1]) < sqrt (eps)));

%% test/octave.test/linalg/svd-2.m
%!test
%! [u, s, v] = svd ([1, 2; 2, 1]);
%! x = 1 / sqrt (2);
%! assert(((abs (u - [-x, -x; -x, x]) < sqrt (eps))
%! && (abs (s - [3, 0; 0, 1]) < sqrt (eps))
%! && (abs (v - [-x, x; -x, -x]) < sqrt (eps))));

%% test/octave.test/linalg/svd-3.m
%!test
%! a = [1, 2, 3; 4, 5, 6];
%! [u, s, v] = svd (a);
%! assert((size (u) == [2, 2] && size (s) == [2, 3] && size (v) == [3, 3]
%! && abs (a - u * s * v') < sqrt (eps)));

%% test/octave.test/linalg/svd-4.m
%!test
%! a = [1, 2; 3, 4; 5, 6];
%! [u, s, v] = svd (a);
%! assert((size (u) == [3, 3] && size (s) == [3, 2] && size (v) == [2, 2]
%! && abs (a - u * s * v') < sqrt (eps)));

%% test/octave.test/linalg/svd-5.m
%!test
%! a = [1, 2, 3; 4, 5, 6];
%! [u, s, v] = svd (a, 1);
%! assert((size (u) == [2, 2] && size (s) == [2, 2] && size (v) == [3, 2]
%! && abs (a - u * s * v') < sqrt (eps)));

%% test/octave.test/linalg/svd-6.m
%!test
%! a = [1, 2; 3, 4; 5, 6];
%! [u, s, v] = svd (a, 1);
%! assert((size (u) == [3, 2] && size (s) == [2, 2] && size (v) == [2, 2]
%! && abs (a - u * s * v') < sqrt (eps)));

%% test/octave.test/linalg/svd-7.m
%!error <... svd:.*> svd ();

%% test/octave.test/linalg/svd-8.m
%!error <... svd:.*> svd ([1, 2; 4, 5], 2, 3);

%% test/octave.test/linalg/svd-9.m
%!error <... svd:.*> [u, v] = svd ([1, 2; 3, 4]);

%% test/octave.test/linalg/syl-1.m
%!test
%! x = syl ([1, 2; 3, 4], [5, 6; 7, 8], [9, 10; 11, 12]);
%! assert(all (all (abs (x - [-1/2, -2/3; -2/3, -1/2]) < sqrt (eps))));

%% test/octave.test/linalg/syl-2.m
%!error <... syl:.*> syl ();

%% test/octave.test/linalg/syl-3.m
%!error <... syl:.*> syl (1, 2, 3, 4);

%% test/octave.test/linalg/syl-4.m
%!error syl ([1, 2; 3, 4], [1, 2, 3; 4, 5, 6], [4, 3]);

