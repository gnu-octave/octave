function retval = testqr (q, r, a, p)
  tol = 10*eps;
  retval = 0;
  if (nargin == 3)
    n1 = norm (q*r-a);
    n2 = norm (q'*q-eye(columns(q)));
    retval = (n1 < tol && n2 < tol);
  else
    n1 = norm (q'*q-eye(columns(q)));
    retval = (n1 < tol);
    if (isvector (p))
      n2 = norm (q*r-a(:,p));
      retval = (retval && n2 < tol);
    else
      n2 = norm (q*r - a*p);
      retval = (retval && n2 < tol);
    endif
  endif
endfunction

t = ones (24, 1);
j = 1;

if 0 # eliminate big matrix tests
a = rand(5000,20);
[q,r]=qr(a,0); t(j++) = testqr(q,r,a);
[q,r]=qr(a',0); t(j++) = testqr(q,r,a');
[q,r,p]=qr(a,0); t(j++) = testqr(q,r,a,p);
[q,r,p]=qr(a',0); t(j++) = testqr(q,r,a',p);

a = a+1i*eps;
[q,r]=qr(a,0); t(j++) = testqr(q,r,a);
[q,r]=qr(a',0); t(j++) = testqr(q,r,a');
[q,r,p]=qr(a,0); t(j++) = testqr(q,r,a,p);
[q,r,p]=qr(a',0); t(j++) = testqr(q,r,a',p);
endif

a = [ ones(1,15); sqrt(eps)*eye(15) ];
[q,r]=qr(a); t(j++) = testqr(q,r,a);
[q,r]=qr(a'); t(j++) = testqr(q,r,a');
[q,r,p]=qr(a); t(j++) = testqr(q,r,a,p);
[q,r,p]=qr(a'); t(j++) = testqr(q,r,a',p);

a = a+1i*eps;
[q,r]=qr(a); t(j++) = testqr(q,r,a);
[q,r]=qr(a'); t(j++) = testqr(q,r,a');
[q,r,p]=qr(a); t(j++) = testqr(q,r,a,p);
[q,r,p]=qr(a'); t(j++) = testqr(q,r,a',p);

a = [ ones(1,15); sqrt(eps)*eye(15) ];
[q,r]=qr(a,0); t(j++) = testqr(q,r,a);
[q,r]=qr(a',0); t(j++) = testqr(q,r,a');
[q,r,p]=qr(a,0); t(j++) = testqr(q,r,a,p);
[q,r,p]=qr(a',0); t(j++) = testqr(q,r,a',p);

a = a+1i*eps;
[q,r]=qr(a,0); t(j++) = testqr(q,r,a);
[q,r]=qr(a',0); t(j++) = testqr(q,r,a');
[q,r,p]=qr(a,0); t(j++) = testqr(q,r,a,p);
[q,r,p]=qr(a',0); t(j++) = testqr(q,r,a',p);

a = [
   611   196  -192   407    -8   -52   -49    29
   196   899   113  -192   -71   -43    -8   -44
  -192   113   899   196    61    49     8    52
   407  -192   196   611     8    44    59   -23
    -8   -71    61     8   411  -599   208   208
   -52   -43    49    44  -599   411   208   208
   -49    -8     8    59   208   208    99  -911
    29   -44    52   -23   208   208  -911    99
];
[q,r] = qr(a);

all (t) && norm(q*r-a) < 5000*eps
