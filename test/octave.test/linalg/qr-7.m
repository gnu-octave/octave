function retval = testqr (q, r, a, p)
  tol = 512*eps;
  retval = 0;
  if (nargin == 3)
    n1 = norm (q*r-a)
    n2 = norm (q'*q-eye(columns(q)))
    retval = (n1 < tol && n2 < tol)
  else
    n1 = norm (q'*q-eye(columns(q)))
    retval = (n1 < tol);
    if (is_vector (p))
      n2 = norm (q*r-a(:,p))
      retval = (retval && n2 < tol);
    else
      n2 = norm (q*r - a*p)
      retval = (retval && n2 < tol);
    endif
  endif
endfunction

t = zeros (16, 1);
j = 1;
a = rand(5000,20);
[q,r]=qr(a,0); t(j++) = testqr(q,r,a);
[q,r]=qr(a',0); t(j++) = testqr(q,r,a');
[q,r,p]=qr(a,0); t(j++) = testqr(q,r,a,p);
[q,r,p]=qr(a',0); t(j++) = testqr(q,r,a',p);

a = a+1i*randn(size(a))*sqrt(eps);
[q,r]=qr(a,0); t(j++) = testqr(q,r,a);
[q,r]=qr(a',0); t(j++) = testqr(q,r,a');
[q,r,p]=qr(a,0); t(j++) = testqr(q,r,a,p);
[q,r,p]=qr(a',0); t(j++) = testqr(q,r,a',p);

a = [ ones(1,15); sqrt(eps)*eye(15) ];
[q,r]=qr(a); t(j++) = testqr(q,r,a);
[q,r]=qr(a'); t(j++) = testqr(q,r,a');
[q,r,p]=qr(a); t(j++) = testqr(q,r,a,p);
[q,r,p]=qr(a'); t(j++) = testqr(q,r,a',p);

a = a+1i*randn(size(a))*sqrt(eps);
[q,r]=qr(a); t(j++) = testqr(q,r,a);
[q,r]=qr(a'); t(j++) = testqr(q,r,a');
[q,r,p]=qr(a); t(j++) = testqr(q,r,a,p);
[q,r,p]=qr(a'); t(j++) = testqr(q,r,a',p);

all (t)
