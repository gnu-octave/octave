function x = are(a,b,c,opt)

#
# usage: x = are(a,b,c{,opt})
#
# solves algebraic riccati equation
#
#    a' x + x a - x b x + c = 0
#
# for identically dimensioned square matrices a,b,c.  If b (c) is not square,
# then the function attempts to use b*b' (c'*c) instead.
#
# opt is an option passed to the eigenvalue balancing routine; default is 'B'
#
# see also: balance

if((nargin == 3) || (nargin == 4))
  if(nargin==4)
    if((opt ~= 'N') || (opt ~= 'P') || (opt ~= 'S') || (opt ~= 'B'))
      printf('warning: are: opt has an illegal value; setting to B');
      opt = 'B';
    endif
  else
    opt = 'B';
  endif
  if( (n = is_square(a) ) == 0)
    error('are: a is not square')
  endif
  if(is_controllable(a,b) == 0)
    printf('warning: are: a,b are not controllable')
  endif
  if( (m = is_square(b)) == 0)
    b = b*b';
    m = rows(b);
  endif
  if(is_observable(a,c) == 0)
    printf('warning: are: a,c are not observable')
  endif
  if( (p = is_square(c)) == 0)
    c = c'*c;
    p = rows(c);
  endif
  if( (n ~= m) || (n ~= p))
    error('are: a,b,c not conformably dimensioned.')
  endif

  # should check for controllability/observability here
  # use Boley-Golub (Syst. Contr. Letters, 1984) method, not the
  #                    n-1
  # rank([ B A*B ... A^   *B]) method 
  
  [u,s] = schur(balance([a,-b;-c,-a'],opt),'A');
  n1=n+1;
  n2 = 2*n;
  x = u(n1:n2,1:n)/u(1:n,1:n);
else
  error('usage: x = are(a,b,c)')
endif

endfunction
