function r = roots(v)
#
# For a vector v with n components, return the roots of the polynomial
# v(1)*z^(n-1) + ... + v(n-1) * z + v(n).
  
# written by KH (Kurt.Hornik@ci.tuwien.ac.at) on Dec 24, 1993
# copyright Dept of Probability Theory and Statistics TU Wien
# modified by KH on Jan 10, 1994
  
  [nr, nc] = size(v);
  if !((nr == 1 && nc > 1) || (nc == 1 && nr > 1))
    error("usage:  roots(v), where v is a nonzero vector");
  endif
  n = nr + nc - 1;
  v = reshape(v,1,n);
  # If v = [ 0 ... 0 v(k+1) ... v(k+l) 0 ... 0 ], we can remove the
  # leading k zeros and n-k-l roots of the polynomial are zero.  
  f = find(v);
  m = max(size(f));
  if (m > 0)
    v = v(f(1):f(m));
    l = max(size(v));
    if (l > 1)
      A = diag(ones(1, l-2), -1);
      A(1,:) = -v(2:l) ./ v(1);
      r = eig(A);    
      if (f(m) < n)
	r = [r; zeros(n-f(m), 1)];
      endif
    else
      r = zeros(n-f(m), 1);
    endif
  else
    error("usage:  roots(v), where v is a nonzero vector");
  endif
  
endfunction
