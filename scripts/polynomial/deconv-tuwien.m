function [x, r] = deconv(a, b)
#
# Returns x and r such that a = conv(b, x) + r.
# If a and b are vectors of polynomial coefficients, x and r are the
# vectors of coefficients of quotient and remainder in the polynomial
# division of a by b. 

# written by KH (Kurt.Hornik@neuro.tuwien.ac.at) on Dec 27, 1993
# copyright Dept of Probability Theory and Statistics TU Wien
  
  if !(nargin == 2)
    error("usage:  deconv(a, b)");
  endif
  f = find(b);
  l_b = length(f);
  if (l_b == 0)
    error("deconv(a, b):  b has to be nonzero");    
  endif
  l_a = length(a);
  if (l_a < l_b)
    x = 0;
    r = a;
  else
    b = reshape(b(f(1):f(l_b)), 1, l_b);
    a = reshape(a, 1, l_a);
    # the stupid way:
    if (l_b == 1)
      x = a / b;
    else
      x(1) = a(1) ./ b(1);
      for i = 2:l_a-l_b+1;
	x(i) = (a(i) - b(i:-1:2) * x(1:i-1)) ./ b(1);
      endfor
    endif
    r = a - conv(b, x);
  endif

endfunction
    








