function [g, v] = gcd (a, ...)
  
  # [g [, v]] = gcd (a) returns the greatest common divisor g of the
  # entries of the integer vector a, and an integer vector v such that
  # g = v(1) * a(k) + ... + v(k) * a(k).
  #
  # [g [, v]] = gcd (a1, ..., ak) is the same with a = [a1, ..., ak].
  
  # Written by KH (Kurt.Hornik@ci.tuwien.ac.at) on Sep 16, 1994
  # Copyright Dept of Statistics and Probability Theory TU Wien

  if (nargin > 1)
    va_start;
    for k=2:nargin;
      a = [a, va_arg()];
    endfor
  endif
  
  if (round(a) != a)
    error("gcd:  all arguments must be integer");
  endif
  
  g = abs(a(1));
  v = sign(a(1));
  for k=1:(length(a)-1)
    x = [g, 1, 0];
    y = [abs(a(k+1)), 0, 1];
    while (y(1) > 0)
      r = x - y * floor(x(1) / y(1));
      x = y;
      y = r;
    endwhile
    g = x(1);
    v = [x(2) * v, x(3) * sign(a(k+1))];
  endfor
    
endfunction
    