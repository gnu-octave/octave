function l = lcm (a, ...)
  
  # lcm (a) returns the least common multiple of the entries of the
  # integer vector a.
  # lcm (a1, ..., ak) is the same as lcm([a1, ..., ak]).
  
  # Written by KH (Kurt.Hornik@ci.tuwien.ac.at) on Sep 16, 1994
  # Copyright Dept of Statistics and Probability Theory TU Wien

  if (nargin > 1)
    va_start;
    for k=2:nargin;
      a = [a, va_arg()];
    endfor
  endif
  
  if (round(a) != a)
    error("lcm:  all arguments must be integer");
  endif
  
  if (any(a) == 0)
    l = 0;
  else
    a = abs(a);
    l = a(1);
    for k=1:(length(a)-1)
      l = l * a(k+1) / gcd(l, a(k+1));
    endfor
  endif
    
endfunction
    