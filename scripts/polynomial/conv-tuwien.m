function c=conv(a,b)
#
# usage: conv(a,b)
#
# Returns the convolution of vectors a and b. The resulting vector
# is of length(a)+length(b)-1. If a and b are polynomial coefficients
# conv(a,b) is equivalent to polynomial multiplication.

# written by Gerhard Kircher on Aug 27, 1993
# modified by KH (Kurt.Hornik@neuro.tuwien.ac.at) on Dec 23, 1993.

  l_a = length(a);
  l_b = length(b);  
  return_row = 0;
  if (l_a > l_b)
    if (rows(a) == 1)
      return_row = 1;
    endif
  else
    if (rows(b) == 1)
      return_row = 1; 
    endif
  endif
  a = reshape(a, l_a, 1);
  b = reshape(b, l_b, 1);
  if (l_a == 1 || l_b == 1)
    c = a * b;
  else
    l_c = l_a + l_b - 1;
    a(l_c) = 0;
    b(l_c) = 0;
    c = ifft(fft(a) .* fft(b));
    if !( any(imag(a)) || any(imag(b)) )
      c = real(c);
    endif
    if !( any(a-round(a)) || any(b-round(b)) )
      c = round(c);
    endif
  endif
  if (return_row == 1)
    c = c';
  end
endfunction
