function [h, w] = freqz(b,...)

# Compute the frequency response of a filter.
#
# [h,w] = resp(b)
#   returns the complex frequency response h of the FIR filter with
#   coefficients b. The response is evaluated at 512 angular frequencies
#   between 0 and pi.  w is a vector containing the 512 frequencies.
#
# [h,w] = resp(b,a)
#   returns the complex frequency response of the rational IIR filter
#   whose numerator has coefficients b and denominator coefficients a.
#
# [h,w] = resp(b,a,n)
#   returns the response evaluated at n angular frequencies.  For fastest
#   computation n should factor into a small number of small primes.
#
# [h,w] = freqz(b,a,n,"whole")
#   evaluates the response at n frequencies between 0 and 2*pi.

  if (nargin == 1)
    # Response of an FIR filter.
    a = 1;
    n = 512;
    region = "half";
  elseif (nargin == 2)
    # Response of an IIR filter
    a = va_arg();
    n = 512;
    region = "half";
  elseif (nargin == 3)
    a = va_arg();
    n = va_arg();
    region = "half";
  elseif (nargin == 4)
    a = va_arg();
    n = va_arg();
    region = va_arg();     
  endif

  la = length(a);
  a = reshape(a,1,la);
  lb = length(b);
  b = reshape(b,1,lb);

  k = max([la, lb]);

  if( n >= k)
    if (strcmp(region,"whole"))
      h = fft(postpad(b,n)) ./ fft(postpad(a,n));
      w = 2*pi*[0:(n-1)]/n;
    else
      h = fft(postpad(b,2*n)) ./ fft(postpad(a,2*n));
      h = h(1:n);
      w = pi*[0:(n-1)]/n;
    endif
  else
    if (strcmp(region,"whole"))
      w = 2*pi*[0:(n-1)]/n;
    else
      w = pi*[0:(n-1)]/n;
    endif    
    h = polyval(postpad(b,k),exp(j*w)) ./ polyval(postpad(a,k),exp(j*w));
  endif

endfunction
