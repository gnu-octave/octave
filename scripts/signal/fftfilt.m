function y = fftfilt (b, x, N)

# usage:  fftfilt (b, x [, N])
#
# y = fftfilt (b, x) filters x with the FIR filter b using the FFT.
# y = fftfilt (b, x, N) uses the overlap-add method to filter x with
# b using an N-point FFT.

# Written by KH (Kurt.Hornik@ci.tuwien.ac.at) on Sep 3, 1994
# Copyright Dept of Statistics and Probability Theory TU Wien

# Reference:  Oppenheim & Schafer (1989).  Discrete-time Signal
# Processing (Chapter 8).  Prentice-Hall.

# If N is not specified explicitly, we do not use the overlap-add 
# method at all because loops are really slow.  Otherwise, we only
# ensure that the number of points in the FFT is the smallest power
# of two larger than N and length(b).  This could result in length
# one blocks, but if the user knows better ...
  
  if (nargin < 2 || nargin > 3)
    usage (" fftfilt (b, x [, N])");
  endif
  
  [r_x, c_x] = size (x);
  [r_b, c_b] = size (b);
  if !( (min ([r_x c_x]) == 1) || (min ([r_b c_b]) == 1) )
    error ("fftfilt:  both x and b should be vectors.");
  endif
  l_x  = r_x * c_x;
  l_b  = r_b * c_b;

  if ((l_x == 1) && (l_b == 1))
    y = b * x;  
    return;
  endif
  
  x    = reshape (x, 1, l_x);
  b    = reshape (b, 1, l_b);
  
  if (nargin == 2)
    # use FFT with the smallest power of 2 which is >= length (x) +
    # length (b) - 1 as number of points ...
    N    = 2^(ceil (log (l_x + l_b - 1) / log(2)));
    y    = ifft (fft (x, N) .* fft(b, N));
  else
    # use overlap-add method ...
    if !(is_scalar (N))
      error ("fftfilt:  N has to be a scalar");
    endif
    N    = 2^(ceil (log (max ([N l_b])) / log(2)));
    L    = N - l_b + 1;
    B    = fft (b, N);
    R    = ceil (l_x / L);
    y    = zeros (1, l_x);
    for r=1:R;
      lo  = (r - 1) * L + 1;
      hi  = min (r * L, l_x);
      tmp = ifft (fft (x(lo:hi), N) .* B);
      hi  = min (lo+N-1, l_x);
      y(lo:hi) = y(lo:hi) + tmp(1:(hi-lo+1));
    endfor  
  endif
    
  y    = reshape (y(1:l_x), r_x, c_x);

  # final cleanups:  if both x and b are real respectively integer, y
  # should also be so
  if !(any (imag (x)) || any (imag (b)))
    y = real (y);
  endif
  if !(any (x - round (x)) || any (b - round (b)))
    y = round (y);
  endif

endfunction






