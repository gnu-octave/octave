## Copyright (C) 1996, 1997 John W. Eaton
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2, or (at your option)
## any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, write to the Free
## Software Foundation, 59 Temple Place - Suite 330, Boston, MA
## 02111-1307, USA.

## Compute the frequency response of a filter.
##
## [h,w] = resp(b)
##   returns the complex frequency response h of the FIR filter with
##   coefficients b. The response is evaluated at 512 angular frequencies
##   between 0 and pi.  w is a vector containing the 512 frequencies.
##
## [h,w] = resp(b,a)
##   returns the complex frequency response of the rational IIR filter
##   whose numerator has coefficients b and denominator coefficients a.
##
## [h,w] = resp(b,a,n)
##   returns the response evaluated at n angular frequencies.  For fastest
##   computation n should factor into a small number of small primes.
##
## [h,w] = freqz(b,a,n,"whole")
##   evaluates the response at n frequencies between 0 and 2*pi.

## Author: jwe

function [h, w] = freqz(b,...)

  if (nargin == 1)
    ## Response of an FIR filter.
    a = 1;
    n = 512;
    region = "half";
  elseif (nargin == 2)
    ## Response of an IIR filter
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
