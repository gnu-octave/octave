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

## -*- texinfo -*-
## @deftypefn {Function File} {[@var{h}, @var{w}] =} freqz (@var{b}, @var{a}, @var{n}, "whole")
## Return the complex frequency response @var{h} of the rational IIR filter
## whose numerator and denominator coefficients are @var{b} and @var{a},
## respectively.  The response is evaluated at @var{n} angular frequencies
## between 0 and
## @ifinfo
##  2*pi.
## @end ifinfo
## @iftex
## @tex
##  $2\pi$.
## @end tex
## @end iftex
##
## @noindent
## The output value @var{w} is a vector of the frequencies.
##
## If the fourth argument is omitted, the response is evaluated at
## frequencies between 0 and
## @ifinfo
##  pi.
## @end ifinfo
## @iftex
## @tex
##  $\pi$.
## @end tex
## @end iftex
##
## If @var{n} is omitted, a value of 512 is assumed.
##
## If @var{a} is omitted, the denominator is assumed to be 1 (this
## corresponds to a simple FIR filter).
##
## For fastest computation, @var{n} should factor into a small number of
## small primes.
## @end deftypefn

## Author: jwe ???

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
