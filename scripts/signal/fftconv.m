## Copyright (C) 1994, 1995, 1996, 1997, 1999, 2000, 2002, 2005, 2006,
##               2007 John W. Eaton
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or (at
## your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn  {Function File} {} fftconv (@var{a}, @var{b})
## @deftypefnx {Function File} {} fftconv (@var{a}, @var{b}, @var{n})
## Convolve two vectors using the FFT for computation.
## 
## @code{c = fftconv (@var{a}, @var{b})} returns a vector of length equal to
## @code{length (@var{a}) + length (@var{b}) - 1}.
## If @var{a} and @var{b} are the coefficient vectors of two polynomials, the
## returned value is the coefficient vector of the product polynomial.
##
## The computation uses the FFT by calling the function @code{fftfilt}.  If
## the optional argument @var{n} is specified, an N-point FFT is used.
## @seealso{deconv, conv, conv2}
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Created: 3 September 1994
## Adapted-By: jwe

function c = fftconv (a, b, N)

  if (nargin < 2 || nargin > 3)
    print_usage ();
  endif

  if (! (isvector (a) && isvector (b)))
    error ("fftconv: both A and B must be vectors");
  endif
  la = length (a);
  lb = length (b);
  if ((la == 1) || (lb == 1))
    c = a * b;
  else
    lc = la + lb - 1;
    a(lc) = 0;
    b(lc) = 0;
    if (nargin == 2)
      c = fftfilt (a, b);
    else
      if (! (isscalar (N)))
        error ("fftconv: N must be a scalar");
      endif
      c = fftfilt (a, b, N);
    endif
  endif

endfunction

%% FIXME: Borrow tests from conv.m.  May need a tolerance on the assert comparison
