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

## The following functions are available for computing the values of
## Bessel functions:
##
##   besselj (alpha, x)   Bessel functions of the first kind
##   bessely (alpha, x)   Bessel functions of the second kind
##   besseli (alpha, x)   modified Bessel functions of the first kind
##   besselk (alpha, x)   modified Bessel functions of the second kind
##
## X must be a real matrix, vector or scalar.
##
## If ALPHA is a scalar, the result is the same size as X.  If ALPHA is
## a range, X must be a vector or scalar, and the result is a matrix
## with length(X) rows and length(ALPHA) columns.
##
## ALPHA must be greater than or equal to zero.  If ALPHA is a range, it
## must have an increment equal to one.

function bessel ()
  error ("bessel: you must use besselj, bessely, besseli, or besselk");
endfunction
