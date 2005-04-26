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
## Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301, USA.

## -*- texinfo -*-
## @deftypefn {Mapping Function} {} besseli (@var{alpha}, @var{x})
## @deftypefnx {Mapping Function} {} besselj (@var{alpha}, @var{x})
## @deftypefnx {Mapping Function} {} besselk (@var{alpha}, @var{x})
## @deftypefnx {Mapping Function} {} bessely (@var{alpha}, @var{x})
## Compute Bessel functions of the following types:
##
## @table @code
## @item besselj
## Bessel functions of the first kind.
##
## @item bessely
## Bessel functions of the second kind.
##
## @item besseli
## Modified Bessel functions of the first kind.
##
## @item besselk
## Modified Bessel functions of the second kind.
## @end table
##
## The second argument, @var{x}, must be a real matrix, vector, or scalar.
##
## The first argument, @var{alpha}, must be greater than or equal to zero.
## If @var{alpha} is a range, it must have an increment equal to one.
##
## If @var{alpha} is a scalar, the result is the same size as @var{x}.
##
## If @var{alpha} is a range, @var{x} must be a vector or scalar, and the
## result is a matrix with @code{length(@var{x})} rows and
## @code{length(@var{alpha})} columns.
## @end deftypefn

function bessel ()
  error ("bessel: you must use besselj, bessely, besseli, or besselk");
endfunction

