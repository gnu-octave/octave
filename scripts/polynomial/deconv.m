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
## @deftypefn {Function File} {} deconv (@var{y}, @var{a})
## Deconvolve two vectors.
##
## @code{[b, r] = deconv (y, a)} solves for @var{b} and @var{r} such that
## @code{y = conv (a, b) + r}.
##
## If @var{y} and @var{a} are polynomial coefficient vectors, @var{b} will
## contain the coefficients of the polynomial quotient and @var{r} will be
## a remander polynomial of lowest order.
## @seealso{conv, poly, roots, residue, polyval, polyderiv, polyinteg}
## @end deftypefn

## Author: Tony Richardson <arichard@stark.cc.oh.us>
## Created: June 1994
## Adapted-By: jwe

function [b, r] = deconv (y, a)

  if (nargin != 2)
    usage ("deconv (y, a)");
  endif

  if (! (isvector (y) && isvector (a)))
    error("conv: both arguments must be vectors");
  endif

  la = length (a);
  ly = length (y);

  lb = ly - la + 1;

  if (ly > la)
    b = filter (y, a, [1, (zeros (1, ly - la))]);
  elseif (ly == la)
    b = filter (y, a, 1);
  else
    b = 0;
  endif

  lc = la + length (b) - 1;
  if (ly == lc)
    r = y - conv (a, b);
  else
    r = [(zeros (1, lc - ly)), y] - conv (a, b);
  endif

endfunction
