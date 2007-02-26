## Copyright (C) 2007  David Bateman
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
## @deftypefn {Function File} {} unidpdf (@var{x}, @var{v})
## For each element of @var{x}, compute the probability density function
## (pDF) at @var{x} of a univariate discrete distribution which assumes
## the values in @var{v} with equal probability.
## @end deftypefn

function pdf = unidpdf (x, v)

  if (nargin != 2)
    print_usage ();
  endif

  if (isscalar(v))
    v = [1:v].';
  else
    v = v(:);
  endif

  pdf = discrete_pdf (x, v, ones(size(v)));
endfunction
