## Copyright (C) 1995, 1996, 1997  Kurt Hornik
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
## @deftypefn {Function File} {} laplace_pdf (@var{x})
## For each element of @var{x}, compute the probability density function
## (PDF) at @var{x} of the Laplace distribution.
## @end deftypefn

## Author: KH <Kurt.Hornik@ci.tuwien.ac.at>
## Description: PDF of the Laplace distribution

function pdf = laplace_pdf (x)

  if (nargin != 1)
    usage ("laplace_pdf (x)");
  endif

  pdf = zeros (size (x));

  k = find (isnan (x));
  if (any (k))
    pdf(k) = NaN;
  endif

  k = find ((x > -Inf) & (x < Inf));
  if (any (k))
    pdf(k) = exp (- abs (x(k))) / 2;
  endif

endfunction
