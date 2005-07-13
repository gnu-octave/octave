## Copyright (C) 1997  Kurt Hornik
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
## @deftypefn {Function File} {} hygecdf (@var{x}, @var{m}, @var{t}, @var{n})
## Compute the cumulative distribution function (CDF) at @var{x} of the
## hypergeometric distribution with parameters @var{m}, @var{t}, and
## @var{n}.  This is the probability of obtaining not more than @var{x}
## marked items when randomly drawing a sample of size @var{n} without
## replacement from a population of total size @var{t} containing
## @var{m} marked items.
##
## The parameters @var{m}, @var{t}, and @var{n} must positive integers
## with @var{m} and @var{n} not greater than @var{t}.
## @end deftypefn

## Author: KH <Kurt.Hornik@ci.tuwien.ac.at>
## Description: CDF of the hypergeometric distribution

function cdf = hygecdf (x, m, t, n)

  if (nargin != 4)
    usage ("hygecdf (x, m, t, n)");
  endif

  if (!isscalar (m) || !isscalar (t) || !isscalar (n))
    error ("hygecdf: m, t and n must all be positive integers");
  endif

  if ((m < 0) | (t < 0) | (n <= 0) | (m != round (m)) |
      (t != round (t)) | (n != round (n)) | (m > t) | (n > t))
    cdf = NaN * ones (size (x))
  else
    cdf = discrete_cdf (x, 0 : n, hygepdf (0 : n, m, t, n));
  endif

endfunction


