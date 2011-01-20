## Copyright (C) 1997-2011 Kurt Hornik
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
## @deftypefn {Function File} {} hygecdf (@var{x}, @var{t}, @var{m}, @var{n})
## Compute the cumulative distribution function (CDF) at @var{x} of the
## hypergeometric distribution with parameters @var{t}, @var{m}, and
## @var{n}.  This is the probability of obtaining not more than @var{x}
## marked items when randomly drawing a sample of size @var{n} without
## replacement from a population of total size @var{t} containing
## @var{m} marked items.
##
## The parameters @var{t}, @var{m}, and @var{n} must positive integers
## with @var{m} and @var{n} not greater than @var{t}.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: CDF of the hypergeometric distribution

function cdf = hygecdf (x, t, m, n)

  if (nargin != 4)
    print_usage ();
  endif

  if (!isscalar (t) || !isscalar (m) || !isscalar (n))
    error ("hygecdf: T, M and N must all be positive integers");
  endif

  if (t < 0 || m < 0 || n <= 0 || t != round (t) || m != round (m)
      || n != round (n) || m > t || n > t)
    cdf = NaN (size (x));
  else
    cdf = discrete_cdf (x, 0 : n, hygepdf (0 : n, t, m, n));
  endif

endfunction
