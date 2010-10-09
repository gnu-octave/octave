## Copyright (C) 1997, 2005, 2006, 2007 Kurt Hornik
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
## @deftypefn {Function File} {} hygeinv (@var{x}, @var{t}, @var{m}, @var{n})
## For each element of @var{x}, compute the quantile at @var{x} of the
## hypergeometric distribution with parameters @var{t}, @var{m}, and
## @var{n}.
##
## The parameters @var{t}, @var{m}, and @var{n} must positive integers
## with @var{m} and @var{n} not greater than @var{t}.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Random deviates from the hypergeometric distribution

function inv = hygeinv (x, t, m, n)

  if (nargin != 4)
    print_usage ();
  endif

  if (!isscalar (t) || !isscalar (m) || !isscalar (n))
    error ("hygeinv: t, m and n must all be positive integers");
  endif

  if (t < 0 || m < 0 || n <= 0 || t != round (t) || m != round (m)
      || n != round (n) || m > t || n > t)
    inv = NaN (size (x))
  else
    inv = discrete_inv (x, 0 : n, hygepdf (0 : n, t, m, n));
  endif

endfunction
