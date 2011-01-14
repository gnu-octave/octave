## Copyright (C) 1995-2011 Kurt Hornik
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
## @deftypefn {Function File} {} chi2pdf (@var{x}, @var{n})
## For each element of @var{x}, compute the probability density function
## (PDF) at @var{x} of the chisquare distribution with @var{n} degrees
## of freedom.
## @end deftypefn

## Author: TT <Teresa.Twaroch@ci.tuwien.ac.at>
## Description: PDF of the chi-square distribution

function pdf = chi2pdf (x, n)

  if (nargin != 2)
    print_usage ();
  endif

  if (!isscalar (n))
    [retval, x, n] = common_size (x, n);
    if (retval > 0)
      error ("chi2pdf: X and N must be of common size or scalar");
    endif
  endif

  pdf = gampdf (x, n / 2, 2);

endfunction
