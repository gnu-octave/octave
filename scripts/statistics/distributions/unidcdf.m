## Copyright (C) 2007-2011 David Bateman
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
## @deftypefn {Function File} {} unidcdf (@var{x}, @var{v})
## For each element of @var{x}, compute the cumulative distribution
## function (CDF) at @var{x} of a univariate discrete distribution which
## assumes the values in @var{v} with equal probability.
## @end deftypefn

function cdf = unidcdf (x, v)

  if (nargin != 2)
    print_usage ();
  endif

  if (isscalar(v))
    v = [1:v].';
  else
    v = v(:);
  endif

  cdf = discrete_cdf (x, v, ones(size(v)));
endfunction
