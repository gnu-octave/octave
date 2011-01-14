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
## @deftypefn {Function File} {} stdnormal_cdf (@var{x})
## For each component of @var{x}, compute the CDF of the standard normal
## distribution at @var{x}.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: CDF of the standard normal distribution

function cdf = stdnormal_cdf (x)

  if (nargin != 1)
    print_usage ();
  endif

  sz = size (x);
  if (numel(x) == 0)
    error ("stdnormal_cdf: X must not be empty");
  endif

  cdf = erfc (x / (-sqrt(2))) / 2;

endfunction




