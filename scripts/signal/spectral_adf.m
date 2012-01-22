## Copyright (C) 1995-2012 Friedrich Leisch
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
## @deftypefn {Function File} {} spectral_adf (@var{c}, @var{win}, @var{b})
## Return the spectral density estimator given a vector of
## autocovariances @var{c}, window name @var{win}, and bandwidth,
## @var{b}.
##
## The window name, e.g., @code{"triangle"} or @code{"rectangle"} is
## used to search for a function called @code{@var{win}_sw}.
##
## If @var{win} is omitted, the triangle window is used.  If @var{b} is
## omitted, @code{1 / sqrt (length (@var{x}))} is used.
## @end deftypefn

## Author: FL <Friedrich.Leisch@ci.tuwien.ac.at>
## Description: Spectral density estimation

function retval = spectral_adf (c, win, b)

  cr = length (c);

  if (columns (c) > 1)
    c = c';
  endif

  if (nargin < 3)
    b = 1 / ceil (sqrt (cr));
  endif

  if (nargin == 1)
    w = triangle_lw (cr, b);
  else
    win = str2func (cstrcat (win, "_lw"));
    w = feval (win, cr, b);
  endif

  c = c .* w;

  retval = 2 * real (fft (c)) - c(1);
  retval = [(zeros (cr, 1)), retval];
  retval(:, 1) = (0 : cr-1)' / cr;

endfunction





