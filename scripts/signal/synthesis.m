## Copyright (C) 1995, 1996, 1997  Andreas Weingessel
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
## @deftypefn {Function File} {} synthesis (@var{y}, @var{c})
## Compute a signal from its short-time Fourier transform @var{y} and a
## 3-element vector @var{c} specifying window size, increment, and
## window type.
##
## The values @var{y} and @var{c} can be derived by
##
## @example
## [@var{y}, @var{c}] = stft (@var{x} , @dots{})
## @end example
## @end deftypefn

## Author: AW <Andreas.Weingessel@ci.tuwien.ac.at>
## Description: Recover a signal from its short-term Fourier transform

function X = synthesis (Y, c)

  if (nargin != 2)
    print_usage ();
  endif

  [nr, nc] = size (c);
  if (nr * nc != 3)
    error ("synthesis: c must contain exactly 3 elements");
  endif

  ## not necessary, enables better reading
  win = c(1);
  inc = c(2);
  w_type = c(3);

  if (w_type == 1)
    H = hanning (win);
  elseif (w_type == 2)
    H = hamming (win);
  elseif (w_type == 3)
    H = ones (win, 1);
  else
    error ("synthesis: window_type must be 1, 2, or 3");
  endif

  Z = real (ifft (Y));
  st = fix ((win-inc) / 2);
  Z = Z(st:st+inc-1, :);
  H = H(st:st+inc-1);

  nc = columns(Z);
  for i = 1:nc
    Z(:, i) = Z(:, i) ./ H;
  endfor

  X = reshape(Z, inc * nc, 1);

endfunction
