## Copyright (C) 2018 Juan Pablo Carbajal
##
## This file is part of Octave.
##
## Octave is free software: you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <https://www.gnu.org/licenses/>.

## Author: Juan Pablo Carbajal <ajuanpi+dev@gmail.com>
## Created: 2018-12-08

## -*- texinfo -*-
## @deftypefn  {} {@var{slcidx} =} movslice (@var{N}, @var{wlen})
## @deftypefnx {} {[@var{slcidx}, @var{C}, @var{Cpre}, @var{Cpost}, @var{win}] =} movslice (@dots{})
## Generate indices to slice a vector of length @var{N} in to windows
## of length @var{wlen}.
##
## FIXME: Document inputs N, wlen
##
## FIXME: Document outputs slcidx, C, Cpre, Cpost, win.
## @seealso{movfun}
## @end deftypefn

function [slcidx, C, Cpre, Cpost, win] = movslice (N, wlen)

  ## FIXME: Input validation for N, wlen

  ## FIXME: Eventually add asymmetric window
  if (isscalar (wlen))
    hwlen = (wlen - 1) / 2;
    wlen = [hwlen hwlen];
  endif

  Cpre  = 1:wlen(1);              # centers that can't fit the pre-window
  Cnf   = N - wlen(2) + 1;        # first center that can't fit the post-window
  Cpost = Cnf:N;                  # centers that can't fit post-window
  C     = (wlen(1) + 1):(Cnf - 1);
  win   = (-wlen(1):wlen(2)).';
  slcidx = C + win;

endfunction


## FIXME: Need BIST tests
