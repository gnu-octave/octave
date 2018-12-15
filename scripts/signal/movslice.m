## Copyright (C) 2018 Juan Pablo Carbajal
##
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or
## (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program. If not, see <http://www.gnu.org/licenses/>.

## Author: Juan Pablo Carbajal <ajuanpi+dev@gmail.com>
## Created: 2018-12-08

## -*- texinfo -*-
## @defun {@var{slc} =} movslice (@var{N}, @var{wlen})
## @defunx {[@dots{} @var{c} @var{pre} @var{pos} @var{w}] =} movslice (@dots{})
## Generate indices to slice a 1D array of length @var{N} in windows @var{wlen}
##
## TODO
## @seealso{movfun, reshapemod}
## @end defun

function [I, C, Cpre, Cpos, win] = movslice (N, wlen)
  # TODO asymmetric window
  if (isscalar (wlen))
    hwlen = (wlen - 1) / 2;
    wlen = [hwlen hwlen];
  endif

  Cpre = 1:wlen(1);               % centers that can't fit the pre-window
  Cnf  = N - wlen(2) + 1;         % first center that can't fit the post-window
  Cpos = Cnf:N;                   % centers that can't fit post-window
  C    = (wlen(1) + 1):(Cnf - 1);
  win  = (-wlen(1):wlen(2)).';
  I    = C + win;
endfunction

