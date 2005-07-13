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
## @deftypefn {Function File} {} hypergeometric_rnd (@var{n_size}, @var{m}, @var{t}, @var{n})
## @deftypefnx {Function File} {} hypergeometric_rnd (@var{m}, @var{t}, @var{n}, @var{r}, @var{c})
## @deftypefnx {Function File} {} hypergeometric_rnd (@var{m}, @var{t}, @var{n}, @var{sz})
## Generate a row vector containing a random sample of size @var{n_size}
## from the hypergeometric distribution with parameters @var{m}, @var{t},
## and @var{n}.
##
## If  @var{r} and @var{c} are given create a matrix with @var{r} rows and
## @var{c} columns. Or if @var{sz} is a vector, create a matrix of size
## @var{sz}.
##
## The parameters @var{m}, @var{t}, and @var{n} must positive integers
## with @var{m} and @var{n} not greater than @var{t}.
## @end deftypefn

function rnd = hypergeometric_rnd (N, m, t, n)

  switch (nargin)
    case 3
      rnd = hygernd (m, t, N);
    case 4
      rnd = hygernd (m, t, N, n);
    otherwise
      usage ("rnd = hypergeometric_rnd (N, m, t, n)");
  endswitch

endfunction
