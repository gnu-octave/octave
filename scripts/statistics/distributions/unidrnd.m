## Copyright (C) 2007  David Bateman
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
## @deftypefn {Function File} {} unidrnd (@var{v}, @var{r}, @var{c})
## @deftypefnx {Function File} {} unidrnd (@var{v}, @var{sz})
## Generate a row vector containing a random sample of values from
## the univariate distribution which assumes the values in @var{v} with
## eqal probability. If @var{v} is a scalar, it is promoted to @code{1:@var{v}}.
##
## If @var{r} and @var{c} are given create a matrix with @var{r} rows and
## @var{c} columns. Or if @var{sz} is a vector, create a matrix of size
## @var{sz}.
## @end deftypefn

function rnd = unidrnd (v, varargin)

  if (nargin != 2)
    print_usage ();
  endif

  if (isscalar(v))
    v = [1:v].';
  else
    v = v(:);
  endif

  rnd = discrete_rnd (v, ones(size(v)), varargin{:});
endfunction
