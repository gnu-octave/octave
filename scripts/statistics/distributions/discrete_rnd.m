## Copyright (C) 1996, 1997  Kurt Hornik
##
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2, or (at your option)
## any later version.
##
## This program is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this file.  If not, write to the Free Software Foundation,
## 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

## -*- texinfo -*-
## @deftypefn {Function File} {} discrete_rnd (@var{n}, @var{v}, @var{p})
## Generate a row vector containing a random sample of size @var{n} from
## the univariate distribution which assumes the values in @var{v} with
## probabilities @var{p}.
##
## Currently, @var{n} must be a scalar.
## @end deftypefn

## Author: KH <Kurt.Hornik@ci.tuwien.ac.at>
## Description: Random deviates from a discrete distribution

function rnd = discrete_rnd (n, v, p)

  if (nargin != 3)
    usage ("discrete_rnd (n, v, p)");
  endif

  if (! is_scalar (n))
    error ("discrete_rnd: n must be a scalar");
  endif

  if (! is_vector (v))
    error ("discrete_rnd: v must be a vector");
  elseif (! is_vector (p) || (length (p) != length (v)))
    error ("discrete_rnd: p must be a vector with length (v) elements");
  elseif (! (all (p >= 0) && any (p)))
    error ("discrete_rnd: p must be a nonzero, nonnegative vector");
  endif

  u = rand (1, n);
  m = length (p);
  s = reshape (cumsum (p / sum (p)), m, 1);

  rnd = v (1 + sum ((s * ones (1, n)) <= ((ones (m, 1) * u))));

endfunction
