## Copyright (C) 1996, 1997  Kurt Hornik
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
## @deftypefn {Function File} {} discrete_rnd (@var{n}, @var{v}, @var{p})
## @deftypefnx {Function File} {} discrete_rnd (@var{v}, @var{p}, @var{r}, @var{c})
## @deftypefnx {Function File} {} discrete_rnd (@var{v}, @var{p}, @var{sz})
## Generate a row vector containing a random sample of size @var{n} from
## the univariate distribution which assumes the values in @var{v} with
## probabilities @var{p}. @var{n} must be a scalar.
##
## If @var{r} and @var{c} are given create a matrix with @var{r} rows and
## @var{c} columns. Or if @var{sz} is a vector, create a matrix of size
## @var{sz}.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Random deviates from a discrete distribution

function rnd = discrete_rnd (v, p, r, c)

  if (nargin == 4)
    if (! (isscalar (r) && (r > 0) && (r == round (r))))
      error ("discrete_rnd: r must be a positive integer");
    endif
    if (! (isscalar (c) && (c > 0) && (c == round (c))))
      error ("discrete_rnd: c must be a positive integer");
    endif
    sz = [r, c];
  elseif (nargin == 3)
    ## A potential problem happens here if all args are scalar, as
    ## we can't distiguish between the command syntax. Thankfully this
    ## case doesn't make much sense. So we assume the first syntax
    ## if the first arg is scalar

    if (isscalar (v))
      sz = [1, floor(v)];
      v = p;
      p = r;
    else
      if (isscalar (r) && (r > 0))
	sz = [r, r];
      elseif (isvector(r) && all (r > 0))
	sz = r(:)';
      else
	error ("discrete_rnd: r must be a postive integer or vector");
      endif
    endif
  else
    print_usage ();
  endif

  if (! isvector (v))
    error ("discrete_rnd: v must be a vector");
  elseif (! isvector (p) || (length (p) != length (v)))
    error ("discrete_rnd: p must be a vector with length (v) elements");
  elseif (! (all (p >= 0) && any (p)))
    error ("discrete_rnd: p must be a nonzero, nonnegative vector");
  endif

  n = prod (sz);
  m = length (v);
  u = rand (1, n);
  s = reshape (cumsum (p / sum (p)), m, 1);

  ## The following loop is a space/time tradeoff in favor of space,
  ## since the dataset may be large.
  ##
  ## Vectorized code is:
  ##
  rnd = v (1 + sum ((s * ones (1, n)) <= ((ones (m, 1) * u))));
  rnd = reshape (rnd, sz);
  ##
  ## Non-vectorized code is:
  ##
  ##  rnd = zeros (sz);
  ##  for q=1:n
  ##    rnd (q) = v (sum (s <= u (q)) + 1);
  ##  endfor

endfunction
