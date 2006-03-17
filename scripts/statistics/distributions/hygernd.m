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
## @deftypefn {Function File} {} hygernd (@var{m}, @var{t}, @var{n}, @var{r}, @var{c})
## @deftypefnx {Function File} {} hygernd (@var{m}, @var{t}, @var{n}, @var{sz})
## Return an @var{r} by @var{c} matrix of random samples from the
## hypergeometric distribution with parameters @var{m}, @var{t},
## and @var{n}.
##
## The parameters @var{m}, @var{t}, and @var{n} must positive integers
## with @var{m} and @var{n} not greater than @var{t}.
## @end deftypefn

## function rnd = hygernd (N, m, t, n)
function rnd = hygernd (m, t, n, r, c)

  if (nargin == 5)
    if (! (isscalar (r) && (r > 0) && (r == round (r))))
      error ("hygernd: r must be a positive integer");
    endif
    if (! (isscalar (c) && (c > 0) && (c == round (c))))
      error ("hygernd: c must be a positive integer");
    endif
    sz = [r, c];
  elseif (nargin == 4)
    ## A potential problem happens here if all args are scalar, as
    ## we can distiguish between the command syntax. This is quite
    ## ambigous! I assume that if the last arg is a vector then 
    ## then third form is assumed. This means that you can't define
    ## and r-by-r matrix with a single scalar!

    if (isscalar (r))
      sz = [1, floor(m)];
      m = t;
      t = n;
      n = r;
    elseif (isvector(r) && all (r > 0))
      sz = r(:)';
    else
      error ("hygernd: r must be a vector of positive integers");
    endif
  else
    usage ("hygernd (N, m, t, n) | hygernd (m, t, n, r, c)");
  endif

  if (!isscalar (m) || !isscalar (t) || !isscalar (n))
    error ("hygernd: m, t and n must all be positive integers");
  endif

  if ((m < 0) | (t < 0) | (n <= 0) | (m != round (m)) |
      (t != round (t)) | (n != round (n)) | (m > t) | (n > t))
    rnd = NaN * ones (sz)
  else
    rnd = discrete_rnd (0 : n, hygepdf (0 : n, m, t, n), sz);
  endif

endfunction
