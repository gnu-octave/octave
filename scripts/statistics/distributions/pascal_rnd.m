## Copyright (C) 1995, 1996, 1997  Kurt Hornik
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
## Software Foundation, 59 Temple Place - Suite 330, Boston, MA
## 02111-1307, USA.

## -*- texinfo -*-
## @deftypefn {Function File} {} pascal_rnd (@var{n}, @var{p}, @var{r}, @var{c})
## Return an @var{r} by @var{c} matrix of random samples from the Pascal
## (negative binomial) distribution with parameters @var{n} and @var{p}.
## Both @var{n} and @var{p} must be scalar or of size @var{r} by @var{c}. 
##
## If @var{r} and @var{c} are omitted, the size of the result matrix is
## the common size of @var{n} and @var{p}.
## @end deftypefn

## Author: KH <Kurt.Hornik@ci.tuwien.ac.at>
## Description: Random deviates from the Pascal distribution

function rnd = pascal_rnd (n, p, r, c)

  if (nargin == 4)
    if (! (is_scalar (r) && (r > 0) && (r == round (r))))
      error ("pascal_rnd: r must be a positive integer");
    endif
    if (! (is_scalar (c) && (c > 0) && (c == round (c))))
      error ("pascal_rnd: c must be a positive integer");
    endif
    [retval, n, p] = common_size (n, p, zeros (r, c));
    if (retval > 0)
      error ("pascal_rnd: n and p must be scalar or of size %d by %d", r, c);
    endif
  elseif (nargin == 2)
    [retval, n, p] = common_size (n, p);
    if (retval > 0)
      error ("pascal_rnd: n and p must be of common size or scalar");
    endif
  else
    usage ("pascal_rnd (n, p, r, c)");
  endif

  [r, c] = size (n);
  s = r * c;
  n = reshape (n, 1, s);
  p = reshape (p, 1, s);
  rnd = zeros (1, s);

  k = find (!(n > 0) | !(n < Inf) | !(n == round (n)) | !(p <= 0) | !(p >= 1));
  if (any (k))
    rnd(k) = NaN * ones (1, length (k));
  endif

  k = find ((n > 0) & (n < Inf) & (n == round (n)) & (p >= 0) & (p <= 1));
  if (any (k))
    N = max (n(k));
    L = length (k);
    tmp = floor (log (rand (N, L)) ./ (ones (N, 1) * log (1 - p(k))));
    ind = (1 : N)' * ones (1, L);
    rnd(k) = sum (tmp .* (ind <= ones (N, 1) * n(k)));
  endif

  rnd = reshape (rnd, r, c);

endfunction