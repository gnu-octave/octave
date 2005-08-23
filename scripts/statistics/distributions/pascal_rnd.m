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
## Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301, USA.

## -*- texinfo -*-
## @deftypefn {Function File} {} pascal_rnd (@var{n}, @var{p}, @var{r}, @var{c})
## @deftypefnx {Function File} {} pascal_rnd (@var{n}, @var{p}, @var{sz})
## Return an @var{r} by @var{c} matrix of random samples from the Pascal
## (negative binomial) distribution with parameters @var{n} and @var{p}.
## Both @var{n} and @var{p} must be scalar or of size @var{r} by @var{c}.
##
## If @var{r} and @var{c} are omitted, the size of the result matrix is
## the common size of @var{n} and @var{p}. Or if @var{sz} is a vector, 
## create a matrix of size @var{sz}.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Random deviates from the Pascal distribution

function rnd = pascal_rnd (n, p, r, c)

  if (nargin > 1)
    if (!isscalar(n) || !isscalar(p)) 
      [retval, n, p] = common_size (n, p);
      if (retval > 0)
	error ("pascal_rnd: n and p must be of common size or scalar");
      endif
    endif
  endif

  if (nargin == 4)
    if (! (isscalar (r) && (r > 0) && (r == round (r))))
      error ("pascal_rnd: r must be a positive integer");
    endif
    if (! (isscalar (c) && (c > 0) && (c == round (c))))
      error ("pascal_rnd: c must be a positive integer");
    endif
    sz = [r, c];

    if (any (size (n) != 1) && 
	((length (size (n)) != length (sz)) || any (size (n) != sz)))
      error ("pascal_rnd: n and p must be scalar or of size [r, c]");
    endif

  elseif (nargin == 3)
    if (isscalar (r) && (r > 0))
      sz = [r, r];
    elseif (isvector(r) && all (r > 0))
      sz = r(:)';
    else
      error ("pascal_rnd: r must be a postive integer or vector");
    endif

    if (any (size (n) != 1) && 
	((length (size (n)) != length (sz)) || any (size (n) != sz)))
      error ("pascal_rnd: n and p must be scalar or of size sz");
    endif
  elseif (nargin == 2)
    sz = size(n);
  else
    usage ("pascal_rnd (n, p, r, c)");
  endif

  if (isscalar (n) && isscalar (p))
    if ((n < 1) || (n == Inf) || (n != round (n)) || (p <= 0) || (p > 1));
      rnd = NaN * ones (sz)
    elseif ((n > 0) && (n < Inf) && (n == round (n)) && 
	    (p > 0) && (p <= 1))
      L = prod (sz);
      tmp = floor (log (rand (n, L)) / log (1 - p));
      if (n == 1)
	rnd = tmp;
      else
	ind = (1 : n)' * ones (1, L);
	rnd = sum (tmp .* (ind <= n));
      endif
    else
      rnd = zeros (sz);
    endif
  else
    rnd = zeros (sz);

    k = find ((n < 1) || (n == Inf) || (n != round (n)) || 
	      (p <= 0) || (p > 1));
    if (any (k))
      rnd(k) = NaN;
    endif

    k = find ((n > 0) & (n < Inf) & (n == round (n)) & (p > 0) & (p <= 1));
    if (any (k))
      n = reshape (n, 1, prod (sz));
      p = reshape (p, 1, prod (sz));
      N = max (n(k));
      L = length (k);
      tmp = floor (log (rand (N, L)) ./ (ones (N, 1) * log (1 - p(k))));
      if (N == 1)
	rnd(k) = tmp;
      else
	ind = (1 : N)' * ones (1, L);
	rnd(k) = sum (tmp .* (ind <= ones (N, 1) * n(k)));
      endif
    endif
  endif

endfunction
