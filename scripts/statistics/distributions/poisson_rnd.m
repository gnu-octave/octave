## Copyright (C) 1995, 1996, 1997  Kurt Hornik
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
## @deftypefn {Function File} {} poisson_rnd (@var{lambda}, @var{r}, @var{c})
## Return an @var{r} by @var{c} matrix of random samples from the
## Poisson distribution with parameter @var{lambda}, which must be a 
## scalar or of size @var{r} by @var{c}.
##
## If @var{r} and @var{c} are omitted, the size of the result matrix is
## the size of @var{lambda}.
## @end deftypefn

## Author: KH <Kurt.Hornik@ci.tuwien.ac.at>
## Description: Random deviates from the Poisson distribution

function rnd = poisson_rnd (l, r, c)

  if (nargin == 3)
    if (! (is_scalar (r) && (r > 0) && (r == round (r))) )
      error ("poisson_rnd: r must be a positive integer");
    endif
    if (! (is_scalar (c) && (c > 0) && (c == round (c))) )
      error ("poisson_rnd: c must be a positive integer");
    endif
    [retval, l] = common_size (l, zeros (r, c));
    if (retval > 0)
      error ("poisson_rnd: lambda must be scalar or of size %d by %d", r, c);
    endif
  elseif (nargin != 1)
    usage ("poisson_rnd (lambda, r, c)");
  endif

  [r, c] = size (l);
  s = r * c;
  l = reshape (l, 1, s);
  rnd = zeros (1, s);

  k = find (!(l > 0) | !(l < Inf));
  if (any (k))
    rnd(k) = NaN * ones (1, length (k));
  endif

  k = find ((l > 0) & (l < Inf));
  if (any (k))
    l = l(k);
    len = length (k);
    num = zeros (1, len);
    sum = - log (1 - rand (1, len)) ./ l;
    while (1)
      ind = find (sum < 1);
      if (any (ind))
        sum(ind) = (sum(ind)
                    - log (1 - rand (1, length (ind))) ./ l(ind));
        num(ind) = num(ind) + 1;
      else
        break;
      endif
    endwhile
    rnd(k) = num;
  endif

  rnd = reshape (rnd, r, c);

endfunction
