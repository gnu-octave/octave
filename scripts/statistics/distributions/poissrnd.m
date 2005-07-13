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
    if (! (isscalar (r) && (r > 0) && (r == round (r))))
      error ("poisson_rnd: r must be a positive integer");
    endif
    if (! (isscalar (c) && (c > 0) && (c == round (c))))
      error ("poisson_rnd: c must be a positive integer");
    endif
    sz = [r, c];

    if (any (size (l) != 1) && 
	((length (size (l)) != length (sz)) || any (size (l) != sz)))
      error ("poisson_rnd: lambda must be scalar or of size [r, c]");
    endif
  elseif (nargin == 2)
    if (isscalar (r) && (r > 0))
      sz = [r, r];
    elseif (isvector(r) && all (r > 0))
      sz = r(:)';
    else
      error ("poisson_rnd: r must be a postive integer or vector");
    endif

    if (any (size (l) != 1) && 
	((length (size (l)) != length (sz)) || any (size (l) != sz)))
      error ("poisson_rnd: lambda must be scalar or of size sz");
    endif
  elseif (nargin == 1)
    sz = size (l);
  else
    usage ("poisson_rnd (lambda, r, c)");
  endif

  if (isscalar (l))

    if (!(l >= 0) | !(l < Inf))
      rnd = NaN * ones (sz);
    elseif ((l > 0) & (l < Inf))
      num = zeros (sz);
      sum = - log (1 - rand (sz)) ./ l;
      while (1)
	ind = find (sum < 1);
	if (any (ind))
          sum(ind) = (sum(ind) - log (1 - rand (size (ind))) / l);
          num(ind) = num(ind) + 1;
	else
          break;
	endif
      endwhile
      rnd = num;
    else
      rnd = zeros (sz);
    endif
  else
    rnd = zeros (sz);

    k = find (!(l >= 0) | !(l < Inf));
    if (any (k))
      rnd(k) = NaN;
    endif

    k = find ((l > 0) & (l < Inf));
    if (any (k))
      l = l(k);
      num = zeros (size (k));
      sum = - log (1 - rand (size (k))) ./ l;
      while (1)
	ind = find (sum < 1);
	if (any (ind))
          sum(ind) = (sum(ind)
                      - log (1 - rand (size (ind))) ./ l(ind));
          num(ind) = num(ind) + 1;
	else
          break;
	endif
      endwhile
      rnd(k) = num;
    endif
  endif

endfunction
