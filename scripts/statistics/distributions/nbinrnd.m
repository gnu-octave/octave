## Copyright (C) 1995, 1996, 1997, 2007, 2009 Kurt Hornik
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or (at
## your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn  {Function File} {} nbinrnd (@var{n}, @var{p}, @var{r}, @var{c})
## @deftypefnx {Function File} {} nbinrnd (@var{n}, @var{p}, @var{sz})
## Return an @var{r} by @var{c} matrix of random samples from the Pascal
## (negative binomial) distribution with parameters @var{n} and @var{p}.
## Both @var{n} and @var{p} must be scalar or of size @var{r} by @var{c}.
##
## If @var{r} and @var{c} are omitted, the size of the result matrix is
## the common size of @var{n} and @var{p}.  Or if @var{sz} is a vector, 
## create a matrix of size @var{sz}.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Random deviates from the Pascal distribution

function rnd = nbinrnd (n, p, r, c)

  if (nargin > 1)
    if (!isscalar(n) || !isscalar(p)) 
      [retval, n, p] = common_size (n, p);
      if (retval > 0)
        error ("nbinrnd: N and P must be of common size or scalar");
      endif
    endif
  endif

  if (nargin == 4)
    if (! (isscalar (r) && (r > 0) && (r == round (r))))
      error ("nbinrnd: R must be a positive integer");
    endif
    if (! (isscalar (c) && (c > 0) && (c == round (c))))
      error ("nbinrnd: C must be a positive integer");
    endif
    sz = [r, c];

    if (any (size (n) != 1)
        && ((length (size (n)) != length (sz)) || any (size (n) != sz)))
      error ("nbinrnd: N and P must be scalar or of size [R, C]");
    endif

  elseif (nargin == 3)
    if (isscalar (r) && (r > 0))
      sz = [r, r];
    elseif (isvector(r) && all (r > 0))
      sz = r(:)';
    else
      error ("nbinrnd: R must be a positive integer or vector");
    endif

    if (any (size (n) != 1)
        && ((length (size (n)) != length (sz)) || any (size (n) != sz)))
      error ("nbinrnd: N and P must be scalar or of size SZ");
    endif
  elseif (nargin == 2)
    sz = size(n);
  else
    print_usage ();
  endif

  if (isscalar (n) && isscalar (p))
    if ((n < 1) || (n == Inf) || (n != round (n)) || (p <= 0) || (p > 1));
      rnd = NaN (sz);
    elseif ((n > 0) && (n < Inf) && (n == round (n))
            && (p > 0) && (p <= 1))
      rnd = randp ((1 - p) ./ p .* randg (n, sz));
    else
      rnd = zeros (sz);
    endif
  else
    rnd = zeros (sz);

    k = find ((n < 1) | (n == Inf) | (n != round (n)) | (p <= 0) | (p > 1));
    if (any (k))
      rnd(k) = NaN;
    endif

    k = find ((n > 0) & (n < Inf) & (n == round (n)) & (p > 0) & (p <= 1));
    if (any (k))
      rnd(k) = randp ((1 - p(k)) ./ p(k) .* randg (n(k), size(k)));
    endif
  endif

endfunction
