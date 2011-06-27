## Copyright (C) 1995-2011 Kurt Hornik
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
## @deftypefn  {Function File} {} geornd (@var{p}, @var{r}, @var{c})
## @deftypefnx {Function File} {} geornd (@var{p}, @var{sz})
## Return an @var{r} by @var{c} matrix of random samples from the
## geometric distribution with parameter @var{p}, which must be a scalar
## or of size @var{r} by @var{c}.
##
## If @var{r} and @var{c} are given create a matrix with @var{r} rows and
## @var{c} columns.  Or if @var{sz} is a vector, create a matrix of size
## @var{sz}.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Random deviates from the geometric distribution

function rnd = geornd (p, r, c)

  if (nargin == 3)
    if (! (isscalar (r) && (r > 0) && (r == round (r))))
      error ("geornd: R must be a positive integer");
    endif
    if (! (isscalar (c) && (c > 0) && (c == round (c))))
      error ("geornd: C must be a positive integer");
    endif
    sz = [r, c];

    if (any (size (p) != 1)
        && ((length (size (p)) != length (sz)) || any (size (p) != sz)))
      error ("geornd: P must be scalar or of size [R, C]");
    endif
  elseif (nargin == 2)
    if (isscalar (r) && (r > 0))
      sz = [r, r];
    elseif (isvector(r) && all (r > 0))
      sz = r(:)';
    else
      error ("geornd: R must be a positive integer or vector");
    endif

    if (any (size (p) != 1)
        && ((length (size (p)) != length (sz)) || any (size (p) != sz)))
      error ("geornd: n must be scalar or of size SZ");
    endif
  elseif (nargin == 1)
    sz = size(p);
  elseif (nargin != 1)
    print_usage ();
  endif


  if (isscalar (p))
    if (p < 0 || p > 1)
      rnd = NaN (sz);
    elseif (p == 0)
      rnd = Inf (sz);
    elseif (p > 0 && p < 1);
      rnd = floor (- rande(sz) ./ log (1 - p));
    else
      rnd = zeros (sz);
    endif
  else
    rnd = floor (- rande(sz) ./ log (1 - p));

    k = find (!(p >= 0) | !(p <= 1));
    if (any (k))
      rnd(k) = NaN (1, length (k));
    endif

    k = find (p == 0);
    if (any (k))
      rnd(k) = Inf (1, length (k));
    endif
  endif

endfunction
