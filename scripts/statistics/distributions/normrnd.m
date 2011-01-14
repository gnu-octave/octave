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
## @deftypefn  {Function File} {} normrnd (@var{m}, @var{s}, @var{r}, @var{c})
## @deftypefnx {Function File} {} normrnd (@var{m}, @var{s}, @var{sz})
## Return an @var{r} by @var{c}  or @code{size (@var{sz})} matrix of
## random samples from the normal distribution with parameters mean @var{m} 
## and standard deviation @var{s}.  Both @var{m} and @var{s} must be scalar 
## or of size @var{r} by @var{c}.
##
## If @var{r} and @var{c} are omitted, the size of the result matrix is
## the common size of @var{m} and @var{s}.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Random deviates from the normal distribution

function rnd = normrnd (m, s, r, c)

  if (nargin > 1)
    if (!isscalar (m) || !isscalar (s)) 
      [retval, m, s] = common_size (m, s);
      if (retval > 0)
        error ("normrnd: M and S must be of common size or scalar");
      endif
    endif
  endif

  if (nargin == 4)
    if (! (isscalar (r) && (r > 0) && (r == round (r))))
      error ("normrnd: R must be a positive integer");
    endif
    if (! (isscalar (c) && (c > 0) && (c == round (c))))
      error ("normrnd: C must be a positive integer");
    endif
    sz = [r, c];

    if (any (size (m) != 1)
        && (length (size (m)) != length (sz) || any (size (m) != sz)))
      error ("normrnd: M and S must be scalar or of size [R, C]");
    endif
  elseif (nargin == 3)
    if (isscalar (r) && (r > 0))
      sz = [r, r];
    elseif (isvector(r) && all (r > 0))
      sz = r(:)';
    else
      error ("normrnd: R must be a positive integer or vector");
    endif

    if (any (size (m) != 1)
        && (length (size (m)) != length (sz) || any (size (m) != sz)))
      error ("normrnd: M and S must be scalar or of size SZ");
    endif
  elseif (nargin == 2)
    sz = size(m);
  else
    print_usage ();
  endif

  if (isscalar (m) && isscalar (s))
    if (find (isnan (m) | isinf (m) | !(s > 0) | !(s < Inf)))
      rnd = NaN (sz);
    else
      rnd =  m + s .* randn (sz);
    endif
  else
    rnd = m + s .* randn (sz);
    k = find (isnan (m) | isinf (m) | !(s > 0) | !(s < Inf));
    if (any (k))
      rnd(k) = NaN;
    endif
  endif

endfunction
