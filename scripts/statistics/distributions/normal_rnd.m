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
## @deftypefn {Function File} {} normal_rnd (@var{m}, @var{v}, @var{r}, @var{c})
## @deftypefnx {Function File} {} normal_rnd (@var{m}, @var{v}, @var{sz})
## Return an @var{r} by @var{c}  or @code{size (@var{sz})} matrix of
## random samples from the normal distribution with parameters @var{m} 
## and @var{v}.  Both @var{m} and @var{v} must be scalar or of size 
## @var{r} by @var{c}.
##
## If @var{r} and @var{c} are omitted, the size of the result matrix is
## the common size of @var{m} and @var{v}.
## @end deftypefn

## Author: KH <Kurt.Hornik@ci.tuwien.ac.at>
## Description: Random deviates from the normal distribution

function rnd = normal_rnd (m, v, r, c)

  if (nargin > 1)
    if (!isscalar(m) || !isscalar(v)) 
      [retval, m, v] = common_size (m, v);
      if (retval > 0)
	error ("normal_rnd: m and v must be of common size or scalar");
      endif
    endif
  endif

  if (nargin == 4)
    if (! (isscalar (r) && (r > 0) && (r == round (r))))
      error ("normal_rnd: r must be a positive integer");
    endif
    if (! (isscalar (c) && (c > 0) && (c == round (c))))
      error ("normal_rnd: c must be a positive integer");
    endif
    sz = [r, c];

    if (any (size (m) != 1)
	&& (length (size (m)) != length (sz) || any (size (m) != sz)))
      error ("normal_rnd: m and v must be scalar or of size [r, c]");
    endif
  elseif (nargin == 3)
    if (isscalar (r) && (r > 0))
      sz = [r, r];
    elseif (isvector(r) && all (r > 0))
      sz = r(:)';
    else
      error ("normal_rnd: r must be a postive integer or vector");
    endif

    if (any (size (m) != 1)
	&& (length (size (m)) != length (sz) || any (size (m) != sz)))
      error ("normal_rnd: m and v must be scalar or of size sz");
    endif
  elseif (nargin == 2)
    sz = size(m);
  else
    usage ("normal_rnd (m, v, r, c)");
  endif

  if (isscalar (m) && isscalar (v))
    if (find (isnan (m) | isinf (m) | !(v > 0) | !(v < Inf)))
      rnd = NaN * ones (sz);
    else
      rnd =  m + sqrt (v) .* randn (sz);
    endif
  else
    rnd = m + sqrt (v) .* randn (sz);
    k = find (isnan (m) | isinf (m) | !(v > 0) | !(v < Inf));
    if (any (k))
      rnd(k) = NaN;
    endif
  endif

endfunction
