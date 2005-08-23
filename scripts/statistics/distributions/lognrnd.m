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
## @deftypefn {Function File} {} lognrnd (@var{a}, @var{v}, @var{r}, @var{c})
## @deftypefnx {Function File} {} lognrnd (@var{a}, @var{v}, @var{sz})
## Return an @var{r} by @var{c} matrix of random samples from the
## lognormal distribution with parameters @var{a} and @var{v}. Both
## @var{a} and @var{v} must be scalar or of size @var{r} by @var{c}.
## Or if @var{sz} is a vector, create a matrix of size @var{sz}.
##
## If @var{r} and @var{c} are omitted, the size of the result matrix is
## the common size of @var{a} and @var{v}.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Random deviates from the log normal distribution

function rnd = lognrnd (a, v, r, c)

  if (nargin > 1)
    if (!isscalar(a) || !isscalar(v)) 
      [retval, a, v] = common_size (a, v);
      if (retval > 0)
	error ("lognrnd: a and v must be of common size or scalar");
      endif
    endif
  endif

  if (nargin == 4)
    if (! (isscalar (r) && (r > 0) && (r == round (r))))
      error ("lognrnd: r must be a positive integer");
    endif
    if (! (isscalar (c) && (c > 0) && (c == round (c))))
      error ("lognrnd: c must be a positive integer");
    endif
    sz = [r, c];

    if (any (size (a) != 1) && 
	((length (size (a)) != length (sz)) || any (size (a) != sz)))
      error ("lognrnd: a and b must be scalar or of size [r, c]");
    endif

  elseif (nargin == 3)
    if (isscalar (r) && (r > 0))
      sz = [r, r];
    elseif (isvector(r) && all (r > 0))
      sz = r(:)';
    else
      error ("lognrnd: r must be a postive integer or vector");
    endif

    if (any (size (a) != 1) && 
	((length (size (a)) != length (sz)) || any (size (a) != sz)))
      error ("lognrnd: a and b must be scalar or of size sz");
    endif
  elseif (nargin == 2)
    sz = size(a);
  else
    usage ("lognrnd (a, v, r, c)");
  endif

  if (isscalar (a) && isscalar (v))
    if  (!(a > 0) | !(a < Inf) | !(v > 0) | !(v < Inf))
      rnd = NaN * ones (sz);
    elseif find ((a > 0) & (a < Inf) & (v > 0) & (v < Inf));
      rnd = a * exp (sqrt (v) .* randn (sz));
    else
      rnd = zeros (sz);
    endif
  else
    rnd = zeros (sz);
    k = find (!(a > 0) | !(a < Inf) | !(v > 0) | !(v < Inf));
    if (any (k))
      rnd(k) = NaN * ones (1, length (k));
    endif

    k = find ((a > 0) & (a < Inf) & (v > 0) & (v < Inf));
    if (any (k))
      rnd(k) = a(k) .* exp (sqrt (v(k)) .* randn (1, length (k)));
    endif
  endif

endfunction
