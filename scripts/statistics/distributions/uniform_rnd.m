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
## @deftypefn {Function File} {} uniform_rnd (@var{a}, @var{b}, @var{r}, @var{c})
## Return an @var{r} by @var{c} matrix of random samples from the
## uniform distribution on [@var{a}, @var{b}].  Both @var{a} and @var{b}
## must be scalar or of size @var{r} by @var{c}.
##
## If @var{r} and @var{c} are omitted, the size of the result matrix is
## the common size of @var{a} and @var{b}.
## @end deftypefn

## Author: KH <Kurt.Hornik@ci.tuwien.ac.at>
## Description: Random deviates from the uniform distribution

function rnd = uniform_rnd (a, b, r, c)

  if (nargin == 4)
    if (! (is_scalar (r) && (r > 0) && (r == round (r))))
      error ("uniform_rnd: r must be a positive integer");
    endif
    if (! (is_scalar (c) && (c > 0) && (c == round (c))))
      error ("uniform_rnd: c must be a positive integer");
    endif
    [retval, a, b] = common_size (a, b, zeros (r, c));
    if (retval > 0)
      error ("uniform_rnd: a and b must be scalar or of size %d by %d", r, c);
    endif
  elseif (nargin == 2)
    [retval, a, b] = common_size (a, b);
    if (retval > 0)
      error ("uniform_rnd: a and b must be of common size or scalar");
    endif
  else
    usage ("uniform_rnd (a, b, r, c)");
  endif

  [r, c] = size (a);
  s = r * c;
  a = reshape (a, 1, s);
  b = reshape (b, 1, s);
  rnd = zeros (1, s);

  k = find (!(-Inf < a) | !(a < b) | !(b < Inf));
  if (any (k))
    rnd(k) = NaN * ones (1, length (k));
  endif

  k = find ((-Inf < a) & (a < b) & (b < Inf));
  if (any (k))
    rnd(k) = a(k) + (b(k) - a(k)) .* rand (1, length (k));
  endif

  rnd = reshape (rnd, r, c);

endfunction