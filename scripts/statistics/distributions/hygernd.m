## Copyright (C) 1997, 2005, 2006, 2007, 2009 Kurt Hornik
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
## @deftypefn  {Function File} {} hygernd (@var{t}, @var{m}, @var{n}, @var{r}, @var{c})
## @deftypefnx {Function File} {} hygernd (@var{t}, @var{m}, @var{n}, @var{sz})
## @deftypefnx {Function File} {} hygernd (@var{t}, @var{m}, @var{n})
## Return an @var{r} by @var{c} matrix of random samples from the
## hypergeometric distribution with parameters @var{t}, @var{m},
## and @var{n}.
##
## The parameters @var{t}, @var{m}, and @var{n} must positive integers
## with @var{m} and @var{n} not greater than @var{t}.
##
## The parameter @var{sz} must be scalar or a vector of matrix
## dimensions.  If @var{sz} is scalar, then a @var{sz} by @var{sz}
## matrix of random samples is generated.
## @end deftypefn

function rnd = hygernd (t, m, n, r, c)

  if (nargin == 5)
    if (! (isscalar (r) && (r > 0) && (r == round (r))))
      error ("hygernd: r must be a positive integer");
    endif
    if (! (isscalar (c) && (c > 0) && (c == round (c))))
      error ("hygernd: c must be a positive integer");
    endif
    sz = [r, c];
  elseif (nargin == 4)
    if (isvector (r) && all (r > 0) && all (r == round (r)))
      if (isscalar (r))
        sz = [r, r];
      else
        sz = r(:)';
      endif
    else
      error ("hygernd: r must be a vector of positive integers");
    endif
  elseif (nargin != 3)
    print_usage ();
  endif

  if (! isscalar (t) || ! isscalar (m) || ! isscalar (n))
    [retval, t, m, n] = common_size (t, m, n);
    if (retval > 0)
      error ("hygernd: t, m and n must be of common size or scalar");
    endif
    if (nargin > 3)
      if (any (sz != size (t)))
        error ("hygernd: t, m and n must have the same size as implied by r and c or must be scalar");
      endif
    else
      sz = size (t);
    endif
  elseif (nargin == 3)
    sz = 1;
  endif

  ## NaN elements
  ne = (! (t >= 0) | ! (m >= 0) | ! (n > 0) | ! (t == round (t)) | ! (m == round (m)) | ! (n == round (n)) | ! (m <= t) | ! (n <= t));

  if (! isscalar (t))
    rnd = zeros (sz);
    rnd(ne) = NaN;
    rn = rand (sz);
    for i = find (! ne)
      v = 0 : n(i);
      p = hygepdf (v, t(i), m(i), n(i));
      rnd(i) = v(lookup (cumsum (p(1 : end-1)) / sum (p), rn(i)) + 1);
    endfor
  else
    if (ne)
      rnd = NaN (sz);
    else
      v = 0:n;
      p = hygepdf (v, t, m, n);
      rnd = v(lookup (cumsum (p(1:end-1)) / sum (p), rand (sz)) + 1);
    endif
  endif

endfunction
