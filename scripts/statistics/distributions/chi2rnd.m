## Copyright (C) 1995, 1996, 1997, 2005, 2006, 2007, 2008 Kurt Hornik
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
## @deftypefn  {Function File} {} chi2rnd (@var{n}, @var{r}, @var{c})
## @deftypefnx {Function File} {} chi2rnd (@var{n}, @var{sz})
## Return an @var{r} by @var{c}  or a @code{size (@var{sz})} matrix of 
## random samples from the chisquare distribution with @var{n} degrees 
## of freedom.  @var{n} must be a scalar or of size @var{r} by @var{c}.
##
## If @var{r} and @var{c} are omitted, the size of the result matrix is
## the size of @var{n}.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Random deviates from the chi-square distribution

function rnd = chi2rnd (n, r, c)

  if (nargin == 3)
    if (! (isscalar (r) && (r > 0) && (r == round (r))))
      error ("chi2rnd: r must be a positive integer");
    endif
    if (! (isscalar (c) && (c > 0) && (c == round (c))))
      error ("chi2rnd: c must be a positive integer");
    endif
    sz = [r, c];

    if (any (size (n) != 1)
        && (length (size (n)) != length (sz) || any (size (n) != sz)))
      error ("chi2rnd: n must be scalar or of size [r, c]");
    endif
  elseif (nargin == 2)
    if (isscalar (r) && (r > 0))
      sz = [r, r];
    elseif (isvector(r) && all (r > 0))
      sz = r(:)';
    else
      error ("chi2rnd: r must be a positive integer or vector");
    endif

    if (any (size (n) != 1)
        && (length (size (n)) != length (sz) || any (size (n) != sz)))
      error ("chi2rnd: n must be scalar or of size sz");
    endif
  elseif (nargin == 1)
    sz = size(n);
  else
    print_usage ();
  endif

  if (isscalar (n))
     if (find (!(n > 0) | !(n < Inf)))
       rnd = NaN (sz);
     else
       rnd = 2 * randg(n/2, sz);
     endif
  else
    [retval, n, dummy] = common_size (n, ones (sz));
    if (retval > 0)
      error ("chi2rnd: a and b must be of common size or scalar");
    endif

    rnd = zeros (sz);
    k = find (!(n > 0) | !(n < Inf));
    if (any (k))
      rnd(k) = NaN;
    endif

    k = find ((n > 0) & (n < Inf));
    if (any (k))
      rnd(k) = 2 * randg(n(k)/2, size(k));
    endif
  endif

endfunction
