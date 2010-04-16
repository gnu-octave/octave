## Copyright (C) 1995, 1996, 1997, 2005, 2006, 2007 Kurt Hornik
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
## @deftypefn {Function File} {} gamrnd (@var{a}, @var{b}, @var{r}, @var{c})
## @deftypefnx {Function File} {} gamrnd (@var{a}, @var{b}, @var{sz})
## Return an @var{r} by @var{c} or a @code{size (@var{sz})} matrix of 
## random samples from the Gamma distribution with parameters @var{a}
## and @var{b}.  Both @var{a} and @var{b} must be scalar or of size 
## @var{r} by @var{c}.
##
## If @var{r} and @var{c} are omitted, the size of the result matrix is
## the common size of @var{a} and @var{b}.
## @seealso{gamma, gammaln, gammainc, gampdf, gamcdf, gaminv}
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Random deviates from the Gamma distribution

function rnd = gamrnd (a, b, r, c)

  if (nargin > 1)
    if (!isscalar(a) || !isscalar(b)) 
      [retval, a, b] = common_size (a, b);
      if (retval > 0)
	error ("gamrnd: a and b must be of common size or scalar");
      endif
    endif
  endif

  if (nargin == 4)
    if (! (isscalar (r) && (r > 0) && (r == round (r))))
      error ("gamrnd: r must be a positive integer");
    endif
    if (! (isscalar (c) && (c > 0) && (c == round (c))))
      error ("gamrnd: c must be a positive integer");
    endif
    sz = [r, c];

    if (any (size (a) != 1)
	&& (length (size (a)) != length (sz) || any (size (a) != sz)))
      error ("gamrnd: a and b must be scalar or of size [r, c]");
    endif
  elseif (nargin == 3)
    if (isscalar (r) && (r > 0))
      sz = [r, r];
    elseif (isvector(r) && all (r > 0))
      sz = r(:)';
    else
      error ("gamrnd: r must be a positive integer or vector");
    endif

    if (any (size (a) != 1)
	&& (length (size (a)) != length (sz) || any (size (a) != sz)))
      error ("gamrnd: a and b must be scalar or of size sz");
    endif
  elseif (nargin == 2)
    sz = size(a);
  else
    print_usage ();
  endif

  rnd = zeros (sz);

  if (isscalar (a) && isscalar(b))
    if (find (!(a > 0) | !(a < Inf) | !(b > 0) | !(b < Inf)))
      rnd = NaN (sz);
    else
      rnd = b .* randg(a, sz);
    endif
  else 
    k = find (!(a > 0) | !(a < Inf) | !(b > 0) | !(b < Inf));
    if (any (k))
      rnd(k) = NaN;
    endif
    k = find ((a > 0) & (a < Inf) & (b > 0) & (b < Inf));
    if (any (k))
      rnd(k) = b(k) .* randg(a(k), size(k));
    endif
  endif

endfunction
