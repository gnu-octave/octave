## Copyright (C) 1995, 1996, 1997, 2005, 2006, 2007, 2009 Kurt Hornik
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
## @deftypefn  {Function File} {} trnd (@var{n}, @var{r}, @var{c})
## @deftypefnx {Function File} {} trnd (@var{n}, @var{sz})
## Return an @var{r} by @var{c} matrix of random samples from the t
## (Student) distribution with @var{n} degrees of freedom.  @var{n} must
## be a scalar or of size @var{r} by @var{c}.  Or if @var{sz} is a
## vector create a matrix of size @var{sz}.
##
## If @var{r} and @var{c} are omitted, the size of the result matrix is
## the size of @var{n}.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Random deviates from the t distribution

function rnd = trnd (n, r, c)

  if (nargin == 3)
    if (! (isscalar (r) && (r > 0) && (r == round (r))))
      error ("trnd: r must be a positive integer");
    endif
    if (! (isscalar (c) && (c > 0) && (c == round (c))))
      error ("trnd: c must be a positive integer");
    endif
    sz = [r, c];

    if (any (size (n) != 1)
        && ((length (size (n)) != length (sz)) || any (size (n) != sz)))
      error ("trnd: n must be scalar or of size sz");
    endif
  elseif (nargin == 2)
    if (isscalar (r) && (r > 0))
      sz = [r, r];
    elseif (isvector(r) && all (r > 0))
      sz = r(:)';
    else
      error ("trnd: r must be a positive integer or vector");
    endif

    if (any (size (n) != 1)
        && ((length (size (n)) != length (sz)) || any (size (n) != sz)))
      error ("trnd: n must be scalar or of size sz");
    endif
  elseif (nargin == 1)
    sz = size (n);
  else
    print_usage ();
  endif

  if (isscalar (n))
    if (!(n > 0) || !(n < Inf))
      rnd = NaN (sz);
    elseif ((n > 0) && (n < Inf))
      rnd = randn(sz) ./ sqrt(2*randg(n/2,sz)./n); 
    else
      rnd = zeros (size (n));
    endif
  else
    rnd = zeros (size (n));

    k = find (!(n > 0) | !(n < Inf));
    if (any (k))
      rnd(k) = NaN;
    endif

    k = find ((n > 0) & (n < Inf));
    if (any (k))
      rnd(k) = randn(size(k)) ./ sqrt(2*randg(n(k)/2,size(k))./n(k)); 
    endif
  endif

endfunction
