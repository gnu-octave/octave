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
## @deftypefn  {Function File} {} lognrnd (@var{mu}, @var{sigma}, @var{r}, @var{c})
## @deftypefnx {Function File} {} lognrnd (@var{mu}, @var{sigma}, @var{sz})
## Return an @var{r} by @var{c} matrix of random samples from the
## lognormal distribution with parameters @var{mu} and @var{sigma}.  Both
## @var{mu} and @var{sigma} must be scalar or of size @var{r} by @var{c}.
## Or if @var{sz} is a vector, create a matrix of size @var{sz}.
##
## If @var{r} and @var{c} are omitted, the size of the result matrix is
## the common size of @var{mu} and @var{sigma}.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Random deviates from the log normal distribution

function rnd = lognrnd (mu, sigma, r, c)

  if (nargin > 1)
    if (!isscalar(mu) || !isscalar(sigma)) 
      [retval, mu, sigma] = common_size (mu, sigma);
      if (retval > 0)
        error ("lognrnd: MU and SIGMA must be of common size or scalar");
      endif
    endif
  endif

  if (nargin == 4)
    if (! (isscalar (r) && (r > 0) && (r == round (r))))
      error ("lognrnd: R must be a positive integer");
    endif
    if (! (isscalar (c) && (c > 0) && (c == round (c))))
      error ("lognrnd: C must be a positive integer");
    endif
    sz = [r, c];

    if (any (size (mu) != 1)
        && ((length (size (mu)) != length (sz)) || any (size (mu) != sz)))
      error ("lognrnd: MU and SIGMA must be scalar or of size [R, C]");
    endif

  elseif (nargin == 3)
    if (isscalar (r) && (r > 0))
      sz = [r, r];
    elseif (isvector(r) && all (r > 0))
      sz = r(:)';
    else
      error ("lognrnd: R must be a positive integer or vector");
    endif

    if (any (size (mu) != 1)
        && ((length (size (mu)) != length (sz)) || any (size (mu) != sz)))
      error ("lognrnd: MU and SIGMA must be scalar or of size SZ");
    endif
  elseif (nargin == 2)
    sz = size(mu);
  else
    print_usage ();
  endif

  if (isscalar (mu) && isscalar (sigma))
    if  (!(sigma > 0) || !(sigma < Inf))
      rnd = NaN (sz);
    else
      rnd = exp(mu + sigma .* randn (sz)); 
    endif
  else
    rnd = exp (mu + sigma .* randn (sz));
    k = find ((sigma < 0) | (sigma == Inf));
    if (any (k))
      rnd(k) = NaN;
    endif
  endif

endfunction
