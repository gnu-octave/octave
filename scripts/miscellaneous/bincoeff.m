## Copyright (C) 1995, 1996, 1999, 2000, 2002, 2004, 2005, 2006, 2007,
##               2008, 2009 Kurt Hornik
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
## @deftypefn {Mapping Function} {} bincoeff (@var{n}, @var{k})
## Return the binomial coefficient of @var{n} and @var{k}, defined as
## @tex
## $$
##  {n \choose k} = {n (n-1) (n-2) \cdots (n-k+1) \over k!}
## $$
## @end tex
## @ifnottex
##
## @example
## @group
##  /   \
##  | n |    n (n-1) (n-2) @dots{} (n-k+1)
##  |   |  = -------------------------
##  | k |               k!
##  \   /
## @end group
## @end example
## @end ifnottex
##
## For example,
##
## @example
## @group
## bincoeff (5, 2)
##      @result{} 10
## @end group
## @end example
##
## In most cases, the @code{nchoosek} function is faster for small
## scalar integer arguments.  It also warns about loss of precision for
## big arguments.
##
## @seealso{nchoosek}
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Created: 8 October 1994
## Adapted-By: jwe

function b = bincoeff (n, k)

  if (nargin != 2)
    print_usage ();
  endif

  [retval, n, k] = common_size (n, k);
  if (retval > 0)
    error ("bincoeff: n and k must be of common size or scalars");
  endif

  sz = size (n);
  b   = zeros (sz);

  ind = (! (k >= 0) | (k != real (round (k))) | isnan (n));
  b(ind) = NaN;
  
  ind = (k == 0);
  b(ind) = 1;

  ind = ((k > 0) & ((n == real (round (n))) & (n < 0)));
  b(ind) = (-1) .^ k(ind) .* exp (gammaln (abs (n(ind)) + k(ind))
                                  - gammaln (k(ind) + 1)
                                  - gammaln (abs (n(ind))));

  ind = ((k > 0) & (n >= k));
  b(ind) = exp (gammaln (n(ind) + 1)
                - gammaln (k(ind) + 1)
                - gammaln (n(ind) - k(ind) + 1));

  ind = ((k > 0) & ((n != real (round (n))) & (n < k)));
  b(ind) = (1/pi) * exp (gammaln (n(ind) + 1)
                         - gammaln (k(ind) + 1)
                         + gammaln (k(ind) - n(ind))
                         + log (sin (pi * (n(ind) - k(ind) + 1))));

  ## Clean up rounding errors.
  ind = (n == round (n));
  b(ind) = round (b(ind));

  ind = (n != round (n));
  b(ind) = real (b(ind));

endfunction

%!assert(bincoeff(4,2), 6)
%!assert(bincoeff(2,4), 0)
%!assert(bincoeff(0.4,2), -.12, 8*eps)

%!assert(bincoeff (5, 2) == 10 && bincoeff (50, 6) == 15890700);

%!error bincoeff ();

%!error bincoeff (1, 2, 3);
