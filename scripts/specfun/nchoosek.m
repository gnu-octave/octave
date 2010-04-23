## Copyright (C) 2001, 2006, 2007, 2009 Rolf Fabian and Paul Kienzle
## Copyright (C) 2008 Jaroslav Hajek
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
## @deftypefn {Function File} {@var{c} =} nchoosek (@var{n}, @var{k})
##
## Compute the binomial coefficient or all combinations of @var{n}.
## If @var{n} is a scalar then, calculate the binomial coefficient
## of @var{n} and @var{k}, defined as
##
## @tex
## $$
##  {n \choose k} = {n (n-1) (n-2) \cdots (n-k+1) \over k!}
##                = {n! \over k! (n-k)!}
## $$
## @end tex
## @ifnottex
##
## @example
## @group
##  /   \
##  | n |    n (n-1) (n-2) @dots{} (n-k+1)       n!
##  |   |  = ------------------------- =  ---------
##  | k |               k!                k! (n-k)!
##  \   /
## @end group
## @end example
## @end ifnottex
##
## If @var{n} is a vector generate all combinations of the elements
## of @var{n}, taken @var{k} at a time, one row per combination.  The 
## resulting @var{c} has size @code{[nchoosek (length (@var{n}), 
## @var{k}), @var{k}]}.
##
## @code{nchoosek} works only for non-negative integer arguments; use
## @code{bincoeff} for non-integer scalar arguments and for using vector
## arguments to compute many coefficients at once.
##
## @seealso{bincoeff}
## @end deftypefn

## Author: Rolf Fabian  <fabian@tu-cottbus.de>
## Author: Paul Kienzle <pkienzle@users.sf.net>
## Author: Jaroslav Hajek

function A = nchoosek (v, k)

  if (nargin != 2
      || !isnumeric(k) || !isnumeric(v)
      || !isscalar(k) || (!isscalar(v) && !isvector(v)))
    print_usage ();
  endif
  if ((isscalar(v) && v < k) || k < 0
      || k != round(k) || any (v < 0 || v != round(v)))
    error ("nchoosek: args are nonnegative integers with V not less than K");
  endif

  n = length (v);

  if (n == 1)
    ## Improve precision at next step.
    k = min (k, v-k);
    A = round (prod ((v-k+1:v)./(1:k)));
    if (A*2*k*eps >= 0.5)
      warning ("nchoosek", "nchoosek: possible loss of precision");
    endif
  elseif (k == 0)
    A = [];
  elseif (k == 1)
    A = v(:);
  elseif (k == n)
    A = v(:).';
  elseif (k > n)
    A = zeros (0, k, class (v));
  else
    p = cell (1, k);
    ## Hack: do the op in the smallest integer class possible to avoid
    ## moving too much data.
    if (n < intmax ("uint8"))
      cl = "uint8";
    elseif (n < intmax ("uint16"))
      cl = "uint16";
    elseif (n < intmax ("uint32"))
      cl = "uint32";
    else
      ## This would exhaust memory anyway.
      cl = "double";
    endif
     
    ## Use a generalized Pascal triangle. Traverse backwards to keep
    ## alphabetical order.
    for i = 1:k
      p{i} = zeros (0, i, cl);
    endfor
    s = ones (1, 1, cl);
    p{1} = n*s;
    for j = n-1:-1:1
      for i = k:-1:2
        q = p{i-1};
        p{i} = [[repmat(s*j, rows (p{i-1}), 1), p{i-1}]; p{i}];
      endfor
      p{1} = [j;p{1}];
    endfor
    v = v(:);
    A = v(p{k});
  endif
endfunction

%!warning (nchoosek(100,45));
%!error (nchoosek(100,45.5));
%!error (nchoosek(100,145));
%!assert (nchoosek(80,10), bincoeff(80,10))
%!assert (nchoosek(1:5,3),[1:3;1,2,4;1,2,5;1,3,4;1,3,5;1,4,5;2:4;2,3,5;2,4,5;3:5])
