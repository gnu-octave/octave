## Copyright (C) 2001, 2006, 2007 Rolf Fabian and Paul Kienzle
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
## @iftex
## @tex
## $$
##  {n \choose k} = {n (n-1) (n-2) \cdots (n-k+1) \over k!}
##                = {n! \over k! (n-k)!}
## $$
## @end tex
## @end iftex
## @ifinfo
##
## @example
## @group
##  /   \
##  | n |    n (n-1) (n-2) ... (n-k+1)       n!
##  |   |  = ------------------------- =  ---------
##  | k |               k!                k! (n-k)!
##  \   /
## @end group
## @end example
## @end ifinfo
##
## If @var{n} is a vector generate all combinations of the elements
## of @var{n}, taken @var{k} at a time, one row per combination. The 
## resulting @var{c} has size @code{[nchoosek (length (@var{n}), 
## @var{k}), @var{k}]}.
##
## @seealso{bincoeff}
## @end deftypefn

## Author: Rolf Fabian  <fabian@tu-cottbus.de>
## Author: Paul Kienzle <pkienzle@users.sf.net>

## FIXME -- This function is identical to bincoeff for scalar
## values, and so should probably be combined with bincoeff.

function A = nchoosek (v, k)

  if (nargin != 2)
    print_usage ();
  endif

  n = length (v);

  if (n == 1)
     A = round (exp (sum (log (k+1:v)) - sum (log (2:v-k))));
  elseif (k == 0)
    A = [];
  elseif (k == 1)
    A = v(:);
  elseif (k == n)
     A = v(:).';
  else
    oldmax = max_recursion_depth ();
    unwind_protect
      max_recursion_depth (n);
      A = nck (v, k);
    unwind_protect_cleanup
      max_recursion_depth (oldmax);
    end_unwind_protect
  endif
endfunction

function A = nck (v, k)
  n = length (v);
  if (n == 1 || k < 2 || k == n)
    A = nchoosek (v, k);
  else
    m = nchoosek (n-1, k-1);
    A = [v(1)*ones(m,1), nck(v(2:n),k-1);
	 nck(v(2:n), k)];
  endif
endfunction

%!assert (nchoosek(100,45), bincoeff(100,45))
%!assert (nchoosek(1:5,3),[1:3;1,2,4;1,2,5;1,3,4;1,3,5;1,4,5;2:4;2,3,5;2,4,5;3:5])
