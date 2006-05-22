## Copyright (C) 2001 Rolf Fabian and Paul Kienzle
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
## @deftypefn {Function File} {@var{c} =} nchoosek (@var{n}, @var{k})
##
## Compute the binomial coeeficient or all combinations of @var{n}.
## If @var{n} is a scalar then, calculate the binomial coefficient
## of @var{n} and @var{k}, defined as
##
## @iftex
## @tex
## $$
##  {n \choose k} = {n (n-1) (n-2) \cdots (n-k+1) \over k!}
## $$
## @end tex
## @end iftex
## @ifinfo
##
## @example
## @group
##  /   \
##  | n |    n (n-1) (n-2) ... (n-k+1)
##  |   |  = -------------------------
##  | k |               k!
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
## @end deftypefn

##AUTHORS Rolf Fabian  <fabian@tu-cottbus.de>
##        Paul Kienzle <pkienzle@users.sf.net>

## XXX FIXME XXX This function is identical to bincoeff for scalar
## values, and so should probably be combined with bincoeff.

function A = nchoosek (v, k)

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
    m = round (exp (sum (log (k:n-1)) - sum (log (2:n-k))));
    A = [v(1)*ones(m,1), nchoosek(v(2:n),k-1);
	 nchoosek(v(2:n),k)];
  endif

endfunction
