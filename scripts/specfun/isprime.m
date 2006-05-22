## Copyright (C) 2000 Paul Kienzle
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
## @deftypefn {Function File} {} isprime (@var{n})
##
## Return true if @var{n} is a prime number, false otherwise.
##
## Something like the following is much faster if you need to test a lot
## of small numbers:
##
## @example
##    @var{t} = ismember (@var{n}, primes (max (@var{n} (:))));
## @end example
##
## If max(n) is very large, then you should be using special purpose 
## factorization code.
##
## @seealso{primes, factor, gcd, lcm}
## @end deftypefn

function t = isprime (n)
  if (! isscalar (n))
    nel = numel (n);
    t = n;
    for i = 1:nel
      t(i) = isprime (t(i));
    endfor
  elseif (n != fix (n) || n < 2)
    t = 0;
  elseif (n < 9)
    t = all (n != [4, 6, 8]);
  else
    q = n./[2, 3:2:sqrt(n)];
    t = all (q != fix (q));
  endif
endfunction
