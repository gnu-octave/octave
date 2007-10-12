## Copyright (C) 1997 Kai P. Mueller
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
## @deftypefn {Function File} {} ctrb (@var{sys}, @var{b})
## @deftypefnx {Function File} {} ctrb (@var{a}, @var{b})
## Build controllability matrix:
## @iftex
## @tex
## $$ Q_s = [ B AB A^2B \ldots A^{n-1}B ] $$
## @end tex
## @end iftex
## @ifinfo
## @example
##              2       n-1
## Qs = [ B AB A B ... A   B ]
## @end example
## @end ifinfo
##
## of a system data structure or the pair (@var{a}, @var{b}).
##
## @command{ctrb} forms the controllability matrix.
## The numerical properties of @command{is_controllable}
## are much better for controllability tests.
## @end deftypefn

## Author: Kai P. Mueller <mueller@ifr.ing.tu-bs.de>
## Created: November 4, 1997
## based on is_controllable.m of Scottedward Hodel

function Qs = ctrb (sys, b)

  if (nargin == 2)
    a = sys;
  elseif (nargin == 1 && isstruct(sys))
    sysupdate(sys,"ss");
    [a,b] = sys2ss(sys);
  else
    print_usage ();
  endif

  if (!is_abcd(a,b))
    Qs = [];
  else
    ## no need to check dimensions, we trust is_abcd().
    [na, ma] = size(a);
    ## using imb avoids name conflict with the "mb" function
    [inb, imb] = size(b);
    Qs = zeros(na, ma*imb);
    for i = 1:na
      Qs(:, (i-1)*imb+1:i*imb) = b;
      b = a * b;
    endfor
  endif
endfunction
