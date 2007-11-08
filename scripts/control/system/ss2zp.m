## Copyright (C) 1996, 2000, 2004, 2005, 2006, 2007
##               Auburn University. All rights reserved.
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
## @deftypefn {Function File} {[@var{pol}, @var{zer}, @var{k}] =} ss2zp (@var{a}, @var{b}, @var{c}, @var{d})
## Converts a state space representation to a set of poles and zeros;
## @var{k} is a gain associated with the zeros.
##
## Used internally in system data structure format manipulations.
## @end deftypefn

## Author: David Clem
## Created: August 15, 1994
## Hodel: changed order of output arguments to zer, pol, k. July 1996
## a s hodel: added argument checking, allow for pure gain blocks aug 1996

function [zer, pol, k] = ss2zp (a, b, c, d)

  if (nargin != 4)
    print_usage ();
  endif

  [n, m, p] = abcddim (a, b, c, d);
  if (n == -1)
    error ("ss2tf: Non compatible matrix arguments");
  elseif (m != 1 || p != 1)
    error ("ss2tf: not SISO system: m=%d p=%d", m, p);
  endif

  if (n == 0)
    ## gain block only
    k = d;
    zer = pol = [];
  else
    ## First, get the denominator coefficients
    [zer, k] = tzero (a, b, c, d);
    pol = eig (a);
  endif

endfunction
