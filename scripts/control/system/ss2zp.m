## Copyright (C) 1996 Auburn University.  All rights reserved.
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by the
## Free Software Foundation; either version 2, or (at your option) any
## later version.
##
## Octave is distributed in the hope that it will be useful, but WITHOUT
## ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
## FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
## for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, write to the Free
## Software Foundation, 59 Temple Place, Suite 330, Boston, MA 02111 USA.

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

  if(nargin != 4)
    usage("[zer,pol,k] = ss2zp(a,b,c,d)");
  endif

  [n,m,p] = abcddim(a,b,c,d);
  if (n == -1)
    error("ss2tf: Non compatible matrix arguments");
  elseif ( (m != 1) | (p != 1))
    error(["ss2tf: not SISO system: m=",num2str(m)," p=",num2str(p)]);
  endif

  if(n == 0)
    ## gain block only
    k = d;
    zer = pol = [];
  else
    ## First, get the denominator coefficients
    [zer,k] = tzero(a,b,c,d);
    pol = eig(a);
  endif
endfunction

