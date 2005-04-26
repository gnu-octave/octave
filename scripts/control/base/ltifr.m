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
## Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301 USA.

## -*- texinfo -*-
## @deftypefn {Function File} {@var{out} =} ltifr (@var{a}, @var{b}, @var{w})
## @deftypefnx {Function File} {@var{out} =} ltifr (@var{sys}, @var{w})
## Linear time invariant frequency response of single-input systems.
##
## @strong{Inputs}
## @table @var
## @item a
## @itemx b
## coefficient matrices of @math{dx/dt = A x + B u}
## @item sys
## system data structure
## @item w
## vector of frequencies
## @end table
## @strong{Output}
## @table @var
## @item out
## frequency response, that is:
## @end table
## @iftex
## @tex
## $$ G(j\omega) = (j\omega I-A)^{-1}B $$
## @end tex
## @end iftex
## @ifinfo
## @example
##                            -1
##              G(s) = (jw I-A) B
## @end example
## @end ifinfo
## for complex frequencies @math{s = jw}.
## @end deftypefn

## Author: R. Bruce Tenison <btenison@eng.auburn.edu>
## Author: David Clem
## Author: A. S. Hodel <a.s.hodel@eng.auburn.edu>
## Created: July 1995
## updated by John Ingram August 1996 for system format

function out = ltifr (a, b, w)

  if ((nargin < 2) || (nargin > 3))
    error("incorrect number of input arguments");
  endif

  if (nargin == 2)
    sys = a;
    w = b;
    if(!isstruct(sys))
      error("two arguments: 1st must be a system data structure");
    endif

    if (!isvector(w))
      error("w must be a vector");
    endif

    [nn,nz,mm,pp] = sysdimensions(sys);
    if(mm != 1)       error("sys has %d > 1 inputs",mm); endif

    [a,b] = sys2ss(sys);

  else

    if (columns(a) != rows(b)),
      error("ltifr:  A(%dx%d), B(%dx%d) not compatibly dimensioned", ...
        rows(a), columns(a), rows(b), columns(b));
    endif

    if(columns(b) != 1)
      error("ltifr: b(%dx%d) must be a single column vector", ...
        rows(b),columns(b));
    endif

    if (!issquare(a))
      error("ltifr:  A(%dx$d) must be square.",rows(a),columns(a))
    endif

  endif

  if (!isvector(w))
    error("w must be a vector");
  endif

  ey = eye(size(a));
  lw = length(w);
  out = ones(columns(a),lw);

  for ii=1:lw,
    out(:,ii) = (w(ii)*ey-a)\b;
  endfor
endfunction
