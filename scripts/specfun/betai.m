## Copyright (C) 1998 John W. Eaton
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
## Software Foundation, 59 Temple Place - Suite 330, Boston, MA
## 02111-1307, USA.

## -*- texinfo -*-
## @deftypefn {Mapping Function} {} betai (@var{a}, @var{b}, @var{x})
## Return the incomplete Beta function,
## @iftex
## @tex
## $$
##  \beta (a, b, x) = B (a, b)^{-1} \int_0^x t^{(a-z)} (1-t)^{(b-1)} dt.
## $$
## @end tex
## @end iftex
## @ifinfo
## 
## @smallexample
##                                     x
##                                    /
## betai (a, b, x) = beta (a, b)^(-1) | t^(a-1) (1-t)^(b-1) dt.
##                                    /
##                                 t=0
## @end smallexample
## @end ifinfo
## 
## If x has more than one component, both @var{a} and @var{b} must be
## scalars.  If @var{x} is a scalar, @var{a} and @var{b} must be of
## compatible dimensions.
## @end deftypefn

## Author: jwe
## Created: 30 Jan 1998

function retval = betai (a, b, x)

  if (nargin == 3)
    retval = betainc (x, a, b);
  else
    usage ("betai (a, b, x)");
  endif

endfunction
