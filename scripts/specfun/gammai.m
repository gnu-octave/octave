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
## @deftypefn {Mapping Function} {} gammai (@var{a}, @var{x})
## Computes the incomplete gamma function,
## @iftex
## @tex
## $$
##  \gamma (a, x) = {\displaystyle\int_0^x e^{-t} t^{a-1} dt \over \Gamma (a)}
## $$
## @end tex
## @end iftex
## @ifinfo
##
## @smallexample
##                               x
##                     1        /
## gammai (a, x) = ---------    | exp (-t) t^(a-1) dt
##                 gamma (a)    /
##                           t=0
## @end smallexample
## @end ifinfo
##
## If @var{a} is scalar, then @code{gammai (@var{a}, @var{x})} is returned
## for each element of @var{x} and vice versa.
##
## If neither @var{a} nor @var{x} is scalar, the sizes of @var{a} and
## @var{x} must agree, and @var{gammai} is applied element-by-element.
## @end deftypefn
## @seealso{gamma and lgamma}

## Author: jwe
## Created: 30 Jan 1998

function retval = gammai (a, x)

  if (nargin == 2)
    retval = gammainc (x, a);
  else
    usage ("gammai (a, x)");
  endif

endfunction
