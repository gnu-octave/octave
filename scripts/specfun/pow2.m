## Copyright (C) 1995, 1996  Kurt Hornik
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
## @deftypefn {Mapping Function} {} pow2 (@var{x})
## @deftypefnx {Mapping Function} {} pow2 (@var{f}, @var{e})
## With one argument, computes
## @iftex
## @tex
##  $2^x$
## @end tex
## @end iftex
## @ifinfo
##  2 .^ x
## @end ifinfo
## for each element of @var{x}.  With two arguments, returns
## @iftex
## @tex
##  $f \cdot 2^e$.
## @end tex
## @end iftex
## @ifinfo
##  f .* (2 .^ e).
## @end ifinfo
## @end deftypefn
##
## @seealso{nextpow2}

## Author: AW <Andreas.Weingessel@ci.tuwien.ac.at>
## Created: 17 October 1994
## Adapted-By: jwe

function y = pow2 (f, e)

  if (nargin == 1)
    y = 2 .^ f;
  elseif (nargin == 2)
    y = f .* (2 .^ e);
  else
    usage ("y = pow2 (f, e)");
  endif

endfunction
