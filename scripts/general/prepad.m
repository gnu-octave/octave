## Copyright (C) 1996, 1997 John W. Eaton
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
## @deftypefn {Function File} {@var{x} =} prepad (@var{x}, @var{l}, @var{c})
## @deftypefnx {Function File} {@var{x} =} postpad (@var{x}, @var{l}, @var{c})
##
## Prepends (appends) the scalar value @var{c} to the vector @var{x}
## until it is of length @var{l}.  If the third argument is not
## supplied, a value of 0 is used.
##
## If @code{length (x) > l}, elements from the beginning (end) of
## @var{x} are removed until a vector of length l is obtained.
## @end deftypefn

## Author: Tony Richardson <arichard@stark.cc.oh.us>
## Created: June 1994

function y = prepad (x, l, c)

  if (nargin == 2)
    c = 0;
  elseif (nargin < 2 || nargin > 3)
    usage ("prepad (x, l) or prepad (x, l, c)");
  endif

  if (! is_vector (x))
    error ("first argument must be a vector");
  elseif (! is_scalar (l))
    error ("second argument must be a scaler");
  endif

  if (l < 0)
    error ("second argument must be non-negative");
  endif

  lx = length (x);

  if (lx >= l)
    y = x(lx-l+1:lx);
  else
    if (rows (x) > 1)
      tmp = c * ones (l-lx, 1);
      y = [tmp; x];
    else
      tmp = c * ones (1, l-lx);
      y = [tmp, x];
    endif
  endif

endfunction
