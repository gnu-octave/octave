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
## @deftypefn {Function File} {} postpad (@var{x}, @var{l}, @var{c})
## See prepad.
## @end deftypefn

## Author: Tony Richardson <arichard@stark.cc.oh.us>
## Created: June 1994

function y = postpad (x, l, c)

  if (nargin == 2)
    c = 0;
  elseif (nargin < 2 || nargin > 3)
    usage ("postpad (x, l) or postpad (x, l, c)");
  endif

  if (! is_matrix (x))
    error ("first argument must be a vector or matrix");
  elseif (! is_scalar (l))
    error ("second argument must be a scaler");
  endif

  if (l < 0)
    error ("second argument must be non-negative");
  endif

  [nr, nc] = size (x);
  if (nr == 1)
    if (nc >= l)
      y = x(1:l);
    else
      y = [x, c*ones(1,l-nc)];
    endif
  else
    if (nr >= l)
      y = x(1:l,:);
    else
      y = [x ; c*ones(l-nr,nc)];
    endif
  endif

endfunction
