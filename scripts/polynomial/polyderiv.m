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
## @deftypefn {Function File} {} polyderiv (@var{c})
## Return the coefficients of the derivative of the polynomial whose
## coefficients are given by vector @var{c}.
## @end deftypefn
##
## @seealso{poly, polyinteg, polyreduce, roots, conv, deconv, residue,
## filter, polyval, and polyvalm}

## Author: Tony Richardson <arichard@stark.cc.oh.us>
## Created: June 1994
## Adapted-By: jwe

function q = polyderiv (p)

  if (nargin != 1)
    usage ("polyderiv (vector)");
  endif

  if (! isvector (p))
    error ("polyderiv: argument must be a vector");
  endif

  lp = numel (p);
  if (lp == 1)
    q = 0;
    return;
  elseif (lp == 0)
    q = [];
    return;
  end

  ## Force P to be a row vector.
  p = p(:).';

  q = p(1:(lp-1)) .* [(lp-1):-1:1];

endfunction
