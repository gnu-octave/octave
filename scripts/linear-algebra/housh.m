## Copyright (C) 1995-2011 A. Scottedward Hodel
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
## @deftypefn {Function File} {[@var{housv}, @var{beta}, @var{zer}] =} housh (@var{x}, @var{j}, @var{z})
## Compute Householder reflection vector @var{housv} to reflect @var{x}
## to be the j-th column of identity, i.e.,
##
## @example
## @group
## (I - beta*housv*housv')x =  norm(x)*e(j) if x(1) < 0,
## (I - beta*housv*housv')x = -norm(x)*e(j) if x(1) >= 0
## @end group
## @end example
##
## @noindent
## Inputs
##
## @table @var
## @item x
## vector
##
## @item j
## index into vector
##
## @item z
## threshold for zero  (usually should be the number 0)
## @end table
##
## @noindent
## Outputs (see Golub and Van Loan):
##
## @table @var
## @item beta
## If beta = 0, then no reflection need be applied (zer set to 0)
##
## @item housv
## householder vector
## @end table
## @end deftypefn

## Author: A. S. Hodel
## Created: August 1995

function [housv, beta, zer] = housh (x, j, z)

  if (nargin != 3)
    print_usage ();
  endif

  ## Check for valid inputs.
  if (! isvector (x) && ! isscalar (x))
    error ("housh: first input must be a vector");
  elseif (! isscalar(j))
    error ("housh: second argment must be an integer scalar");
  else
    housv = x;
    m = max (abs (housv));
    if (m != 0.0)
      housv = housv / m;
      alpha = norm (housv);
      if (alpha > z)
        beta = 1.0 / (alpha * (alpha + abs (housv(j))));
        sg = sign (housv(j));
        if (sg == 0)
          sg = 1;
        endif
        housv(j) = housv(j) + alpha*sg;
      else
        beta = 0.0;
      endif
    else
      beta = 0.0;
    endif
    zer = (beta == 0);
  endif

endfunction
