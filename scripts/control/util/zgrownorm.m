## Copyright (C) 1996, 2000, 2003, 2005, 2007
##               Auburn University.  All rights reserved.
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
## @deftypefn {Function File} {[@var{nonz}, @var{zer}] =} zgrownorm (@var{mat}, @var{meps})
## Return @var{nonz} = number of rows of @var{mat} whose two norm
## exceeds @var{meps}, and @var{zer} = number of rows of mat whose two
## norm is less than @var{meps}.
## @end deftypefn

function [sig, tau] = zgrownorm (mat, meps)

  if (nargin != 2)
    print_usage ();
  endif

  rownorm = [];
  for ii = 1:rows (mat)
    rownorm(ii) = norm (mat(ii,:));
  endfor
  sig = sum (rownorm > meps);
  tau = sum (rownorm <= meps);

endfunction

