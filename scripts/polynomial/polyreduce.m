## Copyright (C) 1996 John W. Eaton
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

## usage: polyreduce(c)
##
## Reduces a polynomial coefficient vector to a minimum number of terms,
## i.e. it strips off any leading zeros.
##
## SEE ALSO: poly, roots, conv, deconv, residue, filter, polyval, polyvalm,
##           polyderiv, polyinteg

## Author: Tony Richardson <amr@mpl.ucsd.edu>
## Created: June 1994
## Adapted-By: jwe

function p = polyreduce (p)

  index = find (p == 0);

  if (length (index) != 0)

    index = find (index == 1:length (index));

    if (length (index) != 0)

      if (length (p) > 1)
	p = p (index (length (index))+1:length (p));
      endif

      if (length (p) == 0)
	p = 0;
      endif

    endif

  endif

endfunction
