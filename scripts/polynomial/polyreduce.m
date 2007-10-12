## Copyright (C) 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2002, 2004,
##               2005, 2006, 2007 John W. Eaton
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
## @deftypefn {Function File} {} polyreduce (@var{c})
## Reduces a polynomial coefficient vector to a minimum number of terms by
## stripping off any leading zeros.
## @seealso{poly, roots, conv, deconv, residue, filter, polyval,
## polyvalm, polyderiv, polyinteg}
## @end deftypefn

## Author: Tony Richardson <arichard@stark.cc.oh.us>
## Created: June 1994
## Adapted-By: jwe

function p = polyreduce (p)

  if (nargin != 1)
    print_usage ();
  endif

  if (! (isvector (p) || isempty (p)))
    error ("polyreduce: argument must be a vector");
  endif

  if (! isempty (p) )

    index = find (p != 0);

    if (isempty (index))
      
      p = 0;
    
    else

      p = p (index (1):length (p));

    endif

  endif

endfunction
