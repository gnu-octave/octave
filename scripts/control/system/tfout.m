## Copyright (C) 1996 Auburn University.  All rights reserved.
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
## @deftypefn {Function File} {} tfout (@var{num}, @var{denom}, @var{x})
## Print formatted transfer function @math{n(s)/d(s)} to the screen.
## @var{x} defaults to the string @code{"s"}
## @seealso{polyval, polyvalm, poly, roots, conv, deconv, residue,
## filter, polyderiv, polyinteg, polyout}
## @end deftypefn

## Author: A. S. Hodel <a.s.hodel@eng.auburn.edu>
## Created: June 1995

function tfout (num, denom, x)

  if (nargin < 2 ) | (nargin > 3) | (nargout != 0 )
    print_usage ();
  endif

  if ( (!isvector(num)) | (!isvector(denom)) )
    error("tfout: first two argument must be vectors");
  endif

  if (nargin == 2)
    x = "s";
  elseif( ! ischar(x) )
    error("tfout: third argument must be a string");
  endif

  numstring = polyout(num,x);
  denomstring = polyout(denom,x);
  len = max(length(numstring),length(denomstring));
  if(len > 0)
    y = strrep(blanks(len)," ","-");
    disp(numstring)
    disp(y)
    disp(denomstring)
  else
    error ("tfout: empty transfer function")
  end

endfunction
