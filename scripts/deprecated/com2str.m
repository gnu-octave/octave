## Copyright (C) 1998, 2004, 2005, 2006, 2007
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
## @deftypefn {Function File} {} com2str (@var{zz}, @var{flg})
## This function has been deprecated.  Use num2str instead.
##
## Convert complex number to a string.
## @strong{Inputs}
## @table @var
## @item zz
## complex number
## @item flg
## format flag
## 0 (default):            -1, 0, 1,   1i,   1 + 0.5i
## 1 (for use with zpout): -1, 0, + 1, + 1i, + 1 + 0.5i
## @end table
## @end deftypefn

## Deprecated in version 3.0

function retval = com2str (zz, flg)

  persistent warned = false;
  if (! warned)
    warned = true;
    warning ("Octave:deprecated-function",
             "com2str is obsolete and will be removed from a future version of Octave; please use num2str instead");
  endif

  if (nargin < 1 || nargin > 2)
    print_usage ();
  endif
  if (nargin == 1)
    flg = 0;
  endif

  if (! (isscalar (zz) && isscalar (flg)))
    error ("com2str: arguments must be a scalar");
  endif

  if (flg != 0 && flg != 1)
    error ("invalid flg value: %d", flg);
  endif

  sgns = "+-";
  rz = real (zz);
  iz = imag (zz);
  az = abs (zz);
  if (iz == 0)
    ## strictly a real number
    switch (flg)
      case(0)
	retval = num2str (rz);
      case(1)
	retval = [sgns(1+(rz<0)), " ", num2str(abs(rz))];
    endswitch
  elseif (rz == 0)
    ## strictly an imaginary number
    switch (flg)
      case(0)
	retval = [num2str(iz), "i"];
      case(1)
	retval = [sgns(1+(iz<0)), " ", num2str(abs(iz)), "i"];
    endswitch
  else
    ## complex number
    ## strictly an imaginary number
    switch (flg)
      case(0)
	retval = [num2str(rz), " ", com2str(i*iz,1)];
      case(1)
	retval = [sgns(1+(rz<0)), " ", num2str(abs(rz)), " ", com2str(i*iz,1)];
    endswitch
  endif

endfunction
