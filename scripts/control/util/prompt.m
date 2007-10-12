## Copyright (C) 1996, 2000, 2003, 2004, 2005, 2006, 2007
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
## @deftypefn {Function File} {} prompt (@var{str})
## Prompt user to continue
## 
## @strong{Input}
## @table @var
## @item str
## Input string. Its default value is: 
## @example 
## \n ---- Press a key to  continue ---
## @end example
## @end table
## @end deftypefn

## Author: David Clem
## Created: August 15, 1994
## Modified A. S. Hodel June 1995

function prompt (str)

  if (nargin > 1)
    print_usage ();
  elseif (nargin == 0)
    str = "\n ---- Press a key to continue ---";
  elseif (! ischar (str) )
    error ("prompt: input must be a string");
  endif

  disp (str);
  fflush (stdout);
  kbhit ();

endfunction
