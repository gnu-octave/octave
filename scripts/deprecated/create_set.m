## Copyright (C) 1994, 1996, 1997, 1999, 2000, 2004, 2005, 2006, 2007,
##               2008, 2009 John W. Eaton
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
## @deftypefn  {Function File} {} create_set (@var{x})
## @deftypefnx {Function File} {} create_set (@var{x}, "rows")
## This function has been deprecated.  Use unique instead.
## @end deftypefn

## Return a row vector containing the unique values in @var{x}, sorted in
## ascending order.  For example,
##
## @example
## @group
## create_set ([ 1, 2; 3, 4; 4, 2; 1, 2 ])
##      @result{} [ 1, 2, 3, 4 ]
## @end group
## @end example
##
## If the optional second input argument is the string "rows" each row of
## the matrix @var{x} will be considered an element of set.  For example,
## @example
## @group
## create_set ([ 1, 2; 3, 4; 4, 2; 1, 2 ], "rows")
##      @result{}  1   2
##     3   4
##     4   2
## @end group
## @end example
## @seealso{union, intersect, complement, unique}

## Author: jwe

## Deprecated in version 3.2

function y = create_set (x, rows_opt)

  persistent warned = false;
  if (! warned)
    warned = true;
    warning ("Octave:deprecated-function",
             "create_set is obsolete and will be removed from a future version of Octave, please use unique instead");
  endif

  if (nargin < 1 || nargin > 2)
    print_usage ();
  endif
  
  if (nargin == 1)
    y = unique (x)(:)';
  elseif (strcmpi (rows_opt, "rows"))
    y = unique (x, "rows");
  else
    error ("create_set: expecting \"rows\" as second argument");
  endif

endfunction

%!assert(all (all (create_set ([1, 2, 3, 4, 2, 4]) == [1, 2, 3, 4])));
%!assert(all (all (create_set ([1, 2; 3, 4; 2, 4]) == [1, 2, 3, 4])));
%!assert(all (all (create_set ([1; 2; 3; 4; 2; 4]) == [1, 2, 3, 4])));
%!assert(isempty (create_set ([])));
%!error create_set (1, 2);

