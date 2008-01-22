## Copyright (C) 1994, 1996, 1997, 1999, 2000, 2003, 2004, 2005, 2006,
##               2007, 2008 John W. Eaton
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
## @deftypefn {Function File} {} union (@var{x}, @var{y})
## @deftypefnx{Function File} {} union (@var{x}, @var{y}, "rows")
## Return the set of elements that are in either of the sets @var{x} and
## @var{y}.  For example,
##
## @example
## @group
## union ([1, 2, 4], [2, 3, 5])
##      @result{} [1, 2, 3, 4, 5]
## @end group
## @end example
##
## If the optional third input argument is the string "rows" each row of
## the matrices @var{x} and @var{y} will be considered an element of sets.
## For example,
## @example
## @group
## union([1, 2; 2, 3], [1, 2; 3, 4], "rows")
##      @result{}  1   2
##     2   3
##     3   4
## @end group
## @end example
## @seealso{create_set, intersection, complement}
## @end deftypefn

## Author: jwe

function y = union (a, b, varargin)

  if (nargin < 2 || nargin > 3)
    print_usage ();
  endif

  if (nargin == 3 && ! strcmpi (varargin{1}, "rows"))
    error ("union: if a third input argument is present, it must be the string 'rows'");
  endif

  if (isempty (a))
    y = create_set (b, varargin{:});
  elseif (isempty (b))
    y = create_set (a, varargin{:});
  elseif (nargin == 2)
    y = create_set ([a(:); b(:)]);
    if (size (a, 1) == 1 || size (b, 1) == 1)
      y = y(:).';
    else
      y = y(:);
    endif
  elseif (ndims (a) == 2 && ndims (b) == 2 && columns (a) == columns (b))
    y = create_set ([a; b], "rows");
  else
    error ("union: input arguments must contain the same number of columns when \"rows\" is specified");
  endif

endfunction

%!assert(all (all (union ([1, 2, 4], [2, 3, 5]) == [1, 2, 3, 4, 5])));

%!assert(all (all (union ([1; 2; 4], [2, 3, 5]) == [1, 2, 3, 4, 5])));

%!assert(all (all (union ([1, 2, 3], [5; 7; 9]) == [1, 2, 3, 5, 7, 9])));

%!error union (1);

%!error union (1, 2, 3);

