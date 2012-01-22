## Copyright (C) 1995-2012 Kurt Hornik
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
## @deftypefn {Mapping Function} {@var{z} =} xor (@var{x}, @var{y})
## Return the `exclusive or' of the entries of @var{x} and @var{y}.
## For boolean expressions @var{x} and @var{y},
## @code{xor (@var{x}, @var{y})} is true if and only if one of @var{x} or
## @var{y} is true.  Otherwise, for @var{x} and @var{y} both true or both
## false, @code{xor} returns false.
##
## The truth table for the xor operation is
##
## @multitable @columnfractions 0.44 .03 .05 .03 0.44
## @item @tab @var{x} @tab @var{y} @tab @var{z} @tab
## @item @tab 0 @tab 0 @tab 0 @tab
## @item @tab 1 @tab 0 @tab 1 @tab
## @item @tab 0 @tab 1 @tab 1 @tab
## @item @tab 1 @tab 1 @tab 0 @tab
## @end multitable
##
## @seealso{and, or, not}
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Created: 16 September 1994
## Adapted-By: jwe

function z = xor (x, y)

  if (nargin == 2)
    if (isscalar (x) || isscalar (y) || size_equal (x, y))
      ## Typecast to logicals is necessary for other numeric types.
      z = logical (x) != logical (y);
    else
      try
        z = bsxfun (@xor, x, y);
      catch
        error ("xor: X and Y must be of compatible size or scalars");
      end_try_catch
    endif
  else
    print_usage ();
  endif

endfunction

%!assert((xor ([1, 1, 0, 0], [0, 1, 0, 1]) == [1, 0, 0, 1]
%! && xor ([i, i, 0, 0], [1, 0, 1, 0]) == [0, 1, 1, 0]));

%!assert(all (all (xor (eye (2), fliplr (eye (2))) == ones (2))));

%!error xor ();

%!error xor (1, 2, 3);

