## Copyright (C) 1993, 1994, 1995, 1996, 1997, 1999, 2000, 2002, 2004,
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
## @deftypefn {Mapping Function} {} rem (@var{x}, @var{y})
## Return the remainder of @code{@var{x} / @var{y}}, computed using the
## expression
##
## @example
## x - y .* fix (x ./ y)
## @end example
##
## An error message is printed if the dimensions of the arguments do not
## agree, or if either of the arguments is complex.
## @seealso{mod, round}
## @end deftypefn

## Author: jwe

function r = rem (x, y)

  if (nargin != 2)
    print_usage ();
  endif

  if (! size_equal (x, y) && ! (isscalar (x) || isscalar (y)))
    error ("rem: argument sizes must agree");
  endif

  ## Matlab allows complex arguments, but as far as I can tell, that's a
  ## bunch of hooey.

  if (isreal (x) && isreal (y))
    r = x - y .* fix (x ./ y);
  else
    error ("rem: complex arguments are not allowed");
  endif

endfunction
