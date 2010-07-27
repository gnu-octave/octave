## Copyright (C) 1996, 1999, 2000, 2002, 2004, 2005, 2006, 2007, 2008, 2009
##               Kurt Hornik
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
## @deftypefn {Function File} {} rindex (@var{s}, @var{t})
## Return the position of the last occurrence of the character string
## @var{t} in the character string @var{s}, or 0 if no occurrence is
## found.  For example:
##
## @example
## @group
## rindex ("Teststring", "t")
##      @result{} 6
## @end group
## @end example
##
## @strong{Caution:} This function does not work for arrays of
## character strings.
## @seealso{find, index}
## @end deftypefn

## Author: Kurt Hornik <Kurt.Hornik@wu-wien.ac.at>
## Adapted-By: jwe

function n = rindex (s, t)

  ## This is patterned after the AWK function of the same name.

  if (nargin != 2)
    print_usage ();
  endif

  n = index (s, t, "last");

endfunction

%!assert(rindex ("foobarbaz", "b") == 7 && rindex ("foobarbaz", "o") == 3);

%!error rindex ();

%!error rindex ("foo", "bar", 3);

