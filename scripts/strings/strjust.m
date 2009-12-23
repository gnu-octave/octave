## Copyright (C) 2000, 2001, 2003, 2005, 2006, 2007, 2009 Paul Kienzle
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
## @deftypefn {Function File} {} strjust (@var{s}, ["left"|"right"|"center"])
## Shift the non-blank text of @var{s} to the left, right or center of
## the string.  If @var{s} is a string array, justify each string in the
## array.  Null characters are replaced by blanks.  If no justification
## is specified, then all rows are right-justified.  For example:
##
## @example
## @group
## strjust (["a"; "ab"; "abc"; "abcd"])
##      @result{} ans =
##            a
##           ab
##          abc
##         abcd
## @end group
## @end example
## @end deftypefn

function x = strjust (x, just)

  if (nargin < 1 || nargin > 2)
    print_usage ();
  endif

  if (nargin == 1)
    just = "right";
  else
    just = tolower (just);
  endif

  if (ndims (x) != 2)
    error ("needs a string or character matrix");
  endif

  if (isempty (x))
    return
  endif

  if (rows (x) == 1)
    ## string case
    nonbl = x != " " & x != char (0);
    n = length (x);
    switch (just)
    case "right"
      idx = find (nonbl, 1, "last");
      if (idx < n) # false if idx is empty
        x = [" "(1, ones (1, n-idx)), x(1:idx)];
      endif
    case "left"
      idx = find (nonbl, 1, "first");
      if (idx > 1) # false if idx is empty
        x = [x(idx:n), " "(1, ones (1, idx))];
      endif
    case "center"
      idx = find (nonbl, 1, "first");
      jdx = find (nonbl, 1, "last");
      if (idx > 1 || jdx < n)
        nsp = ((idx - 1) + (n - jdx)) / 2;
        lpad = " "(1, ones (1, floor (nsp)));
        rpad = " "(1, ones (1, ceil (nsp)));
        x = [lpad, x(idx:jdx), rpad];
      endif
    otherwise
      error ("strjust: invalid justify type: %s", just);
    endswitch
  else
    ## char matrix case.

    ## For all cases, left, right and center, the algorithm is the same.
    ## Find the number of blanks at the left/right end to determine the
    ## shift, rotate the row index by using mod with that shift, then
    ## translate the shifted row index into an array index.
    [nr, nc] = size (x);
    idx = (x != " " & x != char (0)).';
    if (strcmp (just, "right"))
      [N, hi] = max (cumsum (idx));
      shift = hi;
    elseif (strcmp (just, "left"))
      [N, lo] = max (cumsum (flipud (idx)));
      shift = (nc - lo);
    else
      [N, hi] = max (cumsum (idx));
      [N, lo] = max (cumsum (flipud (idx)));
      shift = ceil (nc - (lo-hi)/2);
    endif
    idx = rem (ones(nr,1)*[0:nc-1] + shift'*ones(1,nc), nc);
    x = x (idx*nr + [1:nr]'*ones(1,nc));

  endif

endfunction

%!error <Invalid call to strjust> strjust();
%!error <Invalid call to strjust> strjust(["a";"ab"], "center", 1);
%!assert (strjust (["a"; "ab"; "abc"; "abcd"]),
%!        ["   a";"  ab"; " abc"; "abcd"]);
%!assert (strjust (["a"; "ab"; "abc"; "abcd"], "center"),
%!        [" a  "; " ab"; "abc "; "abcd"]);
