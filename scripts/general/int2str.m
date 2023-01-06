########################################################################
##
## Copyright (C) 1993-2023 The Octave Project Developers
##
## See the file COPYRIGHT.md in the top-level directory of this
## distribution or <https://octave.org/copyright/>.
##
## This file is part of Octave.
##
## Octave is free software: you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <https://www.gnu.org/licenses/>.
##
########################################################################

## -*- texinfo -*-
## @deftypefn {} {@var{str} =} int2str (@var{n})
## Convert an integer (or array of integers) to a string (or a character
## array).
##
## @example
## @group
## int2str (123)
##   @result{} 123
##
## s = int2str ([1, 2, 3; 4, 5, 6])
##   @result{} s =
##         1  2  3
##         4  5  6
##
## whos s
##   @result{} Variables in the current scope:
##         Attr Name        Size                     Bytes  Class
##         ==== ====        ====                     =====  =====
##              s           2x7                         14  char
##      Total is 14 elements using 14 bytes
## @end group
## @end example
##
## This function is not very flexible.  For better control over the
## results, use @code{sprintf} (@pxref{Formatted Output}).
##
## Programming Notes:
##
## Non-integers are rounded to integers before display.  Only the real part
## of complex numbers is displayed.
##
## @seealso{sprintf, num2str, mat2str}
## @end deftypefn

function str = int2str (n)

  if (nargin < 1)
    print_usage ();
  elseif (! (isnumeric (n) || islogical (n) || ischar (n)))
    error ("int2str: N must be a numeric, logical, or character array");
  endif

  if (ischar (n))
    str = n;
    return;
  elseif (isempty (n))
    str = "";
    return;
  endif

  n = round (real (n));

  ## Set up a suitable format string while ignoring Inf/NaN entries
  nan_inf = ! isfinite (n(:));
  ndgt = floor (log10 (max (abs (n(! nan_inf)))));
  if (isempty (ndgt) || ndgt == -Inf)
    ndgt = 0;  # All Inf or all zero array
  endif

  ndgt += 3;
  if (any (nan_inf))
    ndgt = max (ndgt, 5);
  endif

  ## FIXME: Integers should be masked to show only 16 significant digits
  fmt = sprintf ("%%%d.0f", ndgt);

  nd = ndims (n);
  nc = columns (n) * (nd - 1);    # ND-arrays are expanded in columns
  n  = permute (n, [2, 3:nd, 1]);
  fmt = [repmat(fmt, 1, nc), "\n"];
  strtmp = sprintf (fmt, n);
  str = strtrim (char (ostrsplit (strtmp, "\n", true)));

endfunction


%!assert (int2str (123), "123")
%!assert (int2str (-123), "-123")
%!assert (int2str (1.2), "1")
%!assert (int2str (1.6), "2")
%!assert (int2str ([1, 2, 3; 4, 5, 6]), ["1  2  3";"4  5  6"])
%!assert (int2str ([]), "")

%!error <Invalid call> int2str ()
%!error <N must be a numeric> int2str ({1})
