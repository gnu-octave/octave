########################################################################
##
## Copyright (C) 2020-2023 The Octave Project Developers
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
## @deftypefn {} {@var{new_expr} =} __vectorize__ (@var{expr})
## Undocumented internal function.
## @end deftypefn

## The following function was translated directly from the original C++
## version.  Yes, it will be slow, but its use is strongly discouraged
## anyway, and most expressions will probably be short.  It may also be
## buggy.  Well, don't use this function!

function new_expr = __vectorize__ (expr)

  new_expr = "";

  len = length (expr);
  i = 1;

  while (i <= len)
    c = expr(i);

    if (c == "*" || c == "/" || c == "\\" || c == "^")
      if (i > 1 && expr(i-1) != ".")
        new_expr(end+1) = ".";
      endif

      ## Special case for ** operator.
      if (c == '*' && i < (len - 1) && expr(i+1) == '*')
        new_expr(end+1) = "*";
        i++;
      endif
    endif

    new_expr(end+1) = c;
    i++;

  endwhile

endfunction
