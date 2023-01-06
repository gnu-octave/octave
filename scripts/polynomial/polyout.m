########################################################################
##
## Copyright (C) 1995-2023 The Octave Project Developers
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
## @deftypefn  {} {} polyout (@var{c})
## @deftypefnx {} {} polyout (@var{c}, @var{x})
## @deftypefnx {} {@var{str} =} polyout (@dots{})
## Display a formatted version of the polynomial @var{c}.
##
## The formatted polynomial
## @tex
## $$ c(x) = c_1 x^n + \ldots + c_n x + c_{n+1} $$
## @end tex
## @ifnottex
##
## @example
## c(x) = c(1) * x^n + @dots{} + c(n) x + c(n+1)
## @end example
##
## @end ifnottex
## is returned as a string or written to the screen if @code{nargout} is zero.
##
## The second argument @var{x} specifies the variable name to use for each term
## and defaults to the string @qcode{"s"}.
## @seealso{polyreduce}
## @end deftypefn

function str = polyout (c, x)

  if (nargin < 1)
    print_usage ();
  endif

  if (! isvector (c))
    error ("polyout: first argument must be a vector");
  endif

  if (nargin == 1)
    x = "s";
  elseif (! ischar (x))
    error ("polyout: second argument must be a string");
  endif

  n = length (c);
  if (n > 0)
    n1 = n+1;

    s = coeff (c(1));
    for i = 2:n
      if (real (c(i)) < 0)
        ns = " - ";
        c(i) = -c(i);
      else
        ns = " + ";
      endif

      s = sprintf ("%s*%s^%d%s%s", s, x, n1-i, ns, coeff (c(i)));

    endfor
  else
    s = " ";
  endif

  if (nargout == 0)
    disp (s);
  else
    str = s;
  endif

endfunction

function str = coeff (c)

  if (imag (c))
    if (real (c))
      str = sprintf ("(%s)", num2str (c, 5));
    else
      str = num2str (c, 5);
    endif
  else
    str = num2str (c, 5);
  endif

endfunction


%!assert (polyout ([3 2 1]), "3*s^2 + 2*s^1 + 1")
%!assert (polyout ([3 2 1], "x"), "3*x^2 + 2*x^1 + 1")
%!assert (polyout ([3 2 1], "wxyz"), "3*wxyz^2 + 2*wxyz^1 + 1")
%!assert (polyout ([5 4 3 2 1], "1"),"5*1^4 + 4*1^3 + 3*1^2 + 2*1^1 + 1")

%!error polyout ([])
