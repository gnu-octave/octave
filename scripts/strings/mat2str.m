########################################################################
##
## Copyright (C) 2002-2023 The Octave Project Developers
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
## @deftypefn  {} {@var{s} =} mat2str (@var{x}, @var{n})
## @deftypefnx {} {@var{s} =} mat2str (@var{x}, @var{n}, "class")
## Format real, complex, and logical matrices as strings.
##
## The returned string may be used to reconstruct the original matrix by using
## the @code{eval} function.
##
## The precision of the values is given by @var{n}.  If @var{n} is a scalar
## then both real and imaginary parts of the matrix are printed to the same
## precision.  Otherwise @code{@var{n}(1)} defines the precision of the real
## part and @code{@var{n}(2)} defines the precision of the imaginary part.
## The default for @var{n} is 15.
##
## If the argument @qcode{"class"} is given then the class of @var{x} is
## included in the string in such a way that @code{eval} will result in the
## construction of a matrix of the same class.
##
## @example
## @group
## mat2str ([ -1/3 + i/7; 1/3 - i/7 ], [4 2])
##      @result{} "[-0.3333+0.14i;0.3333-0.14i]"
##
## mat2str ([ -1/3 +i/7; 1/3 -i/7 ], [4 2])
##      @result{} "[-0.3333+0i 0+0.14i;0.3333+0i -0-0.14i]"
##
## mat2str (int16 ([1 -1]), "class")
##      @result{} "int16([1 -1])"
##
## mat2str (logical (eye (2)))
##      @result{} "[true false;false true]"
##
## isequal (x, eval (mat2str (x)))
##      @result{} 1
## @end group
## @end example
##
## @seealso{sprintf, num2str, int2str}
## @end deftypefn

function s = mat2str (x, n = 15, cls = "")

  if (nargin < 1 || ! (isnumeric (x) || islogical (x)))
    print_usage ();
  elseif (ndims (x) > 2)
    error ("mat2str: X must be two dimensional");
  endif

  if (nargin == 2 && ischar (n))
    cls = n;
    n = 15;
  elseif (isempty (n))
    n = 15;   # Default precision
  elseif (numel (n) > 2)
    error ("mat2str: N must have only 1 or 2 elements");
  else
    n = fix (n);
  endif

  x_islogical = islogical (x);
  x_iscomplex = iscomplex (x);

  if (x_iscomplex)
    if (isscalar (n))
      n = [n, n];
    endif
    fmt = sprintf ("%%.%dg%%+.%dgi", n(1), n(2));
  elseif (x_islogical)
    v = {"false", "true"};
    fmt = "%s";
  else
    fmt = sprintf ("%%.%dg", n(1));
  endif

  nel = numel (x);

  if (nel == 0)
    ## Empty, only print brackets
    s = "[]";
  elseif (nel == 1)
    ## Scalar X, don't print brackets
    if (x_iscomplex)
      s = sprintf (fmt, real (x), imag (x));
    elseif (x_islogical)
      s = v{x+1};
    else
      s = sprintf (fmt, x);
    endif
  else
    ## Non-scalar X, print brackets
    fmt = [fmt " "];
    if (x_iscomplex)
      t = x.';
      s = sprintf (fmt, [real(t(:))'; imag(t(:))']);
    elseif (x_islogical)
      t = v(x.'+1);
      s = cstrcat (sprintf (fmt, t{:}));
    else
      s = sprintf (fmt, x.');
    endif

    s = ["[" s];
    s(end) = "]";
    idx = strfind (s, " ");
    nc = columns (x);
    s(idx(nc:nc:end)) = ";";
  endif

  if (strcmp ("class", cls))
    s = [class(x) "(" s ")"];
  endif

endfunction


%!assert (mat2str (0.7), "0.7")
%!assert (mat2str (pi), "3.14159265358979")
%!assert (mat2str (pi, 5), "3.1416")
%!assert (mat2str (single (pi), 5, "class"), "single(3.1416)")
%!assert (mat2str ([-1/3 + i/7; 1/3 - i/7], [4 2]),
%!        "[-0.3333+0.14i;0.3333-0.14i]")
%!assert (mat2str ([-1/3 +i/7; 1/3 -i/7], [4 2]),
%!        "[-0.3333+0i 0+0.14i;0.3333+0i -0-0.14i]")
%!assert (mat2str (int16 ([1 -1]), "class"), "int16([1 -1])")
%!assert (mat2str (true), "true")
%!assert (mat2str (false), "false")
%!assert (mat2str (logical (eye (2))), "[true false;false true]")
%!assert (mat2str (logical ([0 1; 0 0])), "[false true;false false]")

## Test input validation
%!error <Invalid call> mat2str ()
%!error mat2str (["Hello"])
%!error <X must be two dimensional> mat2str (ones (3,3,2))
%!error <N must have only 1 or 2 elements> mat2str (ones (3,3), [1 2 3])
