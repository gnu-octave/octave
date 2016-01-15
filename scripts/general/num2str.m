## Copyright (C) 1993-2015 John W. Eaton
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
## @deftypefn  {} {} num2str (@var{x})
## @deftypefnx {} {} num2str (@var{x}, @var{precision})
## @deftypefnx {} {} num2str (@var{x}, @var{format})
## Convert a number (or array) to a string (or a character array).
##
## The optional second argument may either give the number of significant
## digits (@var{precision}) to be used in the output or a format template
## string (@var{format}) as in @code{sprintf} (@pxref{Formatted Output}).
## @code{num2str} can also process complex numbers.
##
## Examples:
##
## @example
## @group
## num2str (123.456)
##      @result{} "123.46"
##
## num2str (123.456, 4)
##      @result{} "123.5"
##
## s = num2str ([1, 1.34; 3, 3.56], "%5.1f")
##      @result{} s =
##         1.0  1.3
##         3.0  3.6
## whos s
##      @result{}
##       Attr Name        Size                     Bytes  Class
##       ==== ====        ====                     =====  =====
##            s           2x8                         16  char
##
## num2str (1.234 + 27.3i)
##      @result{} "1.234+27.3i"
## @end group
## @end example
##
## The @code{num2str} function is not very flexible.  For better control
## over the results, use @code{sprintf} (@pxref{Formatted Output}).
##
## Programming Notes:
##
## For @sc{matlab} compatibility, leading spaces are stripped before returning
## the string.
##
## Integers larger than @code{flintmax} may not be displayed correctly.
##
## For complex @var{x}, the format string may only contain one output
## conversion specification and nothing else.  Otherwise, results will be
## unpredictable.
##
## Any optional @var{format} specified by the programmer is used without
## modification.  This is in contrast to @sc{matlab} which tampers with the
## @var{format} based on internal heuristics.
## @seealso{sprintf, int2str, mat2str}
## @end deftypefn

## Author: jwe

function retval = num2str (x, arg)

  if (nargin != 1 && nargin != 2)
    print_usage ();
  elseif (! (isnumeric (x) || islogical (x) || ischar (x)))
    error ("num2str: X must be a numeric, logical, or character array");
  endif

  if (ischar (x))
    retval = x;
  elseif (isempty (x))
    retval = "";
  elseif (isreal (x))
    if (nargin == 2)
      if (ischar (arg))
        fmt = arg;
      elseif (isnumeric (arg) && isscalar (arg) && arg >= 0 && arg == fix (arg))
        fmt = sprintf ("%%%d.%dg", arg+7, arg);
      else
        error ("num2str: PRECISION must be a scalar integer >= 0");
      endif
    else
      if (isnumeric (x))
        ## Set up a suitable format string while ignoring Inf/NaN entries
        valid = isfinite (x(:));
        ndgt = floor (log10 (max (abs (x(valid)))));
        if (isempty (ndgt) || ndgt == -Inf)
          ndgt = 0;  # All Inf or all zero array
        endif

        if (any (x(valid) != fix (x(valid))))
          ## Floating point input
          ndgt = max (ndgt + 5, 5);   # Keep at least 5 significant digits
          ndgt = min (ndgt, 16);      # Cap significant digits at 16
          fmt = sprintf ("%%%d.%dg", ndgt+7, ndgt);
        else
          ## Integer input
          ndgt += 3;
          if (any (! valid))
            ndgt = max (ndgt, 5);     # Allow space for Inf/NaN
          endif
          ## FIXME: Integers should be masked to show only 16 significant digits
          ##        See %!xtest with 1e23 below.
          fmt = sprintf ("%%%d.0f", ndgt);
        endif
      else
        ## Logical input
        fmt = "%3d";
      endif
    endif
    fmt = do_string_escapes (fmt);  # required now that '\n' is interpreted.
    nd = ndims (x);
    nc = columns (x) * (nd - 1);    # ND-arrays are expanded in columns
    x  = permute (x, [2, 3:nd, 1]);
    if (! (sum (fmt == "%") > 1 || any (strcmp (fmt, {"%s", "%c"}))))
      fmt = [deblank(repmat (fmt, 1, nc)), "\n"];
    endif
    strtmp = sprintf (fmt, x);
    retval = strtrim (char (ostrsplit (strtmp, "\n", true)));
  else   # Complex matrix input
    if (nargin == 2)
      if (ischar (arg))
        fmt = [deblank(arg) "%-+" arg(2:end) "i"];
      elseif (isnumeric (arg) && isscalar (arg) && arg >= 0 && arg == fix (arg))
        fmt = sprintf ("%%%d.%dg%%-+%d.%dgi", arg+7, arg, arg+7, arg);
      else
        error ("num2str: PRECISION must be a scalar integer >= 0");
      endif
    else
      ## Set up a suitable format string while ignoring Inf/NaN entries
      valid_real = isfinite (real (x(:)));
      valid_imag = isfinite (imag (x(:)));
      ndgt = floor (log10 (max (max (abs (real (x(valid_real)))),
                                max (abs (imag (x(valid_imag)))))));
      if (isempty (ndgt) || ndgt == -Inf)
        ndgt = 0;  # All Inf or all zero array
      endif

      if (any (x(valid_real & valid_imag) != fix (x(valid_real & valid_imag))))
        ## Floating point input
        ndgt = max (ndgt + 5, 5);   # Keep at least 5 significant digits
        ndgt = min (ndgt, 16);      # Cap significant digits at 16
        fmt = sprintf ("%%%d.%dg%%-+%d.%dgi", ndgt+7, ndgt, ndgt+7, ndgt);
      else
        ## Integer input
        ndgt += 3;
        ## FIXME: Integers should be masked to show only 16 significant digits
        ##        See %!xtest below
        fmt = sprintf ("%%%d.0f%%-+%d.0fi", ndgt, ndgt);
      endif
    endif

    ## Manipulate the complex value to have real values in the odd
    ## columns and imaginary values in the even columns.
    nd = ndims (x);
    nc = columns (x);
    idx = repmat ({':'}, nd, 1);
    perm(1:2:2*nc) = 1:nc;
    perm(2:2:2*nc) = nc + (1:nc);
    idx{2} = perm;
    x = horzcat (real (x), imag (x));
    x = x(idx{:});

    fmt = [deblank(repmat(fmt, 1, nc * (nd - 1))), "\n"];
    tmp = sprintf (fmt, permute (x, [2, 3:nd, 1]));

    ## Put the "i"'s where they are supposed to be.
    tmp = regexprep (tmp, " +i\n", "i\n");
    tmp = regexprep (tmp, "( +)i", "i$1");

    retval = strtrim (char (ostrsplit (tmp(1:end-1), "\n")));
  endif

endfunction


%!assert (num2str (123), "123")
%!assert (num2str (1.23), "1.23")
%!assert (num2str (123.456, 4), "123.5")
%!assert (num2str ([1, 1.34; 3, 3.56], "%5.1f"),  ["1.0  1.3"; "3.0  3.6"])
%!assert (num2str (1.234 + 27.3i), "1.234+27.3i")
%!assert (num2str ([true false true]), "1  0  1");

%!assert (num2str (19440606), "19440606")
%!assert (num2str (2^33), "8589934592")
%!assert (num2str (-2^33), "-8589934592")
%!assert (num2str (2^33+1i), "8589934592+1i")
%!assert (num2str (-2^33+1i), "-8589934592+1i")
%!assert (num2str ([0 0 0]), "0  0  0")
%!assert (num2str (inf), "Inf")
%!assert (num2str ([inf -inf]), "Inf -Inf")
%!assert (num2str ([inf NaN -inf]), "Inf  NaN -Inf")
%!assert (num2str ([complex(Inf,0), complex(0,-Inf)]), "Inf+0i   0-Infi")
%!assert (num2str (complex(Inf,1)), "Inf+1i")
%!assert (num2str (complex(1,Inf)), "1+Infi")
%!assert (num2str (nan), "NaN")
%!assert (num2str (complex (NaN, 1)), "NaN+1i")
%!assert (num2str (complex (1, NaN)), "1+NaNi")
%!assert (num2str (NA), "NA")
%!assert (num2str (complex (NA, 1)), "NA+1i")
%!assert (num2str (complex (1, NA)), "1+NAi")

## ND-arrays are concatenated in columns
%!shared m, x
%! m = magic (3);
%! x = cat (3, m, -m);

## real case
%!test
%! y = num2str (x);
%! assert (rows (y) == 3);
%! assert (y, ["8  1  6 -8 -1 -6"
%!             "3  5  7 -3 -5 -7"
%!             "4  9  2 -4 -9 -2"]);

## complex case
%!test
%! x(1,1,2) = -8+2i;
%! y = num2str (x);
%! assert (rows (y) == 3);
%! assert (y, ["8+0i   1+0i   6+0i  -8+2i  -1+0i  -6+0i"
%!             "3+0i   5+0i   7+0i  -3+0i  -5+0i  -7+0i"
%!             "4+0i   9+0i   2+0i  -4+0i  -9+0i  -2+0i"]);

## Clear shared variables
%!shared

## FIXME: Integers greater than flintmax() - 1 should be masked to show just
##        16 digits of precision.
%!xtest
%! assert (num2str (1e23), "100000000000000000000000");

## Test for bug #44864, extra rows generated from newlines in format
%!assert (rows (num2str (magic (3), "%3d %3d %3d\n")), 3)

## Test for bug #45174
%!assert (num2str ([65 66 67], "%s"), "ABC")

%!error num2str ()
%!error num2str (1, 2, 3)
%!error <X must be a numeric> num2str ({1})
%!error <PRECISION must be a scalar integer .= 0> num2str (1, {1})
%!error <PRECISION must be a scalar integer .= 0> num2str (1, ones (2))
%!error <PRECISION must be a scalar integer .= 0> num2str (1, -1)
%!error <PRECISION must be a scalar integer .= 0> num2str (1, 1.5)
%!error <PRECISION must be a scalar integer .= 0> num2str (1+1i, {1})
%!error <PRECISION must be a scalar integer .= 0> num2str (1+1i, ones (2))
%!error <PRECISION must be a scalar integer .= 0> num2str (1+1i, -1)
%!error <PRECISION must be a scalar integer .= 0> num2str (1+1i, 1.5)

