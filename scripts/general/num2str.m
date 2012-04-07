## Copyright (C) 1993-2012 John W. Eaton
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
## @deftypefn  {Function File} {} num2str (@var{x})
## @deftypefnx {Function File} {} num2str (@var{x}, @var{precision})
## @deftypefnx {Function File} {} num2str (@var{x}, @var{format})
## Convert a number (or array) to a string (or a character array).  The
## optional second argument may either give the number of significant
## digits (@var{precision}) to be used in the output or a format
## template string (@var{format}) as in @code{sprintf} (@pxref{Formatted
## Output}).  @code{num2str} can also handle complex numbers.  For
## example:
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
## Note that for complex @var{x}, the format string may only contain one
## output conversion specification and nothing else.  Otherwise, you
## will get unpredictable results.
## @seealso{sprintf, int2str, mat2str}
## @end deftypefn

## Author: jwe

## FIXME: Matlab output is essentially sprintf ("%p+8.pg", x)
## where precision p is log10(x) + 4.  This produces differently sized
## columns for matrix input with both integer and floating point values
## which can look rather odd.  For the time being (2012/04/07) we prefer
## Octave's output over strict compatibility.

## FIXME: Switch from "%d" decimal format to "%g" floating point format
## is based on intmax ("int32").  On 64-bit machines we should probably use
## intmax ("int64").

function retval = num2str (x, arg)

  if (nargin != 1 && nargin != 2)
    print_usage ();
  endif

  if (ischar (x))
    retval = x;
  elseif (isempty (x))
    retval = "";
  elseif (isreal (x))
    if (nargin == 2)
      if (ischar (arg))
        fmt = arg;
      else
        if (isnumeric (x) && x == fix (x)
            && abs (x) < min (10 .^ arg, intmax ("int32")))
          fmt = sprintf ("%%%dd  ", arg);
        else
          fmt = sprintf ("%%%d.%dg", arg+7, arg);
        endif
      endif
    else
      if (isnumeric (x) && x == fix (x) && abs (x) < 1e10)
        if (any (x(:) != 0))
          dgt = floor (log10 (max (abs (x(:))))) + (any (x(:) < 0)) + 2;
        else
          dgt = 2;
        endif
        if (any (abs (x(:)) > intmax ("int32")))
          fmt = sprintf ("%%%dg  ", dgt);
        else
          fmt = sprintf ("%%%dd  ", dgt);
        endif
      elseif (isscalar (x))
        fmt = "%11.5g";
      else
        fmt = "%11.5g";
      endif
    endif
    fmt = cstrcat (deblank (repmat (fmt, 1, columns (x))), "\n");
    nd = ndims (x);
    tmp = sprintf (fmt, permute (x, [2, 1, 3:nd]));
    tmp(end) = "";
    retval = strtrim (char (strsplit (tmp, "\n")));
  else  # complex x
    if (nargin == 2)
      if (ischar (arg))
        fmt = cstrcat (arg, "%-+", arg(2:end), "i");
      else
        if (isnumeric (x) && x == fix (x)
            && abs (x) < min (10 .^ arg, intmax ("int32")))
          fmt = sprintf ("%%%dd%%-+%ddi  ", arg, arg);
        else
          fmt = sprintf ("%%%d.%dg%%-+%d.%dgi", arg+7, arg, arg+7, arg);
        endif
      endif
    else
      ## Setup a suitable format string
      if (isnumeric (x) && x == fix (x) && abs (x) < 1e10)
        if (any (real (x(:)) != 0))
          dgt1 = ceil (log10 (max (max (abs (real (x(:)))),
                                   max (abs (imag (x(:))))))) + 2;
        else
          dgt1 = 2;
        endif
        dgt2 = dgt1 - (any (real (x(:)) >= 0));

        if (any (abs (x(:)) > intmax ("int32")))
          fmt = sprintf("%%%dg%%+-%dgi  ", dgt2, dgt1);
        else
          fmt = sprintf("%%%dd%%+-%ddi  ", dgt2, dgt1);
        endif
      elseif (isscalar (x))
        fmt = "%.6g%-+.6gi";
      else
        fmt = "%11.6g%-+11.6gi";
      endif
    endif

    ## Manipulate the complex value to have real values in the odd
    ## columns and imaginary values in the even columns.
    nc = columns (x);
    nd = ndims (x);
    perm = fix ([1:0.5:nc+0.5]);
    perm(2:2:2*nc) = perm(2:2:2*nc) + nc;
    idx = repmat ({':'}, nd, 1);
    idx{2} = perm;
    x = horzcat (real (x), imag (x));
    x = x(idx{:});

    fmt = cstrcat (deblank (repmat (fmt, 1, nc)), "\n");
    tmp = sprintf (fmt, permute (x, [2, 1, 3:nd]));

    ## Put the "i"'s where they are supposed to be.
    while (true)
      tmp2 = strrep (tmp, " i\n", "i\n");
      if (length (tmp) == length (tmp2))
        break;
      else
        tmp = tmp2;
      endif
    endwhile
    while (true)
      tmp2 = strrep (tmp, " i", "i ");
      if (tmp == tmp2)
        break;
      else
        tmp = tmp2;
      endif
    endwhile

    tmp(end) = "";
    retval = strtrim (char (strsplit (tmp, "\n")));
  endif

endfunction


%!assert (num2str (123), "123")
%!assert (num2str (1.23), "1.23")
%!assert (num2str (123.456, 4), "123.5")
%!assert (num2str ([1, 1.34; 3, 3.56], "%5.1f"),  ["1.0  1.3"; "3.0  3.6"])
%!assert (num2str (1.234 + 27.3i), "1.234+27.3i")

%!assert (num2str (19440606), "19440606")
%!assert (num2str (2^33), "8.58993e+09")
%!assert (num2str (-2^33), "-8.58993e+09")
%!assert (num2str (2^33+1i), "8.58993e+09+1i")
%!assert (num2str (-2^33+1i), "-8.58993e+09+1i")

%!error num2str ()
%!error num2str (1, 2, 3)

