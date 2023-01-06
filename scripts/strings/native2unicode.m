########################################################################
##
## Copyright (C) 2016-2023 The Octave Project Developers
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
## @deftypefn  {} {@var{utf8_str} =} native2unicode (@var{native_bytes}, @var{codepage})
## @deftypefnx {} {@var{utf8_str} =} native2unicode (@var{native_bytes})
## Convert byte stream @var{native_bytes} to UTF-8 using @var{codepage}.
##
## The numbers in the vector @var{native_bytes} are rounded and clipped to
## integers between 0 and 255.  This byte stream is then mapped into the
## code page given by the string @var{codepage} and returned in the string
## @var{utf8_str}.  Octave uses UTF-8 as its internal encoding.  The string
## @var{codepage} must be an identifier of a valid code page.  Examples for
## valid code pages are @qcode{"ISO-8859-1"}, @qcode{"Shift-JIS"}, or
## @qcode{"UTF-16"}.  For a list of supported code pages, see
## @url{https://www.gnu.org/software/libiconv}.  If @var{codepage} is omitted
## or empty, the system default codepage is used.
##
## If @var{native_bytes} is a string vector, it is returned as is.
##
## @seealso{unicode2native}
## @end deftypefn

function utf8_str = native2unicode (native_bytes, codepage = "")

  if (nargin < 1)
    print_usage ();
  endif

  if (ischar (native_bytes))
    utf8_str = native_bytes;
    return;
  endif

  if (! isnumeric (native_bytes) || ! isvector (native_bytes))
    error ("native2unicode: NATIVE_BYTES must be a numeric vector");
  endif

  if (nargin == 2 && ! (ischar (codepage) && isrow (codepage)))
    error ("native2unicode: CODEPAGE must be a string");
  endif

  ## Convert to uint8 which rounds and clips values to range [0, 255]3
  native_bytes = uint8 (native_bytes);

  utf8_str = __native2unicode__ (native_bytes, codepage);

  if (! isrow (native_bytes))
    utf8_str = utf8_str.';
  endif

endfunction


## "ЄЅІЇЈЉЊ"
%!testif HAVE_ICONV
%! assert (double (native2unicode (164:170, 'ISO-8859-5')),
%!         [208 132 208 133 208 134 208 135 208 136 208 137 208 138]);
## ["ЄЅІ" 0 "ЇЈЉЊ"]
%!testif HAVE_ICONV
%! assert (double (native2unicode ([164:166 0 167:170], 'ISO-8859-5')),
%!         [208 132 208 133 208 134 0 208 135 208 136 208 137 208 138]);

%!assert (native2unicode ("foobar"), "foobar")
%!assert <*54384> (double (native2unicode ([0 0 120.3 0 0 122.6 0 0])),
%!                 [0 0 120 0 0 123 0 0])

%!error <Invalid call> native2unicode ()
%!error <called with too many inputs> native2unicode (1, 'ISO-8859-1', 'test')
%!error <NATIVE_BYTES must be a numeric vector> native2unicode ([1 2; 3 4])
%!error <NATIVE_BYTES must be a numeric vector> native2unicode ({1 2 3 4})
%!error <CODEPAGE must be a string> native2unicode (164:170, 123)
%!error <CODEPAGE must be a string> native2unicode (164:170, ['ISO-8859-1']')
%!testif HAVE_ICONV
%! fail ("native2unicode (234, 'foo')",
%!       "converting from codepage 'foo' to UTF-8");
