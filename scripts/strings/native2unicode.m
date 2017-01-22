## Copyright (C) 2016 Markus Mützel
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or
## (at your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn  {} {@var{utf8_str} =} native2unicode (@var{native_bytes}, @var{codepage})
## @deftypefnx {} {@var{utf8_str} =} native2unicode (@var{native_bytes})
## Convert byte stream @var{native_bytes} to UTF-8 using @var{codepage}.
##
## The numbers in the vector @var{native_bytes} are rounded and clipped to
## integers between 0 and 255.  This byte stream is then mapped into the
## codepage given by the string @var{codepage} and returned in the string
## @var{utf8_str}.  Octave uses UTF-8 as its internal encoding.
## The string @var{codepage} must be an identifier of a valid codepage.
## Examples for valid codepages are "ISO 8859-1", "Latin-1" or "Shift-JIS".
## If @var{codepage} is omitted or empty, the system default codepage is used.
##
## If @var{native_bytes} is a string vector, it is returned as is.
##
## @seealso{unicode2native}
## @end deftypefn

function utf8_str = native2unicode (native_bytes, codepage)

  ## check input
  if (nargin < 1 || nargin > 2)
    print_usage ();
  endif

  if (ischar (native_bytes))
    utf8_str = native_bytes;
    return
  endif

  if (! isnumeric (native_bytes) || ! isvector (native_bytes))
    error ("native2unicode: NATIVE_BYTES must be a numeric vector.")
  endif

  is_column = false;
  if (! isrow (native_bytes))
    is_column = true;
    native_bytes = native_bytes';
  endif

  if (nargin < 2 || isempty (codepage))
    codepage = 0;
  endif

  if (! ischar (codepage) && codepage != 0)
    error ("native2unicode: CODEPAGE must be a string or 0.")
  endif

  native_bytes = round (native_bytes);
  native_bytes(native_bytes < 0) = 0;
  native_bytes(native_bytes > 255) = 255;

  ## pass to internal function
  utf8_str = __native2unicode__ (native_bytes, codepage);

  if (is_column)
    utf8_str = utf8_str';
  endif

endfunction

%!testif(HAVE_LIBUNISTRING)
%! assert (double (native2unicode (164:170, 'ISO 8859-5')),
%!         [208 132 208 133 208 134 208 135 208 136 208 137 208 138]);  # "ЄЅІЇЈЉЊ"
%!testif(HAVE_LIBUNISTRING)
%! assert (double (native2unicode ([164:166 0 167:170], 'ISO 8859-5')),
%!         [208 132 208 133 208 134 0 208 135 208 136 208 137 208 138]);  # ["ЄЅІ" 0 "ЇЈЉЊ"]
%!testif(HAVE_LIBUNISTRING)
%! assert (native2unicode ("foobar"), "foobar");
%!testif(HAVE_LIBUNISTRING)
%! assert (double (native2unicode ([0 0 120.3 0 0 122.6 0 0])),
%!         [0 0 120 0 0 123 0 0]);
%!testif(HAVE_LIBUNISTRING)
%! fail ("native2unicode ([1 2; 3 4])", "NATIVE_BYTES must be a numeric vector");
%!testif(HAVE_LIBUNISTRING)
%! fail ("native2unicode ({1 2 3 4})", "NATIVE_BYTES must be a numeric vector");
%!testif(HAVE_LIBUNISTRING)
%! fail ("native2unicode (164:170, 123)", "CODEPAGE must be a string or 0");
%!testif(HAVE_LIBUNISTRING)
%! fail ("native2unicode (234, 'foo')", 
%!       "Error .* converting from codepage 'foo' to UTF-8");
%!testif(HAVE_LIBUNISTRING)
%! fail ("native2unicode ()", "Invalid call");
%!testif(HAVE_LIBUNISTRING)
%! fail ("native2unicode (1, 'Latin-1', 'test')", "Invalid call");
 