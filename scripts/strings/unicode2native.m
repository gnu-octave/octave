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
## @deftypefn  {} {@var{native_bytes} =} unicode2native (@var{utf8_str}, @var{codepage})
## @deftypefnx {} {@var{native_bytes} =} unicode2native (@var{utf8_str})
## Convert UTF-8 string @var{utf8_str} to byte stream using @var{codepage}.
##
## The character vector @var{utf8_str} is converted to a byte stream
## @var{native_bytes} using the codepage given by @var{codepage}.
## The string @var{codepage} must be an identifier of a valid codepage.
## Examples for valid codepages are "ISO 8859-1", "Latin-1" or "Shift-JIS".
## If @var{codepage} is omitted or empty, the system default codepage is used.
##
## If any of the characters cannot be mapped into the codepage @var{codepage},
## they are replaced with the appropriate substitution sequence for that
## codepage.
##
## @seealso{native2unicode}
## @end deftypefn

function native_bytes = unicode2native (utf8_str, codepage)

  ## check input
  if (nargin < 1 || nargin > 2)
    print_usage ();
  endif


  if (! ischar (utf8_str) || ! isvector (utf8_str))
    error ("unicode2native: UTF8_STR must be a character vector.")
  endif

  is_column = false;
  if (! isrow (utf8_str))
    is_column = true;
    utf8_str = utf8_str';
  endif

  if (nargin < 2 || isempty (codepage))
    codepage = 0;
  endif

  if (! ischar (codepage) && codepage != 0)
    error ("unicode2native: CODEPAGE must be a string or 0.")
  endif

  ## pass to internal function
  native_bytes = __unicode2native__ (utf8_str, codepage);

  if (is_column)
    native_bytes = native_bytes';
  endif

endfunction

%!testif(HAVE_LIBUNISTRING)
%! assert (unicode2native ("ЄЅІЇЈЉЊ", "ISO 8859-5"), 164:170);
%!testif(HAVE_LIBUNISTRING)
%! assert (unicode2native (["ЄЅІ" 0 "ЇЈЉЊ"], "ISO 8859-5"), [164:166 0 167:170]);
%!testif(HAVE_LIBUNISTRING)
%! fail ("unicode2native (['ab'; 'cd'])", "UTF8_STR must be a character vector");
%!testif(HAVE_LIBUNISTRING)
%! fail ("unicode2native ({1 2 3 4})", "UTF8_STR must be a character vector");
%!testif(HAVE_LIBUNISTRING)
%! fail ("unicode2native ('ЄЅІЇЈЉЊ', 123)", "CODEPAGE must be a string or 0");
%!testif(HAVE_LIBUNISTRING)
%! fail ("unicode2native ('a', 'foo')",
%!       "Error .* converting from UTF-8 to codepage 'foo'");
%!testif(HAVE_LIBUNISTRING)
%! fail ("unicode2native ()", "Invalid call");
%!testif(HAVE_LIBUNISTRING)
%! fail ("unicode2native ('a', 'Latin-1', 'test')", "Invalid call");
 