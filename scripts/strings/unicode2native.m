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
## @deftypefn  {} {@var{native_bytes} =} unicode2native (@var{utf8_str}, @var{codepage})
## @deftypefnx {} {@var{native_bytes} =} unicode2native (@var{utf8_str})
## Convert UTF-8 string @var{utf8_str} to byte stream using @var{codepage}.
##
## The character vector @var{utf8_str} is converted to a byte stream
## @var{native_bytes} using the code page given by @var{codepage}.  The
## string @var{codepage} must be an identifier of a valid code page.
## Examples for valid code pages are @qcode{"ISO-8859-1"},
## @qcode{"Shift-JIS"}, or @qcode{"UTF-16"}.  For a list of supported code
## pages, see @url{https://www.gnu.org/software/libiconv}.  If @var{codepage}
## is omitted or empty, the system default codepage is used.
##
## If any of the characters cannot be mapped into the codepage @var{codepage},
## they are replaced with the appropriate substitution sequence for that
## codepage.
##
## @seealso{native2unicode}
## @end deftypefn

function native_bytes = unicode2native (utf8_str, codepage = "")

  if (nargin < 1)
    print_usage ();
  endif

  ## For Matlab compatibility, return empty output for empty input.
  if (isempty (utf8_str))
    native_bytes = uint8 ([]);
    return;
  endif

  if (! ischar (utf8_str) || ! isvector (utf8_str))
    error ("unicode2native: UTF8_STR must be a character vector");
  endif

  if (! (ischar (codepage) && isrow (codepage)))
    error ("unicode2native: CODEPAGE must be a string");
  endif

  native_bytes = __unicode2native__ (utf8_str, codepage);

  if (iscolumn (utf8_str))
    native_bytes = native_bytes.';
  endif

endfunction


%!testif HAVE_ICONV
%! assert (unicode2native ("ЄЅІЇЈЉЊ", "ISO-8859-5"), uint8 (164:170));
%!testif HAVE_ICONV
%! assert (unicode2native (["ЄЅІ" "\0" "ЇЈЉЊ"], "ISO-8859-5"),
%!         uint8 ([164:166 0 167:170]));
%!assert <*60480> (unicode2native (''), uint8 ([]))

# short character arrays with invalid UTF-8
%!testif HAVE_ICONV <*63930>
%! assert (unicode2native (char (230), 'windows-1252'), uint8 (63));
%! assert (unicode2native (char (249), 'windows-1252'), uint8 (63));
%! assert (unicode2native (char (230:231), 'windows-1252'), uint8 ([63, 63]));
%! assert (unicode2native (char (230:234), 'windows-1252'),
%!         uint8 ([63, 63, 63, 63, 63]));
%! assert (unicode2native (char ([230, 10]), 'windows-1252'),
%!         uint8 ([63, 10]));

%!error <Invalid call> unicode2native ()
%!error <called with too many inputs> unicode2native ('a', 'ISO-8859-1', 'test')
%!error <UTF8_STR must be a character vector> unicode2native (['ab'; 'cd'])
%!error <UTF8_STR must be a character vector> unicode2native ({1 2 3 4})
%!error <CODEPAGE must be a string> unicode2native ('ЄЅІЇЈЉЊ', 123)
%!error <CODEPAGE must be a string> unicode2native ('ЄЅІЇЈЉЊ', ['ISO-8859-1']')
%!testif HAVE_ICONV
%! fail ("unicode2native ('a', 'foo')",
%!       "converting from UTF-8 to codepage 'foo'");
