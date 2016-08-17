## Copyright (C) 2007-2016 David Bateman
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
## @deftypefn  {} {} md5sum (@var{file})
## @deftypefnx {} {} md5sum (@var{str}, @var{opt})
##
## @code{md5sum} is deprecated and will be removed in Octave version 4.6.
## For equivalent functionality replace calls like @code{md5sum (@var{file})}
## with:
##
## @example
## hash (\"md5\", fileread (@var{file}))
## @end example
##
## And calls like @code{md5sum (@var{str}, true)} with:
##
## @example
## hash (\"md5\", fileread (@var{str}))
## @end example
##
## Calculate the MD5 sum of the file @var{file}.
##
## If the second parameter @var{opt} exists and is true, then calculate the MD5
## sum of the string @var{str}.
##
## @seealso{hash, fileread}
## @end deftypefn

function r = md5sum (str, opt)

  persistent warned = false;
  if (! warned)
    warned = true;
    warning ("Octave:deprecated-function",
             "md5sum is obsolete and will be removed from a future version of Octave, please use hash instead");
  endif

  if (nargin == 1)
    r = hash ("md5", fileread (str));
  elseif ((nargin == 2) && isbool (opt) && isscalar (opt) && (opt == true))
    r = hash ("md5", str);
  else
    print_usage ();
  endif

endfunction


%!assert (md5sum ("abc\0", true), "147a664a2ca9410911e61986d3f0d52a")

%!test
%! tfile = tempname ();
%! fid = fopen (tfile, "wb");
%! fwrite (fid, "abc\0");
%! fclose (fid);
%! assert (md5sum (tfile), "147a664a2ca9410911e61986d3f0d52a");
%! unlink (tfile);

%!error md5sum ()

