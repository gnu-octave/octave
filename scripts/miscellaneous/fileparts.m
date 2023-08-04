########################################################################
##
## Copyright (C) 2003-2023 The Octave Project Developers
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
## @deftypefn {} {[@var{dir}, @var{name}, @var{ext}] =} fileparts (@var{filename})
## Return the directory, name, and extension components of @var{filename}.
##
## The input @var{filename} is a string which is parsed.  There is no attempt
## to check whether the filename or directory specified actually exists.
## @seealso{fullfile, filesep}
## @end deftypefn

function [dir, name, ext] = fileparts (filename)

  if (nargin < 1)
    print_usage ();
  endif

  if (! ischar (filename) || rows (filename) > 1)
    error ("fileparts: FILENAME must be a single string");
  endif

  ds = strchr (filename, filesep ("all"), 1, "last");
  if (isempty (ds))
    ds = 0;
  endif
  es = rindex (filename, ".");
  ## These can be the same if they are both 0 (no dir or ext).
  if (es <= ds)
    es = length (filename)+1;
  endif

  if (ds == 0)
    if (ispc () && length (filename) >= 2 && strcmp (filename(2), ":"))
      ## Relative path on Windows drive. At least, fix file name.
      ds = 2;
      dir = filename(1:2);
    else
      dir = "";
    endif
  elseif (ds == 1)
    dir = filename(1);
  else
    dir = filename(1:ds-1);
  endif

  name = filename(ds+1:es-1);
  if (isempty (name))
    name = "";
  endif

  if (es > 0 && es <= length (filename))
    ext = filename(es:end);
  else
    ext = "";
  endif

endfunction


%!test
%! [d, n, e] = fileparts ("file");
%! assert (d, "");
%! assert (n, "file");
%! assert (e, "");

%!test
%! [d, n, e] = fileparts ("file.ext");
%! assert (d, "");
%! assert (n, "file");
%! assert (e, ".ext");

%!test
%! [d, n, e] = fileparts ("/file.ext");
%! assert (d, "/");
%! assert (n, "file");
%! assert (e, ".ext");

%!test
%! [d, n, e] = fileparts ("dir/file.ext");
%! assert (d, "dir");
%! assert (n, "file");
%! assert (e, ".ext");

%!test
%! [d, n, e] = fileparts ("./file.ext");
%! assert (d, ".");
%! assert (n, "file");
%! assert (e, ".ext");

%!test
%! [d, n, e] = fileparts ("d1/d2/file.ext");
%! assert (d, "d1/d2");
%! assert (n, "file");
%! assert (e, ".ext");

%!test
%! [d, n, e] = fileparts ("/d1/d2/file.ext");
%! assert (d, "/d1/d2");
%! assert (n, "file");
%! assert (e, ".ext");

%!test
%! [d, n, e] = fileparts ("/.ext");
%! assert (d, "/");
%! assert (n, "");
%! assert (e, ".ext");

%!test
%! [d, n, e] = fileparts (".ext");
%! assert (d, "");
%! assert (n, "");
%! assert (e, ".ext");

%!test
%! [d, n, e] = fileparts ("a");
%! assert (d, "");
%! assert (n, "a");
%! assert (e, "");

%!test
%! [d, n, e] = fileparts ("");
%! assert (d, "");
%! assert (n, "");
%! assert (e, "");

%!testif ; ispc () <*64462>
%! [d, n, e] = fileparts ("c:file.ext");
%! assert (d, "c:");
%! assert (n, "file");
%! assert (e, ".ext");

%!testif ; ispc () <*64462>
%! [d, n, e] = fileparts ("c:d1/../d2/file.ext");
%! assert (d, "c:d1/../d2");
%! assert (n, "file");
%! assert (e, ".ext");

%!testif ; ispc ()
%! [d, n, e] = fileparts ('c:\dir\file.ext');
%! assert (d, 'c:\dir');
%! assert (n, "file");
%! assert (e, ".ext");

## Test input validation
%!error <Invalid call> fileparts ()
%!error <FILENAME must be a single string> fileparts (1)
%!error <FILENAME must be a single string> fileparts (["a"; "b"])
