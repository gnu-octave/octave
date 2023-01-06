########################################################################
##
## Copyright (C) 2018-2023 The Octave Project Developers
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
## @deftypefn {} {@var{tf} =} isfile (@var{f})
## Return true if @var{f} is a regular file and false otherwise.
##
## If @var{f} is a cell array of strings, @var{tf} is a logical array of the
## same size.
## @seealso{isfolder, exist, stat, is_absolute_filename,
## is_rooted_relative_filename}
## @end deftypefn

function retval = isfile (f)

  if (nargin < 1)
    print_usage ();
  endif

  if (! (ischar (f) || iscellstr (f)))
    error ("isfile: F must be a string or cell array of strings");
  endif

  f = cellstr (f);
  retval = false (size (f));
  for i = 1:numel (f)
    [info, err] = stat (f{i});
    retval(i) = (! err && S_ISREG (info.mode));
  endfor

endfunction


%!shared mfile
%! mfile = which ("isfile");

%!assert (isfile (mfile))
%!assert (! isfile (tempdir ()))
%!assert (isfile ({mfile, pwd()}), [true, false])
%!assert (isfile ({mfile; pwd()}), [true; false])

%!test
%! unwind_protect
%!   tmp = tempname ();
%!   [d, n] = fileparts (tmp);
%!   assert (! isfile (tmp));
%!   save ("-text", tmp, "tmp");  # cheap way to create a file
%!   assert (isfile (tmp));
%!   addpath (d);
%!   assert (! isfile (n));
%! unwind_protect_cleanup
%!   try, unlink (tmp); end_try_catch
%!   try, rmpath (d); end_try_catch
%! end_unwind_protect

## Test input validation
%!error <Invalid call> isfile ()
%!error isfile ("a", "b")
%!error <F must be a string> isfile (1.0)
