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
## @deftypefn {} {@var{tf} =} isfolder (@var{f})
## Return true if @var{f} is a directory and false otherwise.
##
## If @var{f} is a cell array of strings, @var{tf} is a logical array of the
## same size.
## @seealso{isfile, exist, stat, is_absolute_filename,
## is_rooted_relative_filename}
## @end deftypefn

function tf = isfolder (f)

  if (nargin < 1)
    print_usage ();
  endif

  if (! (ischar (f) || iscellstr (f)))
    error ("isfolder: F must be a string or cell array of strings");
  endif

  f = cellstr (f);
  tf = false (size (f));
  for i = 1:numel (f)
    [info, err] = stat (f{i});
    tf(i) = (! err && S_ISDIR (info.mode));
  endfor

endfunction


%!assert (isfolder (pwd ()))
%!assert (! isfolder (tempname ()))
%!assert (! isfolder (which ("isfolder")))
%!assert (isfolder ( {pwd(), tempname()}), [true, false])
%!assert (isfolder ( {pwd(); tempname()}), [true; false])

%!test
%! unwind_protect
%!   tmp = tempname ();
%!   [d, n] = fileparts (tmp);
%!   assert (! isfolder (tmp));
%!   mkdir (tmp);
%!   assert (isfolder (tmp));
%!   assert (! isfolder (n));
%!   addpath (d);
%!   assert (! isfolder (n));
%! unwind_protect_cleanup
%!   sts = rmdir (tmp);
%!   rmpath (d);
%! end_unwind_protect

## Test input validation
%!error <Invalid call> isfolder ()
%!error isfolder ("a", "b")
%!error <F must be a string> isfolder (1.0)
