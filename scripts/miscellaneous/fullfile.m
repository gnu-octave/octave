## Copyright (C) 2003-2012 John W. Eaton
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
## @deftypefn {Function File} {@var{filename} =} fullfile (@var{dir1}, @var{dir2}, @dots{}, @var{file})
## Return a complete filename constructed from the given components.
## @seealso{fileparts}
## @end deftypefn

function filename = fullfile (varargin)

  if (nargin > 0)
    ## Discard all empty arguments
    varargin(cellfun ("isempty", varargin)) = [];
    nargs = numel (varargin);
    if (nargs > 1)
      filename = varargin{1};
      if (strcmp (filename(end), filesep))
        filename(end) = "";
      endif
      for i = 2:nargs
        tmp = varargin{i};
        if (i < nargs && strcmp (tmp(end), filesep))
          tmp(end) = "";
        elseif (i == nargs && strcmp (tmp, filesep))
          tmp = "";
        endif
        filename = cstrcat (filename, filesep, tmp);
      endfor
    elseif (nargs == 1)
      filename = varargin{1};
    else
      filename = "";
    endif
  else
    print_usage ();
  endif

endfunction

%!shared fs, fsx, xfs, fsxfs, xfsy
%! fs = filesep ();
%! fsx = cstrcat (fs, "x");
%! xfs = cstrcat ("x", fs);
%! fsxfs = cstrcat (fs, "x", fs);
%! xfsy = cstrcat ("x", fs, "y");
%!assert (fullfile (""), "")
%!assert (fullfile (fs), fs)
%!assert (fullfile ("", fs), fs)
%!assert (fullfile (fs, ""), fs)
%!assert (fullfile ("", fs), fs)
%!assert (fullfile ("x"), "x")
%!assert (fullfile ("", "x"), "x")
%!assert (fullfile ("x", ""), "x")
%!assert (fullfile ("", "x", ""), "x")
%!assert (fullfile ("x", "y"), xfsy)
%!assert (fullfile ("x", "", "y"), xfsy)
%!assert (fullfile ("x", "", "y", ""), xfsy)
%!assert (fullfile ("", "x", "", "y", ""), xfsy)
%!assert (fullfile (fs), fs)
%!assert (fullfile (fs, fs), fs)
%!assert (fullfile (fs, "x"), fsx)
%!assert (fullfile (fs, xfs), fsxfs)
%!assert (fullfile (fsx, fs), fsxfs)
%!assert (fullfile (fs, "x", fs), fsxfs)
