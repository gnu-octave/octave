## Copyright (C) 2014 Carnë Draug
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
## Build complete filename from separate parts.
##
## Joins any number of path components intelligently.  The return value
## is the concatenation of each component with exactly one file separator
## between each non empty part.
##
## @seealso{fileparts}
## @end deftypefn

## Author: Carnë Draug <carandraug@octave.org>

function filename = fullfile (varargin)

  non_empty = cellfun ("isempty", varargin);
  filename = strjoin (varargin(! non_empty), filesep);
  filename(strfind (filename, [filesep filesep])) = "";

endfunction


%!shared fs, fsx, xfs, fsxfs, xfsy
%! fs = filesep ();
%! fsx = [fs "x"];
%! xfs = ["x" fs];
%! fsxfs = [fs "x" fs];
%! xfsy = ["x" fs "y"];
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

%!assert (fullfile ("a/", "/", "/", "b", "/", "/"), "a/b/")
%!assert (fullfile ("/", "a/", "/", "/", "b", "/", "/"), "/a/b/")
%!assert (fullfile ("/a/", "/", "/", "b", "/", "/"), "/a/b/")

## different on purpose so that "fullfile (c{:})" works for empty c
%!assert (fullfile (), "")
