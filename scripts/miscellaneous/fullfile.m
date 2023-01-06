########################################################################
##
## Copyright (C) 2014-2023 The Octave Project Developers
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
## @deftypefn {} {@var{filename} =} fullfile (@var{dir1}, @var{dir2}, @dots{}, @var{file})
## Build complete filename from separate parts.
##
## The function joins any number of path components intelligently.  The return
## value is the concatenation of each component with exactly one file separator
## between each part of the path and at most one leading and/or trailing file
## separator.
##
## The input arguments might be strings or cell strings.  Any input arguments
## that are cell strings must contain one single string or must be equal in
## size.  In that case, the function returns a cell string of filepaths of the
## same dimensions as the input cell strings, e.g.:
##
## @example
## @group
## fullfile ("/home/username", "data", @{"f1.csv", "f2.csv", "f3.csv"@})
##   @result{}
##       @{
##         [1,1] = /home/username/data/f1.csv
##         [1,2] = /home/username/data/f2.csv
##         [1,3] = /home/username/data/f3.csv
##       @}
## @end group
## @end example
##
## On Windows systems, while forward slash file separators do work, they are
## replaced by backslashes.  In addition, drive letters are stripped of leading
## file separators to obtain a valid file path.
##
## Note: @code{fullfile} does not perform any validation of the resulting full
## filename.
## @seealso{fileparts, filesep}
## @end deftypefn

function filename = fullfile (varargin)

  ## remove empty arguments
  varargin(cellfun (@isempty, varargin)) = [];

  if (isempty (varargin))
    ## return early for all empty or no input
    filename = "";
    return;
  endif

  ## check input type
  is_cellstr = cellfun (@iscellstr, varargin);
  if (! all (is_cellstr | cellfun (@ischar, varargin)))
    error ("fullfile: input must either be strings or cell strings");
  endif

  ## convert regular strings to cell strings
  varargin(! is_cellstr) = num2cell (varargin(! is_cellstr));

  ## check if input size matches
  if (numel (varargin) > 1 && common_size (varargin{:}) != 0)
    error ("fullfile: cell string input must be scalar or of the same size");
  endif

  fs = filesep ();

  if (ispc ())
    ## replace forward slashes with backslashes
    varargin = cellfun (@(x) strrep (x, "/", fs), varargin,
                        "UniformOutput", false);

    ## Strip fileseps preceeding drive letters
    varargin{1} = regexprep (varargin{1}, '\\*([a-zA-Z]:\\*)', "$1");

    unc = strncmp (varargin{1}, '\\', 2);
  endif

  ## insert file separator between elements
  varargin(2,:) = {fs};
  varargin{end} = "";

  filename = strcat (varargin{:});

  ## remove multiplicate file separators
  filename = regexprep (filename, [undo_string_escapes(fs), "*"], fs);

  if (ispc ())
    ## prepend removed file separator for UNC paths
    filename(unc) = strcat (fs, filename(unc));
  endif

  if (! any (is_cellstr))
    filename = filename{1};
  endif

endfunction


%!shared fs, fsx, xfs, fsxfs, xfsy, xfsyfs
%! fs = filesep ();
%! fsx = [fs, "x"];
%! xfs = ["x", fs];
%! fsxfs = [fs, "x", fs];
%! xfsy = ["x", fs, "y"];
%! xfsyfs = ["x", fs, "y", fs];

%!assert (fullfile (""), "")
%!assert (fullfile ("", ""), "")
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
%!assert (fullfile (fs, "x"), fsx)
%!assert (fullfile (fs, xfs), fsxfs)
%!assert (fullfile (fsx, fs), fsxfs)
%!assert (fullfile (fs, "x", fs), fsxfs)

%!assert (fullfile ("x/", "/", "/", "y", "/", "/"), xfsyfs)
%!assert (fullfile ("/", "x/", "/", "/", "y", "/", "/"), [fs, xfsyfs])
%!assert (fullfile ("/x/", "/", "/", "y", "/", "/"), [fs, xfsyfs])

## different on purpose so that "fullfile (c{:})" works for empty c
%!assert (fullfile (), "")

%!assert (fullfile ("x", "y", {"c", "d"}), {[xfsyfs, "c"], [xfsyfs, "d"]})
%!assert (fullfile ({"folder1", "folder2"}, "sub", {"f1.m", "f2.m"}), ...
%!        {["folder1", fs, "sub", fs, "f1.m"], ...
%!         ["folder2", fs, "sub", fs, "f2.m"]});

## Windows specific - drive letters and file sep type
%!testif ; ispc ()
%! assert (fullfile ('\/\/\//A:/\/\', "x/", "/", "/", "y", "/", "/"), ...
%!         ['A:\' xfsyfs]);

## *nix specific - double backslash
%!testif ; ! ispc ()
%! assert (fullfile (fs, fs), fs);

## Windows specific - UNC path
%!testif ; ispc ()
%! assert (fullfile ({'\/\//server1', 'C:', '\\server2\/'}, ...
%!                   "x/", "/", "/", "y", "/", "/"), ...
%!         {['\\server1\', xfsyfs], ['C:\', xfsyfs], ['\\server2\', xfsyfs]});

## Windows specific - drive letters and file sep type, cell array
%!testif ; ispc ()
%! tmp = fullfile ({'\\\/B:\//', 'A://c', '\\\C:/g/h/i/j\/'});
%! assert (tmp{1}, 'B:\');
%! assert (tmp{2}, 'A:\c');
%! assert (tmp{3}, 'C:\g\h\i\j\');

%!error <strings or cell strings> fullfile (1)
%!error <strings or cell strings> fullfile ({1})
%!error <same size> fullfile ({"a", "b"}, {"a", "b", "c"})
