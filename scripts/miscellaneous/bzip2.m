## Copyright (C) 2008-2012 Thorsten Meyer
## (based on gzip.m by David Bateman)
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
## @deftypefn  {Function File} {@var{entries} =} bzip2 (@var{files})
## @deftypefnx {Function File} {@var{entries} =} bzip2 (@var{files}, @var{outdir})
## Compress the list of files specified in @var{files}.
## Each file is compressed separately and a new file with a '.bz2' extension
## is created.  The original files are not modified.  Existing compressed files
## are silently overwritten.  If @var{outdir} is defined the compressed
## files are placed in this directory.
## @seealso{bunzip2, gzip, zip, tar}
## @end deftypefn

function entries = bzip2 (varargin)

  if (nargin == 1 || nargin == 2)
    if nargout == 0
      __xzip__ ("bzip2", "bz2", "bzip2 %s", varargin{:});
    else
      entries = __xzip__ ("bzip2", "bz2", "bzip2 %s", varargin{:});
    endif
  else
    print_usage ();
  endif

endfunction

%!xtest
%!  # test for correct cleanup of temporary files
%!  unwind_protect
%!    filename = tmpnam;
%!    dummy    = 1;
%!    save(filename, "dummy");
%!    n_tmpfiles_before = length(find(strncmp("oct-", cellstr(ls(P_tmpdir)), 4)));
%!    entry = bzip2(filename);
%!    n_tmpfiles_after = length(find(strncmp("oct-", cellstr(ls(P_tmpdir)), 4)));
%!    if (n_tmpfiles_before != n_tmpfiles_after)
%!      error("bzip2 has not cleaned up temporary files correctly!");
%!    endif
%!  unwind_protect_cleanup
%!    delete(filename);
%!    [path, basename, extension] = fileparts(filename);
%!    delete([basename, extension, ".bz2"]);
%!  end_unwind_protect
