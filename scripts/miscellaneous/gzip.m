## Copyright (C) 2007-2012 David Bateman
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
## @deftypefn  {Function File} {@var{entries} =} gzip (@var{files})
## @deftypefnx {Function File} {@var{entries} =} gzip (@var{files}, @var{outdir})
## Compress the list of files and/or directories specified in @var{files}.
## Each file is compressed separately and a new file with a '.gz' extension
## is created.  The original files are not modified.  Existing compressed
## files are silently overwritten.  If @var{outdir} is defined the compressed
## files are placed in this directory.
## @seealso{gunzip, bzip2, zip, tar}
## @end deftypefn

function entries = gzip (varargin)
  if (nargin != 1 && nargin != 2) || (nargout > 1)
    print_usage ();
  endif

  if (nargout == 0)
    __xzip__ ("gzip", "gz", "gzip -r %s", varargin{:});
  else
    entries = __xzip__ ("gzip", "gz", "gzip -r %s", varargin{:});
  endif

endfunction

%!error <Invalid call to gzip.  Correct usage is> gzip("1", "2", "3");
%!error <Invalid call to gzip.  Correct usage is> gzip();
%!error <output directory does not exist> gzip("1", tmpnam);
%!error <FILES must be a character array or cellstr> gzip(1);
%!xtest
%!  # test gzip together with gunzip
%!  unwind_protect
%!    filename = tmpnam;
%!    dummy    = 1;
%!    save(filename, "dummy");
%!    dirname  = tmpnam;
%!    mkdir(dirname);
%!    entry = gzip(filename, dirname);
%!    [path, basename, extension] = fileparts(filename);
%!    if ! strcmp(entry, [dirname, filesep, basename, extension, ".gz"])
%!      error("gzipped file does not match expected name!");
%!    endif
%!    if ! exist(entry, "file")
%!      error("gzipped file cannot be found!");
%!    endif
%!    gunzip(entry);
%!    if (system(sprintf("diff %s %s%c%s%s", filename, dirname, filesep,
%!                                          basename, extension)))
%!      error("unzipped file not equal to original file!");
%!    end
%!  unwind_protect_cleanup
%!    delete(filename);
%!    delete([dirname, filesep, basename, extension]);
%!    rmdir(dirname);
%!  end_unwind_protect
