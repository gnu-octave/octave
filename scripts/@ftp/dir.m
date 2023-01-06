########################################################################
##
## Copyright (C) 2009-2023 The Octave Project Developers
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
## @deftypefn  {} {} dir (@var{f})
## @deftypefnx {} {@var{lst} =} dir (@var{f})
## List the current directory in verbose form for the FTP connection @var{f}.
##
## @var{f} is an FTP object returned by the @code{ftp} function.
##
## If the optional output @var{lst} is requested return a struct array
## with one entry per file with the fields @code{name}, @code{date},
## @code{bytes}, @code{isdir}, @code{datenum}.
## @seealso{@ftp/cd, @ftp/mkdir, @ftp/rmdir, @ftp/ftp}
## @end deftypefn

function lst = dir (f)
  if (nargout == 0)
    __ftp_dir__ (f.curlhandle);
  else
    lst = __ftp_dir__ (f.curlhandle);
  endif
endfunction


## No test possible for interactive function.
%!assert (1)
