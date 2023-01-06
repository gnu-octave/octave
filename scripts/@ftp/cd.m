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
## @deftypefn  {} {@var{cwd} =} cd (@var{f})
## @deftypefnx {} {} cd (@var{f}, @var{path})
## @deftypefnx {} {@var{new_cwd} =} cd (@var{f}, @var{path})
## Get or set the remote directory on the FTP connection @var{f}.
##
## @var{f} is an FTP object returned by the @code{ftp} function.
##
## If @var{path} is not specified, return the remote current working
## directory.  Otherwise, set the remote directory to @var{path} and return
## the new remote working directory.
##
## If the directory does not exist, an error message is printed and the
## working directory is not changed.
## @seealso{@ftp/dir, @ftp/ftp}
## @end deftypefn

function path = cd (f, path)

  if (nargin == 2)
    __ftp_cwd__ (f.curlhandle, path);
  endif

  path = __ftp_pwd__ (f.curlhandle);

endfunction


## No test possible for interactive function.
%!assert (1)
