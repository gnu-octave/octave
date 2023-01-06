########################################################################
##
## Copyright (C) 2019-2023 The Octave Project Developers
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
## @deftypefn {} {@var{out} =} tar_is_bsd ()
## True if the default tar command is BSD tar.
##
## Checks whether the default tar command (the one invoked when an un-prefixed
## @code{tar} is executed) is BSD tar or another tar.  Caches the results for
## performance.
##
## Returns true if the detected tar is BSD tar, and false otherwise.  Errors if
## @code{tar --version} does not succeed.
## @end deftypefn

function out = tar_is_bsd ()

  ## BSD tar needs to be handled differently from GNU tar
  persistent cache;
  if (isempty (cache))
    [status, tar_ver_str] = system ("tar --version");
    if (status)
      error ("tar: Failed executing tar --version (status = %d)", status);
    endif
    cache = ! isempty (regexp (tar_ver_str, "bsdtar"));
  endif
  out = cache;

endfunction
