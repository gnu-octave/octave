########################################################################
##
## Copyright (C) 2005-2023 The Octave Project Developers
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
## @deftypefn {} {[@var{url}, @var{local_file}] =} get_forge_download (@var{name})
## Undocumented internal function.
## @end deftypefn

function [url, local_file] = get_forge_download (name)
  [ver, url] = get_forge_pkg (name);
  local_file = tempname (tempdir (), [name "-" ver "-"]);
  local_file = [local_file ".tar.gz"];
endfunction
