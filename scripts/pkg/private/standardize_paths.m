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
## @deftypefn {} {@var{pkg_list} =} standardize_paths (@var{pkg_list})
## Convert pathnames in various representations to unique strings.
##
## The input @var{pkg_list} must be a cell array of strings.
##
## This function is particularly necessary on Windows platforms where pathnames
## may differ in case (@file{file1} vs.@: @file {FILE1}), file separator
## (@samp{\} vs.@: @samp{/}), and format (@file{A~spaces.txt} (8.3 convention)
## vs.@: @file{A filename with spaces.txt}).
##
## @seealso{canonicalize_file_name}
## @end deftypefn

function pkg_list = standardize_paths (pkg_list)

  for i = 1:numel (pkg_list)
    pkg_list{i}.dir = canonicalize_file_name (pkg_list{i}.dir);
    pkg_list{i}.archprefix = canonicalize_file_name (pkg_list{i}.archprefix);
  endfor

endfunction
