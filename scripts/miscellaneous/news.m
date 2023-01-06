########################################################################
##
## Copyright (C) 2007-2023 The Octave Project Developers
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
## @deftypefn  {} {} news
## @deftypefnx {} {} news @var{package}
## Display the current NEWS file for Octave or an installed package.
##
## When called without an argument, display the NEWS file for Octave.
##
## When given a package name @var{package}, display the current NEWS file for
## that package.
## @seealso{ver, pkg}
## @end deftypefn

function news (package = "octave")

  ## function takes care of validating PACKAGE input
  display_info_file ("news", package, "NEWS");

endfunction


%!error <news: PACKAGE must be a string> news (1)
%!error <news: package .* is not installed> news ("__NOT_A_VALID_PKG_NAME__")
