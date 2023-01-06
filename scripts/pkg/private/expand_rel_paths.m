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
## @deftypefn {} {@var{pkg_list} =} expand_rel_paths (@var{pkg_list})
## Internal undocumented function.
## @end deftypefn

function pkg_list = expand_rel_paths (pkg_list)

  ## Prepend location of OCTAVE_HOME to install directories
  loc = regexptranslate ("escape", OCTAVE_HOME ());
  for i = 1:numel (pkg_list)
    ## Be sure to only prepend OCTAVE_HOME to pertinent package paths
    pkg_list{i}.dir = regexprep (pkg_list{i}.dir, "^__OH__", loc);
    pkg_list{i}.archprefix = regexprep (pkg_list{i}.archprefix, ...
                                        "^__OH__", loc);
  endfor

endfunction
