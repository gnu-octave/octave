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
## @deftypefn {} {} load_packages (@var{files}, @var{handle_deps}, @var{local_list}, @var{global_list})
## Undocumented internal function.
## @end deftypefn

function load_packages (files, handle_deps, local_list, global_list)

  installed_pkgs_lst = installed_packages (local_list, global_list);
  num_packages = length (installed_pkgs_lst);

  ## Read package names and installdirs into a more convenient format.
  pnames = pdirs = cell (1, num_packages);
  for i = 1:num_packages
    pnames{i} = installed_pkgs_lst{i}.name;
    pdirs{i} = installed_pkgs_lst{i}.dir;
  endfor

  idx = [];
  for i = 1:length (files)
    idx2 = find (strcmp (pnames, files{i}));
    if (! any (idx2))
      error ("package %s is not installed", files{i});
    endif
    idx(end + 1) = idx2;
  endfor

  ## Load the packages, but take care of the ordering of dependencies.
  load_packages_and_dependencies (idx, handle_deps, installed_pkgs_lst, true);

endfunction
