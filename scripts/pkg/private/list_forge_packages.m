########################################################################
##
## Copyright (C) 2005-2025 The Octave Project Developers
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
## @deftypefn {} {@var{list} =} list_forge_packages ()
## Gets the current list of Octave packages, then either displays the list
## with version numbers and some brief installation instructions, or
## returns the list of packages compatible with @code{pkg install -forge}.
## @end deftypefn

function retval = list_forge_packages ()

  __pkg__ = get_validated_pkg_list ();  # fresh data from packages.octave.org

  pkgnames = fieldnames (__pkg__);

  formatmore = (nargout == 0);  # do further string formatting for display

  ## Determine whether each package can be installed by `pkg install -forge`.
  ## This is possible if `pkg` is listed as a prerequisite for that package.
  lgl = false (1, numel (pkgnames));
  for i = 1:numel (pkgnames)

    this = char (pkgnames(i));

    ## In the case of multiple versions, versions(1) is the most recent.
    prereq = char (__pkg__.(this).versions(1).depends.name);
    lgl(i) = any (cell2mat (strfind (cellstr (prereq), "pkg")));

    if (formatmore)  # add version number to output
      v = __pkg__.(this).versions(1).id;
      tmp = sprintf ("%s %s", this, v);
      retval(i, 1:numel (tmp)) = tmp;
    endif

  endfor

  if (! formatmore)  # we want only the package names not the versions.

    ## Return only those packages that can be installed with `pkg install -forge`
    retval = char (pkgnames(lgl));

  else

    ## `retval` has already been built above for display.
    page_screen_output (false, "local");
    fprintf (1, "The following %d packages were found on Octave Packages.\n", rows (retval));

    fprintf (1, "These %d packages should be installed following their individual instructions:\n", nnz (! lgl));
    disp (retval(! lgl, :));

    fprintf (1, "These %d packages can be installed with `pkg install -forge <packagename>`:\n", nnz (lgl));
    disp (retval(lgl, :));

  endif

endfunction
