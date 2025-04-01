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
## with version numbers and some brief descriptions, or returns the list of
## packages compatible with @code{pkg install -forge}.
## @end deftypefn

function retval = list_forge_packages ()

  __pkg__ = get_validated_pkg_list ();  # fresh data from packages.octave.org

  pkgnames = fieldnames (__pkg__);

  formatmore = (nargout == 0);  # do further string formatting for display

  retval = "";

  ## Determine whether each package can be installed by `pkg install -forge`.
  ## This is possible if `pkg` is listed as a prerequisite for that package.
  lgl = false (1, numel (pkgnames));
  for i = 1:numel (pkgnames)

    this = char (pkgnames(i));

    ## In the case of multiple versions, versions(1) is the most recent.
    prereq = char (__pkg__.(this).versions(1).depends.name);
    lgl(i) = any (cell2mat (strfind (cellstr (prereq), "pkg")));

    if (formatmore)  # add more descriptive text to output

      ## Add version number
      v = __pkg__.(this).versions(1).id;
      vers(i, 1:numel(v)) = v;

      ## Add description, truncating long text with "..." but not mid-word.
      str = __pkg__.(this).description;
      if (numel (str) > 80)
        str(81:end) = [];
        f = find (isspace (str), 1, "last");
        str(f:end) = [];
        str = [str, "..."];
      endif
      desc(i, 1:numel (str)) = str;

    endif

  endfor

  if (! formatmore)  # we want only the package names not the versions.

    ## Return only those packages that can be installed with `pkg install -forge`
    retval = char (pkgnames(lgl));

  else  # pretty print on screen.

    vers(vers == 0) = ' ';
    desc(desc == 0) = ' ';

    page_screen_output (false, "local");

    printf ("              Package Name | Version | Description\n");
    printf ("---------------------------+---------+-----------------------------------------------------------------------------------\n");
    for i = 1:nnz (lgl)
      str = char (pkgnames(i));
      if (! lgl(i))
        str = [str, " (!)"];
      endif
      printf ("%26s | %7s | %s\n", str, vers(i, :), desc(i, :));
    endfor
    printf ("(!) These packages have special installation instructions.\n");

  endif

endfunction
