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
## @deftypefn {} {@var{retval} =} search_packages (@var{searchterms})
## Search for all packages on the Octave Packages repository whose
## descriptions include @var{searchterms}, then either display the search
## results with brief descriptions, or return the list of matching packages
## that can also be installed with @code{pkg install}.
## @end deftypefn

function retval = search_packages (searchterms, allpackages)

  __pkg__ = get_validated_pkg_list ();  # fresh data from packages.octave.org

  pkgnames = fieldnames (__pkg__);

  prettyprint = (nargout == 0);  # do further string formatting for display

  retval = "";

  ## Examine each package in turn and check for search terms and installability
  installable = false (1, numel (pkgnames));
  has_search_terms = true (1, numel (pkgnames));

  for i = numel (pkgnames) : -1 : 1

    this = char (pkgnames(i));

    ## Filter based on search term(s) being present in the description.
    ## If multiple search terms were given, we do a boolean AND over them,
    ## which returns the set intersection of the search results for the
    ## individual terms.
    str = __pkg__.(this).description;
    if (! allpackages)  # search terms are provided, so we need to filter
      j = 0;
      while (has_search_terms(i) && ++j <= numel (searchterms))
        has_search_terms(i) &= any (regexpi (str, searchterms{j}, 'once'));
      endwhile
    endif

    if (! has_search_terms(i))  # no need to examine this package further
      continue;
    endif

    ## If we are here, has_search_terms(i) = true.

    ## Determine whether each package can be installed by "pkg install".
    ## This is possible if "pkg" is listed as a prerequisite for that package.
    ## In the case of multiple versions, versions(1) is the most recent.
    prereq = convert_possible_cell_to_struct (__pkg__.(this).versions(1)).depends;
    installable(i) = any (strcmp (prereq, "pkg"));

    if (prettyprint)  # add more descriptive text to output

      ## Add version number
      v = convert_possible_cell_to_struct (__pkg__.(this).versions(1)).id;
      vers(i, 1:numel (v)) = v;

      ## Add description, truncating long text with "..." but not mid-word.
      ## FIXME: Maybe show only the relevant part of description that includes
      ## the search term(s).
      str = __pkg__.(this).description;
      if (numel (str) > 80)
        str(81:end) = [];
        idx = find (isspace (str), 1, "last");
        str(idx:end) = [];
        str = [str, "..."];
      endif
      desc(i, 1:numel (str)) = str;

    endif

  endfor

  if (! prettyprint)  # we want only the package names not the versions.

    ## Return only those packages that match the given search terms
    ## and can also be installed with "pkg install".
    retval = char (pkgnames(has_search_terms & installable));

  else  # pretty print on screen

    if (! any (has_search_terms))  # no search results
      printf ("No packages were found with the given search term(s)\n");
      return;
    endif

    vers(vers == 0) = ' ';
    desc(desc == 0) = ' ';
    special = false;

    printf ("Search found %d results\n", nnz (has_search_terms));
    printf ("              Package Name | Version | Description\n");
    printf ("---------------------------+---------+------------------------------------\n");

    for i = find (has_search_terms)  # restrict attention to search results only
      str = char (pkgnames(i));
      if (! installable(i))
        str = [str, " (!)"];
        special = true;
      endif
      printf ("%26s | %7s | %s\n", str, vers(i, :), desc(i, :));
    endfor

    if (special)
      printf ("(!) These packages have special installation instructions.\n");
    endif

  endif

endfunction
