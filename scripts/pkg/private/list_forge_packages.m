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
## @deftypefn {} {@var{list} =} list_forge_packages ()
## Undocumented internal function.
## @end deftypefn

function list = list_forge_packages ()

  [list, succ] = urlread ("https://packages.octave.org/list_packages.php");
  if (! succ)
    error ("pkg: could not read URL, please verify internet connection");
  endif

  list = ostrsplit (list, " \n\t", true);

  if (nargout == 0)
    ## FIXME: This is a convoluted way to get the latest version number
    ##        for each package in less than 56 seconds (bug #39479).

    ## Get the list of all packages ever published
    [html, succ] = urlread ('https://sourceforge.net/projects/octave/files/Octave%20Forge%20Packages/Individual%20Package%20Releases');

    if (! succ)
      error ("pkg: failed to fetch list of packages from sourceforge.net");
    endif

    ## Scrape the HTML
    ptn = '<tr\s+title="(.*?gz)"\s+class="file';
    [succ, tok] = regexp (html, ptn, "start", "tokens");
    if (isempty (succ))
      error ("pkg: failed to parse list of packages from sourceforge.net");
    endif

    ## Remove version numbers and produce unique list of packages
    files = cellstr (tok);
    pkg_names = cellstr (regexp (files, '^.*?(?=-\d)', "match"));
    [~, idx] = unique (pkg_names, "first");
    files = files(idx);

    page_screen_output (false, "local");
    puts ("Octave Forge provides these packages:\n");
    for i = 1:length (list)
      pkg_nm = list{i};
      idx = regexp (files, sprintf ('^%s(?=-\\d)', pkg_nm));
      idx = ! cellfun (@isempty, idx);
      if (any (idx))
        ver = regexp (files{idx}, '\d+\.\d+\.\d+', "match"){1};
      else
        ver = "unknown";
      endif
      printf ("  %s %s\n", pkg_nm, ver);
    endfor
  endif

endfunction
