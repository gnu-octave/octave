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
## @deftypefn {} {[@var{ver}, @var{url}] =} get_pkg_info (@var{name})
## Return the current version and URL of the Octave package @var{name}.
##
## @end deftypefn

function [ver, url] = get_pkg_info (name)

  ## Verify that name is valid.
  if (! (ischar (name) && rows (name) == 1 && ndims (name) == 2))
    error ("get_pkg_info: package NAME must be a string");
  elseif (! all (isalnum (name) | name == "-" | name == "." | name == "_"))
    error ("get_pkg_info: invalid package NAME: %s", name);
  endif

  name = lower (name);

  __pkg__ = get_validated_pkg_list ();  # fresh data from packages.octave.org
  pkgnames = fieldnames (__pkg__);  # all the different packages

  if (any (strcmp (pkgnames, name)))  # named package does exist

    ## If multiple versions, then versions(1) is the most recent version.
    tmp = convert_possible_cell_to_struct (__pkg__.(name).versions(1));
    ver = tmp.id;
    url = tmp.url;

  else  # no such package in list; offer suggestions with error message.

    ## Try a simplistic method to determine similar names.
    function d = fdist (x)

      len1 = numel (name);
      len2 = numel (x);
      lo = min (len1, len2);
      excess = max (len1, len2) - lo;
      d = sum (abs (lower (name(1:lo)) - lower (x(1:lo)))) + excess * 23;

    endfunction

    dist = cellfun ("fdist", pkgnames);
    [~, i] = min (dist);
    error ("get_pkg_info: package not found: ""%s"".  Did you mean ""%s""?", ...
           name, pkgnames{i});

  endif

endfunction
