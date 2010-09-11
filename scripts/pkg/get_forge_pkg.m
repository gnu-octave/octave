## Copyright (C) 2010 VZLU Prague, a.s.
##
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or
## (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn {Function File} {[@var{ver}, @var{url}] =} get_forge_pkg (@var{name})
## Tries to discover the current version of an OctaveForge package from the web,
## using a working internet connection and the urlread function.
## If two output arguments are requested, returns also an address to download
## the file.
## @end deftypefn

function [ver, url] = get_forge_pkg (name)
  if (nargin != 1)
    print_usage ();
  endif
  ## Verify that name is valid.
  if (! (ischar (name) && rows (name) == 1 && ndims (name) == 2))
    error ("get_forge_pkg: package name must be a string");
  elseif (! all (isalnum (name) | name == "-" | name == "." | name == "_"))
    error ("get_forge_pkg: invalid package name: %s", name);
  endif

  name = tolower (name);

  ## Try to download package's index page.
  [html, succ] = urlread (sprintf ("http://octave.sourceforge.net/%s/index.html", name));
  if (succ)
    ## Remove blanks for simpler matching.
    html(isspace(html)) = [];
    ## Good. Let's grep for the version.
    pat = "<tdclass=""package_table"">PackageVersion:</td><td>([0-9\\.]*)</td>";
    [~, ~, ~, ~, t] = regexp (html, pat);
    if (isempty (t) || isempty(t{1}))
      error ("get_forge_pkg: could not read version number from package's page.");
    else
      ver = t{1}{1};
      if (nargout > 1)
        # Build download string.
        urlbase = "http://downloads.sourceforge.net/octave/%s-%s.tar.gz?download";
        url = sprintf (urlbase, name, ver);
        ## Verify that the string exists on the page.
        if (isempty (strfind (html, url)))
          warning ("get_forge_pkg: download URL not verified.");
        endif
      endif
    endif
  else
    ## Try get the list of all packages.
    [html, succ] = urlread ("http://octave.sourceforge.net/packages.php");
    if (succ)
      [~, ~, ~, ~, t] = regexp (html, "<div class=""package"" id=""(\\w+)"">");
      t = horzcat (t{:});
      if (any (strcmp (t, name)))
        error ("get_forge_pkg: package name exists, but index page not available");
      else
        ## Try a simplistic method to determine close names.
        dist = cellfun (@(n) length (setdiff (name, n)), t);
        [~, i] = min (dist);
        error ("get_forge_pkg: package not found: ""%s"". Maybe you meant ""%s?""", name, t{i});
      endif
    else
      error ("get_forge_pkg: could not read URL, please verify internet connection");
    endif
  endif

endfunction
