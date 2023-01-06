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
## @deftypefn {} {@var{desc} =} get_description (@var{filename})
## Undocumented internal function.
## @end deftypefn

## Parse the DESCRIPTION file.
function desc = get_description (filename)

  [fid, msg] = fopen (filename, "r");
  if (fid == -1)
    error ("the DESCRIPTION file %s could not be read: %s", filename, msg);
  endif

  desc = struct ();

  line = fgetl (fid);
  while (line != -1)
    if (line(1) == "#")
      ## Comments, do nothing.
    elseif (isspace (line(1)))
      ## Continuation lines
      if (exist ("keyword", "var") && isfield (desc, keyword))
        desc.(keyword) = [desc.(keyword) " " deblank(line)];
      endif
    else
      ## Keyword/value pair
      colon = find (line == ":");
      if (length (colon) == 0)
        warning ("pkg: skipping invalid line in DESCRIPTION file");
      else
        colon = colon(1);
        keyword = tolower (strtrim (line(1:colon-1)));
        value = strtrim (line (colon+1:end));
        if (length (value) == 0)
            fclose (fid);
            error ("The keyword '%s' of the package '%s' has an empty value",
                    keyword, desc.name);
        endif
        if (isfield (desc, keyword))
          warning ('pkg: duplicate keyword "%s" in DESCRIPTION, ignoring',
                   keyword);
        else
          desc.(keyword) = value;
        endif
      endif
    endif
    line = fgetl (fid);
  endwhile
  fclose (fid);

  ## Make sure all is okay.
  needed_fields = {"name", "version", "date", "title", ...
                   "author", "maintainer", "description"};
  for f = needed_fields
    if (! isfield (desc, f{1}))
      error ("description is missing needed field %s", f{1});
    endif
  endfor

  if (! is_valid_pkg_version_string (desc.version))
    error ("invalid version string '%s'", desc.version);
  endif

  if (isfield (desc, "depends"))
    desc.depends = fix_depends (desc.depends);
  else
    desc.depends = "";
  endif
  desc.name = tolower (desc.name);

endfunction


## Make sure the depends field is of the right format.
## This function returns a cell of structures with the following fields:
##   package, version, operator
function deps_cell = fix_depends (depends)

  deps = strtrim (ostrsplit (tolower (depends), ","));
  deps_cell = cell (1, length (deps));
  dep_pat = ...
  '\s*(?<name>[-\w]+)\s*(\(\s*(?<op>[<>=]+)\s*(?<ver>\d+\.\d+(\.\d+)*)\s*\))*\s*';

  ## For each dependency.
  for i = 1:length (deps)
    dep = deps{i};
    [start, nm] = regexp (dep, dep_pat, 'start', 'names');
    ## Is the dependency specified
    ## in the correct format?
    if (! isempty (start))
      package = tolower (strtrim (nm.name));
      ## Does the dependency specify a version
      ## Example: package(>= version).
      if (! isempty (nm.ver))
        operator = nm.op;
        if (! any (strcmp (operator, {">", ">=", "<=", "<", "=="})))
          error ("unsupported operator: %s", operator);
        endif
        if (! is_valid_pkg_version_string (nm.ver))
          error ("invalid dependency version string '%s'", nm.ver);
        endif
      else
        ## If no version is specified for the dependency
        ## we say that the version should be greater than
        ## or equal to "0.0.0".
        package = tolower (strtrim (dep));
        operator = ">=";
        nm.ver  = "0.0.0";
      endif
      deps_cell{i} = struct ("package", package,
                             "operator", operator,
                             "version", nm.ver);
    else
      error ("incorrect syntax for dependency '%s' in the DESCRIPTION file\n",
             dep);
    endif
  endfor

endfunction

function valid = is_valid_pkg_version_string (str)

  ## We are limiting ourselves to this set of characters because the
  ## version will appear on the filepath.  The portable character, according to
  ## http://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap03.html#tag_03_278
  ## is [A-Za-z0-9\.\_\-].  However, this is very limited.  We specially
  ## want to support a "+" so we can support "pkgname-2.1.0+" during
  ## development.  So we use Debian's character set for version strings
  ## https://www.debian.org/doc/debian-policy/ch-controlfields.html#s-f-Version
  ## with the exception of ":" (colon) because that's the PATH separator.
  ##
  ## Debian does not include "_" because it is used to separate the name,
  ## version, and arch in their deb files.  While the actual filenames are
  ## never parsed to get that information, it is important to have a unique
  ## separator character to prevent filename clashes.  For example, if we
  ## used hyhen as separator, "signal-2-1-rc1" could be "signal-2" version
  ## "1-rc1" or "signal" version "2-1-rc1".  A package file for both must be
  ## able to co-exist in the same directory, e.g., during package install or
  ## in a flat level package repository.
  valid = numel (regexp (str, '[^0-9a-zA-Z\.\+\-\~]')) == 0;

endfunction
