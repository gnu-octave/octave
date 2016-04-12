## Copyright (C) 2005-2015 Søren Hauberg
## Copyright (C) 2010 VZLU Prague, a.s.
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or (at
## your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

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
        disp ("skipping line");
      else
        colon = colon(1);
        keyword = tolower (strtrim (line(1:colon-1)));
        value = strtrim (line (colon+1:end));
        if (length (value) == 0)
            fclose (fid);
            error ("The keyword '%s' of the package '%s' has an empty value",
                    keyword, desc.name);
        endif
        desc.(keyword) = value;
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
  desc.version = fix_version (desc.version);
  if (isfield (desc, "depends"))
    desc.depends = fix_depends (desc.depends);
  else
    desc.depends = "";
  endif
  desc.name = tolower (desc.name);
endfunction


## Make sure the version string v is a valid x.y.z version string
## Examples: "0.1" => "0.1.0", "monkey" => error(...).
function out = fix_version (v)
  if (regexp (v, '^\d+(\.\d+){1,2}$') == 1)
    parts = ostrsplit (v, '.', true);
    if (numel (parts) == 2)
      out = [v ".0"];
    else
      out = v;
    endif
  else
    error ("bad version string: %s", v);
  endif
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
        version = fix_version (nm.ver);
        ## If no version is specified for the dependency
        ## we say that the version should be greater than
        ## or equal to "0.0.0".
      else
        package = tolower (strtrim (dep));
        operator = ">=";
        version = "0.0.0";
      endif
      deps_cell{i} = struct ("package", package,
                             "operator", operator,
                             "version", version);
    else
      error ("incorrect syntax for dependency '%s' in the DESCRIPTION file\n",
             dep);
    endif
  endfor
endfunction
