## Copyright (C) 2005-2012 Søren Hauberg
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
## @deftypefn {Function File} {[@var{out1}, @var{out2}] =} installed_packages (@var{local_list}, @var{global_list})
## Undocumented internal function.
## @end deftypefn

function [out1, out2] = installed_packages (local_list, global_list)
  ## Get the list of installed packages.
  try
    local_packages = load (local_list).local_packages;
  catch
    local_packages = {};
  end_try_catch
  try
    global_packages = load (global_list).global_packages;
  catch
    global_packages = {};
  end_try_catch
  installed_pkgs_lst = {local_packages{:}, global_packages{:}};

  ## Eliminate duplicates in the installed package list.
  ## Locally installed packages take precedence.
  dup = [];
  for i = 1:length (installed_pkgs_lst)
    if (any (dup == i))
      continue;
    endif
    for j = (i+1):length (installed_pkgs_lst)
      if (any (dup == j))
        continue;
      endif
      if (strcmp (installed_pkgs_lst{i}.name, installed_pkgs_lst{j}.name))
        dup = [dup, j];
      endif
    endfor
  endfor
  if (! isempty (dup))
    installed_pkgs_lst(dup) = [];
  endif

  ## Now check if the package is loaded.
  tmppath = strrep (path (), "\\", "/");
  for i = 1:length (installed_pkgs_lst)
    if (strfind (tmppath, strrep (installed_pkgs_lst{i}.dir, '\', '/')))
      installed_pkgs_lst{i}.loaded = true;
    else
      installed_pkgs_lst{i}.loaded = false;
    endif
  endfor
  for i = 1:length (local_packages)
    if (strfind (tmppath, strrep (local_packages{i}.dir, '\', '/')))
      local_packages{i}.loaded = true;
    else
      local_packages{i}.loaded = false;
    endif
  endfor
  for i = 1:length (global_packages)
    if (strfind (tmppath, strrep (global_packages{i}.dir, '\', '/')))
      global_packages{i}.loaded = true;
    else
      global_packages{i}.loaded = false;
    endif
  endfor

  ## Should we return something?
  if (nargout == 2)
    out1 = local_packages;
    out2 = global_packages;
    return;
  elseif (nargout == 1)
    out1 = installed_pkgs_lst;
    return;
  endif

  ## We shouldn't return something, so we'll print something.
  num_packages = length (installed_pkgs_lst);
  if (num_packages == 0)
    printf ("no packages installed.\n");
    return;
  endif

  ## Compute the maximal lengths of name, version, and dir.
  h1 = "Package Name";
  h2 = "Version";
  h3 = "Installation directory";
  max_name_length = length (h1);
  max_version_length = length (h2);
  names = cell (num_packages, 1);
  for i = 1:num_packages
    max_name_length = max (max_name_length,
                           length (installed_pkgs_lst{i}.name));
    max_version_length = max (max_version_length,
                              length (installed_pkgs_lst{i}.version));
    names{i} = installed_pkgs_lst{i}.name;
  endfor
  max_dir_length = terminal_size ()(2) - max_name_length - ...
                                             max_version_length - 7;
  if (max_dir_length < 20)
     max_dir_length = Inf;
  endif

  h1 = postpad (h1, max_name_length + 1, " ");
  h2 = postpad (h2, max_version_length, " ");;

  ## Print a header.
  header = sprintf ("%s | %s | %s\n", h1, h2, h3);
  printf (header);
  tmp = sprintf (repmat ("-", 1, length (header) - 1));
  tmp(length(h1)+2) = "+";
  tmp(length(h1)+length(h2)+5) = "+";
  printf ("%s\n", tmp);

  ## Print the packages.
  format = sprintf ("%%%ds %%1s| %%%ds | %%s\n", max_name_length,
                    max_version_length);
  [dummy, idx] = sort (names);
  for i = 1:num_packages
    cur_name = installed_pkgs_lst{idx(i)}.name;
    cur_version = installed_pkgs_lst{idx(i)}.version;
    cur_dir = installed_pkgs_lst{idx(i)}.dir;
    if (length (cur_dir) > max_dir_length)
      first_char = length (cur_dir) - max_dir_length + 4;
      first_filesep = strfind (cur_dir(first_char:end), filesep ());
      if (! isempty (first_filesep))
        cur_dir = ["..." cur_dir((first_char + first_filesep(1) - 1):end)];
      else
        cur_dir = ["..." cur_dir(first_char:end)];
      endif
    endif
    if (installed_pkgs_lst{idx(i)}.loaded)
      cur_loaded = "*";
    else
      cur_loaded = " ";
    endif
    printf (format, cur_name, cur_loaded, cur_version, cur_dir);
  endfor
endfunction

