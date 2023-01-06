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
## @deftypefn {} {[@var{pkg_desc_list}, @var{flag}] =} describe (@var{pkgnames}, @var{verbose}, @var{local_list}, @var{global_list})
## Undocumented internal function.
## @end deftypefn

function [pkg_desc_list, flag] = describe (pkgnames, verbose, local_list, global_list)

  ## Get the list of installed packages.
  installed_pkgs_lst = installed_packages (local_list, global_list);
  num_packages = length (installed_pkgs_lst);
  ## Add inverse dependencies to "installed_pkgs_lst"
  installed_pkgs_lst = get_inverse_dependencies (installed_pkgs_lst);

  if (isempty (pkgnames))
    describe_all = true;
    flag(1:num_packages) = {"Not Loaded"};
    num_pkgnames = num_packages;
  else
    describe_all = false;
    num_pkgnames = length (pkgnames);
    flag(1:num_pkgnames) = {"Not installed"};
  endif

  for i = 1:num_packages
    curr_name = installed_pkgs_lst{i}.name;
    if (describe_all)
      name_pos = i;
    else
      name_pos = find (strcmp (curr_name, pkgnames));
    endif

    if (! isempty (name_pos))
      if (installed_pkgs_lst{i}.loaded)
        flag{name_pos} = "Loaded";
      else
        flag{name_pos} = "Not loaded";
      endif

      pkg_desc_list{name_pos}.name = installed_pkgs_lst{i}.name;
      pkg_desc_list{name_pos}.version = installed_pkgs_lst{i}.version;
      pkg_desc_list{name_pos}.description = installed_pkgs_lst{i}.description;
      pkg_desc_list{name_pos}.depends = installed_pkgs_lst{i}.depends;
      pkg_desc_list{name_pos}.provides = parse_pkg_idx (installed_pkgs_lst{i}.dir);
      pkg_desc_list{name_pos}.invdeps = unique (installed_pkgs_lst{i}.invdeps);

    endif
  endfor

  non_inst = find (strcmp (flag, "Not installed"));
  if (! isempty (non_inst))
    if (nargout < 2)
      non_inst_str = sprintf (" %s ", pkgnames{non_inst});
      error ("some packages are not installed: %s", non_inst_str);
    else
      pkg_desc_list{non_inst} = struct ("name", {}, "description",
                                        {}, "provides", {});
    endif
  endif

  if (nargout == 0)
    for i = 1:num_pkgnames
      print_package_description (pkg_desc_list{i}.name,
                                 pkg_desc_list{i}.version,
                                 pkg_desc_list{i}.provides,
                                 pkg_desc_list{i}.description,
                                 pkg_desc_list{i}.depends,
                                 pkg_desc_list{i}.invdeps,
                                 flag{i}, verbose);
    endfor
  endif

endfunction


## Read an INDEX file.
function pkg_idx_struct = parse_pkg_idx (packdir)

  index_file = fullfile (packdir, "packinfo", "INDEX");

  if (! exist (index_file, "file"))
    error ("could not find any INDEX file in directory %s, try 'pkg rebuild all' to generate missing INDEX files", packdir);
  endif


  [fid, msg] = fopen (index_file, "r");
  if (fid == -1)
    error ("the INDEX file %s could not be read: %s",
           index_file, msg);
  endif

  cat_num = 1;
  pkg_idx_struct{1}.category = "Uncategorized";
  pkg_idx_struct{1}.functions = {};

  line = fgetl (fid);
  while (isempty (strfind (line, ">>")) && ! feof (fid))
    line = fgetl (fid);
  endwhile

  while (! feof (fid) || line != -1)
    if (! any (! isspace (line)) || line(1) == "#" || any (line == "="))
      ## Comments,  blank lines or comments about unimplemented
      ## functions: do nothing
      ## FIXME: probably comments and pointers to external functions
      ## could be treated better when printing to screen?
    elseif (! isempty (strfind (line, ">>")))
      ## Skip package name and description as they are in DESCRIPTION
      ## already.
    elseif (! isspace (line(1)))
      ## Category.
      if (! isempty (pkg_idx_struct{cat_num}.functions))
        pkg_idx_struct{++cat_num}.functions = {};
      endif
      pkg_idx_struct{cat_num}.category = deblank (line);
    else
      ## Function names.
      while (any (! isspace (line)))
        [fun_name, line] = strtok (line);
        pkg_idx_struct{cat_num}.functions{end+1} = deblank (fun_name);
      endwhile
    endif
    line = fgetl (fid);
  endwhile
  fclose (fid);

endfunction


function print_package_description (pkg_name, pkg_ver, pkg_idx_struct,
                                    pkg_desc, pkg_deps, pkg_invd, status,
                                    verbose)

  printf ("---\nPackage name:\n\t%s\n", pkg_name);
  printf ("Version:\n\t%s\n", pkg_ver);
  printf ("Short description:\n\t%s\n", pkg_desc);
  pkg_deps = cellfun (@(d) sprintf ("%s %s %s", struct2cell (d){:}), pkg_deps,
                      "UniformOutput", false);
  pkg_deps = strjoin (pkg_deps, "\n\t");
  printf ("Depends on:\n\t%s\n", pkg_deps);

  pkg_invd = strjoin (pkg_invd, "\n\t");
  printf ("Depended on by:\n\t%s\n", pkg_invd);

  printf ("Status:\n\t%s\n", status);
  if (verbose)
    printf ("---\nProvides:\n");
    for i = 1:length (pkg_idx_struct)
      if (! isempty (pkg_idx_struct{i}.functions))
        printf ("%s\n", pkg_idx_struct{i}.category);
        for j = 1:length (pkg_idx_struct{i}.functions)
          printf ("\t%s\n", pkg_idx_struct{i}.functions{j});
        endfor
      endif
    endfor
  endif

endfunction
