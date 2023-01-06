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
## @deftypefn {} {} uninstall (@var{pkgnames}, @var{handle_deps}, @var{verbose}, @var{local_list}, @var{global_list}, @var{global_install})
## Undocumented internal function.
## @end deftypefn

function uninstall (pkgnames, handle_deps, verbose, local_list,
                    global_list, global_install)

  ## Get the list of installed packages.
  [local_packages, global_packages] = installed_packages (local_list,
                                                          global_list);
  if (global_install)
    installed_pkgs_lst = [local_packages, global_packages];
  else
    installed_pkgs_lst = local_packages;
  endif

  all_pkgs_list = [local_packages, global_packages];
  all_installed_pkgs_lst = cellfun (@(x) x.name, all_pkgs_list, ...
                                    "uniformoutput", false);
  num_packages = numel (installed_pkgs_lst);
  delete_idx = [];
  available_packages = intersect (all_installed_pkgs_lst, pkgnames);

  for i = 1:num_packages
    cur_name = installed_pkgs_lst{i}.name;
    if (any (strcmp (cur_name, pkgnames)))
      delete_idx(end+1) = i;
    endif
  endfor

  ## Are all the packages that should be uninstalled already installed?
  if (numel (available_packages) != numel (pkgnames))
    pkgs_not_installed = setxor (available_packages, pkgnames);
    for idx = 1:numel (pkgs_not_installed)
      warning ("package %s is not installed\n", pkgs_not_installed{idx});
    endfor
  endif

  ## inform user if any global packages can't be uninstalled
  if (! global_install)
    for i = 1:numel (global_packages)
      if (any (strcmp (global_packages{i}.name, pkgnames)))
        warning ("%s is a global package and cannot be removed locally\n", ...
          global_packages{i}.name);
      endif
    endfor
  endif

  if (isempty (delete_idx))
    warning ("no packages will be uninstalled");
  else

    ## Compute the packages that will remain installed.
    idx = setdiff (1:num_packages, delete_idx);
    remaining_packages = installed_pkgs_lst(idx);
    to_delete_packages = installed_pkgs_lst(delete_idx);

    ## Check dependencies.
    if (handle_deps)
      error_text = "";
      for i = 1:numel (remaining_packages)
        desc = remaining_packages{i};
        bad_deps = get_unsatisfied_deps (desc, to_delete_packages, true);

        ## Will the uninstallation break any dependencies?
        if (! isempty (bad_deps))
          for i = 1:numel (bad_deps)
            dep = bad_deps{i};
            error_text = [error_text " " desc.name " needs " ...
                          dep.package " " dep.operator " " dep.version "\n"];
          endfor
        endif
      endfor

      if (! isempty (error_text))
        error ("the following dependencies where unsatisfied:\n  %s", error_text);
      endif
    endif

    ## Delete the directories containing the packages.
    for i = delete_idx
      desc = installed_pkgs_lst{i};
      ## If an 'on_uninstall.m' exist, call it!
      if (exist (fullfile (desc.dir, "packinfo", "on_uninstall.m"), "file"))
        wd = pwd ();
        cd (fullfile (desc.dir, "packinfo"));
        on_uninstall (desc);
        cd (wd);
      endif
      ## Do the actual deletion.
      if (desc.loaded)
        rmpath (desc.dir);
        if (isfolder (getarchdir (desc)))
          rmpath (getarchdir (desc));
        endif
      endif
      if (isfolder (desc.dir))
        ## FIXME: If first call to rmdir fails, then error() will
        ##        stop further processing of getarchdir & archprefix.
        ##        If this is, in fact, correct, then calls should
        ##        just be shortened to rmdir (...) and let rmdir()
        ##        report failure and reason for failure.
        [status, msg] = rmdir (desc.dir, "s");
        if (status != 1 && isfolder (desc.dir))
          error ("couldn't delete directory %s: %s", desc.dir, msg);
        endif
        [status, msg] = rmdir (getarchdir (desc), "s");
        if (status != 1 && isfolder (getarchdir (desc)))
          error ("couldn't delete directory %s: %s", getarchdir (desc), msg);
        endif
        if (dirempty (desc.archprefix))
          sts = rmdir (desc.archprefix, "s");
        endif
      else
        warning ("directory %s previously lost", desc.dir);
      endif
    endfor

    ## Write a new ~/.octave_packages.
    if (global_install)
      if (numel (remaining_packages) == 0)
        [~] = unlink (global_list);
      else
        global_packages = save_order (remaining_packages);
        if (ispc)
          ## On Windows ensure LFN paths are saved rather than 8.3 style paths
          global_packages = standardize_paths (global_packages);
        endif
        global_packages = make_rel_paths (global_packages);
        save (global_list, "global_packages");
      endif
    else
      if (numel (remaining_packages) == 0)
        [~] = unlink (local_list);
      else
        local_packages = save_order (remaining_packages);
        if (ispc)
          local_packages = standardize_paths (local_packages);
        endif
        save (local_list, "local_packages");
      endif
    endif
  endif

endfunction
