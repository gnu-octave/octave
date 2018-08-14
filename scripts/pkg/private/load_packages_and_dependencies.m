## Copyright (C) 2005-2018 Søren Hauberg
## Copyright (C) 2010 VZLU Prague, a.s.
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

## -*- texinfo -*-
## @deftypefn {} {} load_packages_and_dependencies (@var{idx}, @var{handle_deps}, @var{installed_pkgs_lst}, @var{global_install})
## Undocumented internal function.
## @end deftypefn

function load_packages_and_dependencies (idx, handle_deps, installed_pkgs_lst,
                                         global_install)

  idx = load_package_dirs (idx, [], handle_deps, installed_pkgs_lst);
  dirs = {};
  execpath = EXEC_PATH ();
  for i = idx;
    ndir = installed_pkgs_lst{i}.dir;
    dirs{end+1} = ndir;
    if (isfolder (fullfile (dirs{end}, "bin")))
      execpath = [execpath pathsep() fullfile(dirs{end}, "bin")];
    endif
    tmpdir = getarchdir (installed_pkgs_lst{i});
    if (isfolder (tmpdir))
      dirs{end + 1} = tmpdir;
      if (isfolder (fullfile (dirs{end}, "bin")))
        execpath = [execpath pathsep() fullfile(dirs{end}, "bin")];
      endif
    endif
  endfor

  ## Load the packages.
  if (length (dirs) > 0)
    addpath (dirs{:});
  endif

  ## Add the binaries to exec_path.
  if (! strcmp (EXEC_PATH, execpath))
    EXEC_PATH (execpath);
  endif

endfunction


function idx = load_package_dirs (lidx, idx, handle_deps, installed_pkgs_lst)

  for i = lidx
    if (isfield (installed_pkgs_lst{i}, "loaded")
        && installed_pkgs_lst{i}.loaded)
      continue;
    else
      ## Insert this package at the front before recursing over dependencies.
      if (! any (idx == i))
        idx = [i, idx];
      endif

      if (handle_deps)
        deps = installed_pkgs_lst{i}.depends;
        if ((length (deps) > 1)
            || (length (deps) == 1 && ! strcmp (deps{1}.package, "octave")))
          tmplidx = [];
          for k = 1 : length (deps)
            for j = 1 : length (installed_pkgs_lst)
              if (strcmp (installed_pkgs_lst{j}.name, deps{k}.package))
                if (! any (idx == j))
                  tmplidx(end + 1) = j;
                  break;
                endif
              endif
            endfor
          endfor
          idx = load_package_dirs (tmplidx, idx, handle_deps,
                                 installed_pkgs_lst);
        endif
      endif
    endif
  endfor

endfunction
