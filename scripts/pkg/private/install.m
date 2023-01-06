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
## @deftypefn {} {} install (@var{files}, @var{handle_deps}, @var{prefix}, @var{archprefix}, @var{verbose}, @var{local_list}, @var{global_list}, @var{global_install})
## Undocumented internal function.
## @end deftypefn

function install (files, handle_deps, prefix, archprefix, verbose,
                  local_list, global_list, global_install)

  ## Check that the directory in prefix exist.  If it doesn't: create it!
  if (! isfolder (prefix))
    warning ("creating installation directory %s", prefix);
    [status, msg] = mkdir (prefix);
    if (status != 1)
      error ("could not create installation directory: %s", msg);
    endif
  endif

  ## Get the list of installed packages.
  [local_packages, global_packages] = installed_packages (local_list,
                                                          global_list);

  installed_pkgs_lst = {local_packages{:}, global_packages{:}};

  if (global_install)
    packages = global_packages;
  else
    packages = local_packages;
  endif

  if (ispc ())
    oct_glob = @__wglob__;
  else
    oct_glob = @glob;
  endif

  ## Uncompress the packages and read the DESCRIPTION files.
  tmpdirs = packdirs = descriptions = {};
  try
    ## Warn about non existent files.
    for i = 1:length (files)
      if (isempty (oct_glob (files{i})))
        warning ("file %s does not exist", files{i});
      endif
    endfor

    ## Unpack the package files and read the DESCRIPTION files.
    files = oct_glob (files);
    packages_to_uninstall = [];
    for i = 1:length (files)
      tgz = files{i};

      if (exist (tgz, "file"))
        ## Create a temporary directory.
        tmpdir = tempname ();
        tmpdirs{end+1} = tmpdir;
        if (verbose)
          printf ("mkdir (%s)\n", tmpdir);
        endif
        [status, msg] = mkdir (tmpdir);
        if (status != 1)
          error ("couldn't create temporary directory: %s", msg);
        endif

        ## Uncompress the package.
        [~, ~, ext] = fileparts (tgz);
        if (strcmpi (ext, ".zip"))
          func_uncompress = @unzip;
        else
          func_uncompress = @untar;
        endif
        if (verbose)
          printf ("%s (%s, %s)\n", func2str (func_uncompress), tgz, tmpdir);
        endif
        func_uncompress (tgz, tmpdir);

        ## Get the name of the directories produced by tar.
        [dirlist, err, msg] = readdir (tmpdir);
        if (err)
          error ("couldn't read directory produced by tar: %s", msg);
        endif

        if (length (dirlist) > 3)
          error ("bundles of packages are not allowed");
        endif
      endif

      ## The filename pointed to an uncompressed package to begin with.
      if (isfolder (tgz))
        dirlist = {".", "..", tgz};
      endif

      if (exist (tgz, "file") || isfolder (tgz))
        ## The two first entries of dirlist are "." and "..".
        if (exist (tgz, "file"))
          packdir = fullfile (tmpdir, dirlist{3});
        else
          packdir = fullfile (pwd (), dirlist{3});
        endif
        packdirs{end+1} = packdir;

        ## Make sure the package contains necessary files.
        verify_directory (packdir);

        ## Read the DESCRIPTION file.
        filename = fullfile (packdir, "DESCRIPTION");
        desc = get_description (filename);

        ## Set default installation directory.
        desc.dir = fullfile (prefix, [desc.name "-" desc.version]);

        ## Set default architectire dependent installation directory.
        desc.archprefix = fullfile (archprefix, [desc.name "-" desc.version]);

        ## Save desc.
        descriptions{end+1} = desc;

        ## Are any of the new packages already installed?
        ## If so we'll remove the old version.
        for j = 1:length (packages)
          if (strcmp (packages{j}.name, desc.name))
            packages_to_uninstall(end+1) = j;
          endif
        endfor
      endif
    endfor
  catch
    ## Something went wrong, delete tmpdirs.
    for i = 1:length (tmpdirs)
      sts = rmdir (tmpdirs{i}, "s");
    endfor
    rethrow (lasterror ());
  end_try_catch

  ## Check dependencies.
  if (handle_deps)
    ok = true;
    error_text = "";
    for i = 1:length (descriptions)
      desc = descriptions{i};
      idx2 = setdiff (1:length (descriptions), i);
      if (global_install)
        ## Global installation is not allowed to have dependencies on locally
        ## installed packages.
        idx1 = setdiff (1:length (global_packages), packages_to_uninstall);
        pseudo_installed_packages = {global_packages{idx1}, ...
                                     descriptions{idx2}};
      else
        idx1 = setdiff (1:length (local_packages), packages_to_uninstall);
        pseudo_installed_packages = {local_packages{idx1}, ...
                                     global_packages{:}, ...
                                     descriptions{idx2}};
      endif
      bad_deps = get_unsatisfied_deps (desc, pseudo_installed_packages);
      ## Are there any unsatisfied dependencies?
      if (! isempty (bad_deps))
        ok = false;
        for i = 1:length (bad_deps)
          dep = bad_deps{i};
          error_text = [error_text " " desc.name " needs " ...
                        dep.package " " dep.operator " " dep.version "\n"];
        endfor
      endif
    endfor

    ## Did we find any unsatisfied dependencies?
    if (! ok)
      error ("the following dependencies were unsatisfied:\n  %s", error_text);
    endif
  endif

  ## Prepare each package for installation.
  try
    for i = 1:length (descriptions)
      desc = descriptions{i};
      pdir = packdirs{i};
      prepare_installation (desc, pdir);
      configure_make (desc, pdir, verbose);
      copy_built_files (desc, pdir, verbose);
    endfor
  catch
    ## Something went wrong, delete tmpdirs.
    for i = 1:length (tmpdirs)
      sts = rmdir (tmpdirs{i}, "s");
    endfor
    rethrow (lasterror ());
  end_try_catch

  ## Uninstall the packages that will be replaced.
  try
    for i = packages_to_uninstall
      if (global_install)
        uninstall ({global_packages{i}.name}, false, verbose, local_list,
                   global_list, global_install);
      else
        uninstall ({local_packages{i}.name}, false, verbose, local_list,
                   global_list, global_install);
      endif
    endfor
  catch
    ## Something went wrong, delete tmpdirs.
    for i = 1:length (tmpdirs)
      sts = rmdir (tmpdirs{i}, "s");
    endfor
    rethrow (lasterror ());
  end_try_catch

  ## Install each package.
  try
    for i = 1:length (descriptions)
      desc = descriptions{i};
      pdir = packdirs{i};
      copy_files (desc, pdir, global_install);
      create_pkgadddel (desc, pdir, "PKG_ADD", global_install);
      create_pkgadddel (desc, pdir, "PKG_DEL", global_install);
      finish_installation (desc, pdir, global_install);
      generate_lookfor_cache (desc);
    endfor
  catch
    ## Something went wrong, delete tmpdirs.
    for i = 1:length (tmpdirs)
      sts = rmdir (tmpdirs{i}, "s");
    endfor
    for i = 1:length (descriptions)
      sts = rmdir (descriptions{i}.dir, "s");
      sts = rmdir (getarchdir (descriptions{i}), "s");
    endfor
    rethrow (lasterror ());
  end_try_catch

  ## Check if the installed directory is empty.  If it is remove it
  ## from the list.
  for i = length (descriptions):-1:1
    if (dirempty (descriptions{i}.dir, {"packinfo", "doc"})
        && dirempty (getarchdir (descriptions{i})))
      warning ("package %s is empty\n", descriptions{i}.name);
      sts = rmdir (descriptions{i}.dir, "s");
      sts = rmdir (getarchdir (descriptions{i}), "s");
      descriptions(i) = [];
    endif
  endfor

  ## Add the packages to the package list.
  try
    if (global_install)
      idx = setdiff (1:length (global_packages), packages_to_uninstall);
      global_packages = save_order ({global_packages{idx}, descriptions{:}});
      if (ispc)
        ## On Windows ensure LFN paths are saved rather than 8.3 style paths
        global_packages = standardize_paths (global_packages);
      endif
      global_packages = make_rel_paths (global_packages);
      save (global_list, "global_packages");
      installed_pkgs_lst = {local_packages{:}, global_packages{:}};
    else
      idx = setdiff (1:length (local_packages), packages_to_uninstall);
      local_packages = save_order ({local_packages{idx}, descriptions{:}});
      if (ispc)
        local_packages = standardize_paths (local_packages);
      endif
      if (! exist (fileparts (local_list), "dir")
          && ! mkdir (fileparts (local_list)))
        error ("Octave:pkg:install:local-dir", ...
               "pkg: Could not create directory for local package configuration");
      endif
      save (local_list, "local_packages");
      installed_pkgs_lst = {local_packages{:}, global_packages{:}};
    endif
  catch
    ## Something went wrong, delete tmpdirs.
    for i = 1:length (tmpdirs)
      sts = rmdir (tmpdirs{i}, "s");
    endfor
    for i = 1:length (descriptions)
      sts = rmdir (descriptions{i}.dir, "s");
    endfor
    if (global_install)
      printf ("error: couldn't append to %s\n", global_list);
    else
      printf ("error: couldn't append to %s\n", local_list);
    endif
    rethrow (lasterror ());
  end_try_catch

  ## All is well, let's clean up.
  for i = 1:length (tmpdirs)
    [status, msg] = rmdir (tmpdirs{i}, "s");
    if (status != 1 && isfolder (tmpdirs{i}))
      warning ("couldn't clean up after my self: %s\n", msg);
    endif
  endfor

  ## If there is a NEWS file, mention it.
  ## Check if desc exists too because it's possible to get to this point
  ## without creating it such as giving an invalid filename for the package
  if (exist ("desc", "var")
      && exist (fullfile (desc.dir, "packinfo", "NEWS"), "file"))
    printf (["For information about changes from previous versions " ...
             "of the %s package, run 'news %s'.\n"],
            desc.name, desc.name);
  endif

endfunction


function pkg = extract_pkg (nm, pat)

  mfile_encoding = __mfile_encoding__ ();
  if (strcmp (mfile_encoding, "system"))
    mfile_encoding = __locale_charset__ ();
  endif
  fid = fopen (nm, "rt", "n", mfile_encoding);
  pkg = "";
  if (fid >= 0)
    while (! feof (fid))
      ln = __u8_validate__ (fgetl (fid));
      if (ln > 0)
        t = regexp (ln, pat, "tokens");
        if (! isempty (t))
          pkg = [pkg "\n" t{1}{1}];
        endif
      endif
    endwhile
    if (! isempty (pkg))
      pkg = [pkg "\n"];
    endif
    fclose (fid);
  endif

endfunction


## Make sure the package contains the essential files.
function verify_directory (dir)

  needed_files = {"COPYING", "DESCRIPTION"};
  for f = needed_files
    if (! exist (fullfile (dir, f{1}), "file"))
      error ("package is missing file: %s", f{1});
    endif
  endfor

endfunction


function prepare_installation (desc, packdir)

  ## Is there a pre_install to call?
  if (exist (fullfile (packdir, "pre_install.m"), "file"))
    wd = pwd ();
    try
      cd (packdir);
      pre_install (desc);
      cd (wd);
    catch
      cd (wd);
      rethrow (lasterror ());
    end_try_catch
  endif

  ## If the directory "inst" doesn't exist, we create it.
  inst_dir = fullfile (packdir, "inst");
  if (! isfolder (inst_dir))
    [status, msg] = mkdir (inst_dir);
    if (status != 1)
      sts = rmdir (desc.dir, "s");
      error ("the 'inst' directory did not exist and could not be created: %s",
             msg);
    endif
  endif

endfunction


function copy_built_files (desc, packdir, verbose)

  src = fullfile (packdir, "src");
  if (! isfolder (src))
    return;
  endif

  ## Copy files to "inst" and "inst/arch" (this is instead of 'make install').
  files = fullfile (src, "FILES");
  instdir = fullfile (packdir, "inst");
  archdir = fullfile (packdir, "inst", getarch ());

  ## Get filenames.
  if (exist (files, "file"))
    [fid, msg] = fopen (files, "r");
    if (fid < 0)
      error ("couldn't open %s: %s", files, msg);
    endif
    filenames = char (fread (fid))';
    fclose (fid);
    if (filenames(end) == "\n")
      filenames(end) = [];
    endif
    filenames = strtrim (ostrsplit (filenames, "\n"));
    delete_idx = [];
    for i = 1:length (filenames)
      if (! all (isspace (filenames{i})))
        filenames{i} = fullfile (src, filenames{i});
      else
        delete_idx(end+1) = i;
      endif
    endfor
    filenames(delete_idx) = [];
  else
    m = dir (fullfile (src, "*.m"));
    oct = dir (fullfile (src, "*.oct"));
    mex = dir (fullfile (src, "*.mex"));
    tst = dir (fullfile (src, "*tst"));

    filenames = cellfun (@(x) fullfile (src, x),
                         {m.name, oct.name, mex.name, tst.name},
                         "uniformoutput", false);
  endif

  ## Split into architecture dependent and independent files.
  if (isempty (filenames))
    idx = [];
  else
    idx = cellfun ("is_architecture_dependent", filenames);
  endif
  archdependent = filenames(idx);
  archindependent = filenames(! idx);

  ## Copy the files.
  if (! all (isspace ([filenames{:}])))
      if (! isfolder (instdir))
        mkdir (instdir);
      endif
      if (! all (isspace ([archindependent{:}])))
        if (verbose)
          printf ("copyfile");
          printf (" %s", archindependent{:});
          printf (" %s\n", instdir);
        endif
        [status, output] = copyfile (archindependent, instdir);
        if (status != 1)
          sts = rmdir (desc.dir, "s");
          error ("Couldn't copy files from 'src' to 'inst': %s", output);
        endif
      endif
      if (! all (isspace ([archdependent{:}])))
        if (verbose)
          printf ("copyfile");
          printf (" %s", archdependent{:});
          printf (" %s\n", archdir);
        endif
        if (! isfolder (archdir))
          mkdir (archdir);
        endif
        [status, output] = copyfile (archdependent, archdir);
        if (status != 1)
          sts = rmdir (desc.dir, "s");
          error ("Couldn't copy files from 'src' to 'inst': %s", output);
        endif
      endif
  endif

endfunction


function dep = is_architecture_dependent (nm)

  persistent archdepsuffix = {".oct", ".mex", ".a", ".lib", ".so", ...
                              "tst", ".so.*", ".dll", "dylib"};

  dep = false;
  for i = 1 : length (archdepsuffix)
    ext = archdepsuffix{i};
    if (ext(end) == "*")
      isglob = true;
      ext(end) = [];
    else
      isglob = false;
    endif
    pos = strfind (nm, ext);
    if (pos)
      if (! isglob && (length (nm) - pos(end) != length (ext) - 1))
        continue;
      endif
      dep = true;
      break;
    endif
  endfor

endfunction


function copy_files (desc, packdir, global_install)

  ## Create the installation directory.
  if (! isfolder (desc.dir))
    [status, output] = mkdir (desc.dir);
    if (status != 1)
      error ("couldn't create installation directory %s : %s",
             desc.dir, output);
    endif
  endif

  octfiledir = getarchdir (desc);

  ## Copy the files from "inst" to installdir.
  instdir = fullfile (packdir, "inst");
  if (! dirempty (instdir))
    [status, output] = copyfile (fullfile (instdir, "*"), desc.dir);
    if (status != 1)
      sts = rmdir (desc.dir, "s");
      error ("couldn't copy files to the installation directory");
    endif
    if (isfolder (fullfile (desc.dir, getarch ()))
        && ! is_same_file (fullfile (desc.dir, getarch ()), octfiledir))
      if (! isfolder (octfiledir))
        ## Can be required to create up to three levels of dirs.
        octm1 = fileparts (octfiledir);
        if (! isfolder (octm1))
          octm2 = fileparts (octm1);
          if (! isfolder (octm2))
            octm3 = fileparts (octm2);
            if (! isfolder (octm3))
              [status, output] = mkdir (octm3);
              if (status != 1)
                sts = rmdir (desc.dir, "s");
                error ("couldn't create installation directory %s : %s",
                       octm3, output);
              endif
            endif
            [status, output] = mkdir (octm2);
            if (status != 1)
              sts = rmdir (desc.dir, "s");
              error ("couldn't create installation directory %s : %s",
                     octm2, output);
            endif
          endif
          [status, output] = mkdir (octm1);
          if (status != 1)
            sts = rmdir (desc.dir, "s");
            error ("couldn't create installation directory %s : %s",
                   octm1, output);
          endif
        endif
        [status, output] = mkdir (octfiledir);
        if (status != 1)
          sts = rmdir (desc.dir, "s");
          error ("couldn't create installation directory %s : %s",
                 octfiledir, output);
        endif
      endif
      [status, output] = movefile (fullfile (desc.dir, getarch (), "*"),
                                   octfiledir);
      sts = rmdir (fullfile (desc.dir, getarch ()), "s");

      if (status != 1)
        sts = rmdir (desc.dir, "s");
        sts = rmdir (octfiledir, "s");
        error ("couldn't copy files to the installation directory");
      endif
    endif

  endif

  ## Create the "packinfo" directory.
  packinfo = fullfile (desc.dir, "packinfo");
  [status, msg] = mkdir (packinfo);
  if (status != 1)
    sts = rmdir (desc.dir, "s");
    sts = rmdir (octfiledir, "s");
    error ("couldn't create packinfo directory: %s", msg);
  endif

  packinfo_copy_file ("DESCRIPTION", "required", packdir, packinfo, desc, octfiledir);
  packinfo_copy_file ("COPYING", "required", packdir, packinfo, desc, octfiledir);
  packinfo_copy_file ("CITATION", "optional", packdir, packinfo, desc, octfiledir);
  packinfo_copy_file ("NEWS", "optional", packdir, packinfo, desc, octfiledir);
  packinfo_copy_file ("ONEWS", "optional", packdir, packinfo, desc, octfiledir);
  packinfo_copy_file ("ChangeLog", "optional", packdir, packinfo, desc, octfiledir);

  ## Is there an INDEX file to copy or should we generate one?
  index_file = fullfile (packdir, "INDEX");
  if (exist (index_file, "file"))
    packinfo_copy_file ("INDEX", "required", packdir, packinfo, desc, octfiledir);
  else
    try
      write_index (desc, fullfile (packdir, "inst"),
                   fullfile (packinfo, "INDEX"), global_install);
    catch
      sts = rmdir (desc.dir, "s");
      sts = rmdir (octfiledir, "s");
      rethrow (lasterror ());
    end_try_catch
  endif

  ## Is there an 'on_uninstall.m' to install?
  packinfo_copy_file ("on_uninstall.m", "optional", packdir, packinfo, desc, octfiledir);

  ## Is there a doc/ directory that needs to be installed?
  docdir = fullfile (packdir, "doc");
  if (isfolder (docdir) && ! dirempty (docdir))
    [status, output] = copyfile (docdir, desc.dir);
  endif

  ## Is there a bin/ directory that needs to be installed?
  ## FIXME: Need to treat architecture dependent files in bin/
  bindir = fullfile (packdir, "bin");
  if (isfolder (bindir) && ! dirempty (bindir))
    [status, output] = copyfile (bindir, desc.dir);
  endif

endfunction


function packinfo_copy_file (filename, requirement, packdir, packinfo, desc, octfiledir)

  filepath = fullfile (packdir, filename);
  if (! exist (filepath, "file") && strcmpi (requirement, "optional"))
    ## do nothing, it's still OK
  else
    [status, output] = copyfile (filepath, packinfo);
    if (status != 1)
      sts = rmdir (desc.dir, "s");
      sts = rmdir (octfiledir, "s");
      error ("Couldn't copy %s file: %s", filename, output);
    endif
  endif

endfunction


## Create an INDEX file for a package that doesn't provide one.
##   'desc'  describes the package.
##   'dir'   is the 'inst' directory in temporary directory.
##   'index_file' is the name (including path) of resulting INDEX file.
function write_index (desc, dir, index_file, global_install)

  ## Get names of functions in dir
  [files, err, msg] = readdir (dir);
  if (err)
    error ("couldn't read directory %s: %s", dir, msg);
  endif

  ## Get classes in dir
  class_idx = find (strncmp (files, '@', 1));
  for k = 1:length (class_idx)
    class_name = files {class_idx(k)};
    class_dir = fullfile (dir, class_name);
    if (isfolder (class_dir))
      [files2, err, msg] = readdir (class_dir);
      if (err)
        error ("couldn't read directory %s: %s", class_dir, msg);
      endif
      files2 = strcat (class_name, filesep (), files2);
      files = [files; files2];
    endif
  endfor

  ## Check for architecture dependent files.
  tmpdir = getarchdir (desc);
  if (isfolder (tmpdir))
    [files2, err, msg] = readdir (tmpdir);
    if (err)
      error ("couldn't read directory %s: %s", tmpdir, msg);
    endif
    files = [files; files2];
  endif

  functions = {};
  for i = 1:length (files)
    file = files{i};
    lf = length (file);
    if (lf > 2 && strcmp (file(end-1:end), ".m"))
      functions{end+1} = file(1:end-2);
    elseif (lf > 4 && strcmp (file(end-3:end), ".oct"))
      functions{end+1} = file(1:end-4);
    endif
  endfor

  ## Does desc have a categories field?
  if (! isfield (desc, "categories"))
    error ("the DESCRIPTION file must have a Categories field, when no INDEX file is given");
  endif
  categories = strtrim (strsplit (desc.categories, ","));
  if (length (categories) < 1)
    error ("the Category field is empty");
  endif

  ## Write INDEX.
  fid = fopen (index_file, "w");
  if (fid == -1)
    error ("couldn't open %s for writing", index_file);
  endif
  fprintf (fid, "%s >> %s\n", desc.name, desc.title);
  fprintf (fid, "%s\n", categories{1});
  fprintf (fid, "  %s\n", functions{:});
  fclose (fid);

endfunction


function create_pkgadddel (desc, packdir, nm, global_install)

  instpkg = fullfile (desc.dir, nm);
  instfid = fopen (instpkg, "at"); # append to support PKG_ADD at inst/
  ## If it is exists, most of the PKG_* file should go into the
  ## architecture dependent directory so that the autoload/mfilename
  ## commands work as expected.  The only part that doesn't is the
  ## part in the main directory.
  archdir = fullfile (getarchprefix (desc, global_install),
                      [desc.name "-" desc.version], getarch ());
  if (isfolder (getarchdir (desc)))
    archpkg = fullfile (getarchdir (desc), nm);
    archfid = fopen (archpkg, "at");
  else
    archpkg = instpkg;
    archfid = instfid;
  endif

  if (archfid >= 0 && instfid >= 0)
    if (ispc ())
      oct_glob = @__wglob__;
    else
      oct_glob = @glob;
    endif

    ## Search all dot-m files for PKG commands.
    lst = oct_glob (fullfile (packdir, "inst", "*.m"));
    for i = 1:length (lst)
      nam = lst{i};
      fwrite (instfid, extract_pkg (nam, ['^[#%][#%]* *' nm ': *(.*)$']));
    endfor

    ## Search all C++ source files for PKG commands.
    cc_lst = oct_glob (fullfile (packdir, "src", "*.cc"));
    cpp_lst = oct_glob (fullfile (packdir, "src", "*.cpp"));
    cxx_lst = oct_glob (fullfile (packdir, "src", "*.cxx"));
    lst = [cc_lst; cpp_lst; cxx_lst];
    for i = 1:length (lst)
      nam = lst{i};
      fwrite (archfid, extract_pkg (nam, ['^//* *' nm ': *(.*)$']));
      fwrite (archfid, extract_pkg (nam, ['^/\** *' nm ': *(.*) *\*/$']));
    endfor

    ## Add developer included PKG commands.
    packdirnm = fullfile (packdir, nm);
    if (exist (packdirnm, "file"))
      fid = fopen (packdirnm, "rt");
      if (fid >= 0)
        while (! feof (fid))
          ln = fgets (fid);
          if (ln > 0)
            fwrite (archfid, ln);
          endif
        endwhile
        fclose (fid);
      endif
    endif

    ## If the files is empty remove it.
    fclose (instfid);
    t = dir (instpkg);
    if (t.bytes <= 0)
      unlink (instpkg);
    endif

    if (instfid != archfid)
      fclose (archfid);
      t = dir (archpkg);
      if (t.bytes <= 0)
        unlink (archpkg);
      endif
    endif
  endif

endfunction


function archprefix = getarchprefix (desc, global_install)

  if (global_install)
    [~, archprefix] = default_prefix (global_install, desc);
  else
    archprefix = desc.dir;
  endif

endfunction


function finish_installation (desc, packdir, global_install)

  ## Is there a post-install to call?
  if (exist (fullfile (packdir, "post_install.m"), "file"))
    wd = pwd ();
    try
      cd (packdir);
      post_install (desc);
      cd (wd);
    catch
      cd (wd);
      sts = rmdir (desc.dir, "s");
      sts = rmdir (getarchdir (desc), "s");
      rethrow (lasterror ());
    end_try_catch
  endif

endfunction


function generate_lookfor_cache (desc)

  dirs = strtrim (ostrsplit (genpath (desc.dir), pathsep ()));
  if (ispc)
    dirs = cellfun (@canonicalize_file_name, dirs, "uniformoutput", false);
  endif
  for i = 1 : length (dirs)
    doc_cache_create (fullfile (dirs{i}, "doc-cache"), dirs{i});
  endfor

endfunction
