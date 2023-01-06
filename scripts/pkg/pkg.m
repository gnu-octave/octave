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
## @deftypefn  {} {} pkg @var{command} @var{pkg_name}
## @deftypefnx {} {} pkg @var{command} @var{option} @var{pkg_name}
## @deftypefnx {} {[@var{out1}, @dots{}] =} pkg (@var{command}, @dots{} )
## Manage or query packages (groups of add-on functions) for Octave.
##
## Packages can be installed globally (i.e., for all users of the system) or
## locally (i.e., for the current user only).
##
## Global packages are installed by default in a system-wide location.  This is
## usually a subdirectory of the folder where Octave itself is installed.
## Therefore, Octave needs write access to this folder to install global
## packages, which is usually only available when Octave is run with
## administrative privileges, such as when run as root (or superuser) on
## Unix-like systems, or run with elevated privileges ("Run as administrator")
## on Windows.
##
## In contrast, local packages are installed by default in the user's home
## directory (or user profile on Windows) and are only available to that
## specific user.  Usually, they can be installed without administrative
## privileges.
##
## When Octave is running with administrative privileges, @code{pkg} will
## install packages to the global package location by default.  Otherwise,
## packages will be installed to the local location by default.  The user can
## override this default installation location with optional arguments
## (@option{-local} or @option{-global}) as described below.  The currently
## used default package installation location can be queried with
## @code{pkg prefix}.
##
## For global and local packages, there are separate databases holding the
## information about the installed packages.  If some package is installed
## globally as well as locally, the local installation takes precedence over
## ("shadows") the global one.  Which (global or local) package installation is
## used can also be manipulated by using prefixes and/or using the
## @samp{local_list} input argument.  Using these mechanisms, several different
## releases of the same package can be installed side by side as well (but
## cannot be loaded simultaneously).
##
## Packages might depend on external software and/or other packages.  To be
## able to install such packages, these dependencies should be installed
## beforehand.  A package that depends on other package(s) can still be
## installed using the @option{-nodeps} flag.  The effects of unsatisfied
## dependencies on external software---like libraries---depends on the
## individual package.
##
## Packages must be loaded before they can be used.  When loading a package,
## Octave performs the following tasks:
## @enumerate
## @item
## If the package depends on other packages (and @code{pkg load} is called
## without the @option{-nodeps} option), the package is not loaded
## immediately.  Instead, those dependencies are loaded first (recursively if
## needed).
##
## @item
## When all dependencies are satisfied, the package's subdirectories are
## added to the search path.
## @end enumerate
##
## This load order leads to functions that are provided by dependencies being
## potentially shadowed by functions of the same name that are provided by
## top-level packages.
##
## Each time, a package is added to the search path, initialization script(s)
## for the package are automatically executed if they are provided by the
## package.
##
## Depending on the value of @var{command} and on the number of requested
## return arguments, @code{pkg} can be used to perform several tasks.
## Possible values for @var{command} are:
##
## @table @samp
##
## @item install
## Install named packages.  For example,
##
## @example
## pkg install image-1.0.0.tar.gz
## @end example
##
## @noindent
## installs the package found in the file @file{image-1.0.0.tar.gz}.  The
## file containing the package can be a URL, e.g.,
##
## @example
## pkg install 'http://somewebsite.org/image-1.0.0.tar.gz'
## @end example
##
## @noindent
## installs the package found in the given URL@.  This
## requires an internet connection and the cURL library.
##
## @noindent
## @emph{Security risk}: no verification of the package is performed
## before the installation.  It has the same security issues as manually
## downloading the package from the given URL and installing it.
##
## @noindent
## @emph{No support}: the GNU Octave community is not responsible for
## packages installed from foreign sites.  For support or for
## reporting bugs you need to contact the maintainers of the installed
## package directly (see the @file{DESCRIPTION} file of the package)
##
## The @var{option} variable can contain options that affect the manner
## in which a package is installed.  These options can be one or more of
##
## @table @code
## @item -nodeps
## The package manager will disable dependency checking.  With this option it
## is possible to install a package even when it depends on another package
## which is not installed on the system.  @strong{Use this option with care.}
##
## @item -local
## A local installation (package available only to current user) is forced,
## even if Octave is being run with administrative privileges.
##
## @item -global
## A global installation (package available to all users) is forced, even if
## Octave is not being run with administrative privileges.  The user must have
## write access to the global package store.
##
## @item -forge
## Install a package directly from the Octave Forge repository.  This
## requires an internet connection and the cURL library.
##
## @emph{Security risk}: no verification of the package is performed
## before the installation.  There are no signatures for packages, or
## checksums to confirm the correct file was downloaded.  It has the
## same security issues as manually downloading the package from the
## Octave Forge repository and installing it.
##
## @item -verbose
## The package manager will print the output of all commands as
## they are performed.
## @end table
##
## @item update
## Check installed Octave Forge packages against repository and update any
## outdated items.  Updated packages are installed either globally or locally
## depending on whether Octave is running with elevated privileges.  This
## requires an internet connection and the cURL library.
##
## Options for the install command and the names of individual packages to be
## checked for updates may be specified as a list following the update
## command.  If the @option{-local} or @option{-global} option is specified,
## @code{pkg update} limits the update check to the local or global installed
## packages, and installs updates in that same context.  For example,
##
## Update all packages:
##
## @example
## pkg update
## @end example
##
## Update all local packages:
##
## @example
## pkg update -local
## @end example
##
## Update certain packages, ignore dependencies, max verbosity:
##
## @example
## pkg update -verbose -nodeps image signal geometry
## @end example
##
## @noindent
## Updates for multiple packages are sorted alphabetically and not checked
## for dependencies affected by installation order.  If dependency order
## related @code{pkg update} failure occurs, use @code{pkg update -nodeps} to
## ignore dependencies, or @code{pkg install -forge <package_name>} to update
## individual packages manually.
##
## @item uninstall
## Uninstall named packages.  For example,
##
## @example
## pkg uninstall image
## @end example
##
## @noindent
## removes the @code{image} package from the system.  If another installed
## package depends on the @code{image} package an error will be issued.
## The package can be uninstalled anyway by using the @option{-nodeps} option.
##
## @item load
## Add named packages to the path.  After loading a package it is
## possible to use the functions provided by the package.  For example,
##
## @example
## pkg load image
## @end example
##
## @noindent
## adds the @code{image} package to the path.
##
## Note: When loading a package, @code{pkg} will automatically try to load
## any unloaded dependencies as well, unless the @option{-nodeps} flag has
## been specified.  For example,
##
## @example
## pkg load signal
## @end example
##
## @noindent
## adds the @code{signal} package and also tries to load its dependency: the
## @code{control} package.  Be aware that the functionality of package(s)
## loaded will probably be impacted by use of the @option{-nodeps} flag.  Even
## if necessary dependencies are loaded later, the functionality of top-level
## packages can still be affected because the optimal loading order may not
## have been followed.
##
## @item unload
## Remove named packages from the path.  After unloading a package it is
## no longer possible to use the functions provided by the package.  Trying
## to unload a package that other loaded packages still depend on will result
## in an error; no packages will be unloaded in this case.  A package can
## be forcibly removed with the @option{-nodeps} flag, but be aware that the
## functionality of dependent packages will likely be affected.  As when
## loading packages, reloading dependencies after having unloaded them with the
## @option{-nodeps} flag may not restore all functionality of the dependent
## packages as the required loading order may be incorrect.
##
## @item list
## Show the list of currently installed packages.  For example,
##
## @example
## pkg list
## @end example
##
## @noindent
## will produce a short report with the package name, version, and installation
## directory for each installed package.  Supply a package name to limit
## reporting to a particular package.  For example:
##
## @example
## pkg list image
## @end example
##
## If a single return argument is requested then @code{pkg} returns a cell
## array where each element is a structure with information on a single
## package.
##
## @example
## installed_packages = pkg ("list")
## @end example
##
## If two output arguments are requested @code{pkg} splits the list of
## installed packages into those which were installed by the current user,
## and those which were installed by the system administrator.
##
## @example
## [user_packages, system_packages] = pkg ("list")
## @end example
##
## The @qcode{"-forge"} option lists packages available at the Octave Forge
## repository.  This requires an internet connection and the cURL library.
## For example:
##
## @example
## oct_forge_pkgs = pkg ("list", "-forge")
## @end example
##
## @item describe
## Show a short description of installed packages.  With the option
## @qcode{"-verbose"} also list functions provided by the package.  For
## example,
##
## @example
## pkg describe -verbose
## @end example
##
## @noindent
## will describe all installed packages and the functions they provide.
## Display can be limited to a set of packages:
##
## @example
## @group
## ## describe control and signal packages
## pkg describe control signal
## @end group
## @end example
##
## If one output is requested a cell of structure containing the
## description and list of functions of each package is returned as
## output rather than printed on screen:
##
## @example
## desc = pkg ("describe", "secs1d", "image")
## @end example
##
## @noindent
## If any of the requested packages is not installed, @code{pkg} returns an
## error, unless a second output is requested:
##
## @example
## [desc, flag] = pkg ("describe", "secs1d", "image")
## @end example
##
## @noindent
## @var{flag} will take one of the values @qcode{"Not installed"},
## @qcode{"Loaded"}, or
## @qcode{"Not loaded"} for each of the named packages.
##
## @item prefix
## Set the installation prefix directory.  For example,
##
## @example
## pkg prefix ~/my_octave_packages
## @end example
##
## @noindent
## sets the installation prefix to @file{~/my_octave_packages}.
## Packages will be installed in this directory.
##
## It is possible to get the current installation prefix by requesting an
## output argument.  For example:
##
## @example
## pfx = pkg ("prefix")
## @end example
##
## The location in which to install the architecture dependent files can be
## independently specified with an addition argument.  For example:
##
## @example
## pkg prefix ~/my_octave_packages ~/my_arch_dep_pkgs
## @end example
##
## @item local_list
## Set the file in which to look for information on locally
## installed packages.  Locally installed packages are those that are
## available only to the current user.  For example:
##
## @example
## pkg local_list ~/.octave_packages
## @end example
##
## It is possible to get the current value of local_list with the following
##
## @example
## pkg local_list
## @end example
##
## @item global_list
## Set the file in which to look for information on globally
## installed packages.  Globally installed packages are those that are
## available to all users.  For example:
##
## @example
## pkg global_list /usr/share/octave/octave_packages
## @end example
##
## It is possible to get the current value of global_list with the following
##
## @example
## pkg global_list
## @end example
##
## @item build
## Build a binary form of a package or packages.  The binary file produced
## will itself be an Octave package that can be installed normally with
## @code{pkg}.  The form of the command to build a binary package is
##
## @example
## pkg build builddir image-1.0.0.tar.gz @dots{}
## @end example
##
## @noindent
## where @code{builddir} is the name of a directory where the temporary
## installation will be produced and the binary packages will be found.
## The options @option{-verbose} and @option{-nodeps} are respected, while
## all other options are ignored.
##
## @item rebuild
## Rebuild the package database from the installed directories.  This can
## be used in cases where the package database has been corrupted.
##
## @item test
## Perform the built-in self tests contained in all functions provided by
## the named packages.  For example:
##
## @example
## pkg test image
## @end example
##
## @end table
## @seealso{ver, news}
## @end deftypefn

function [local_packages, global_packages] = pkg (varargin)

  ## Installation prefix
  persistent user_prefix = false;
  persistent prefix = false;
  persistent archprefix = -1;
  persistent local_list = fullfile (user_config_dir (), "octave", ...
                                    __octave_config_info__ ("api_version"), ...
                                    "octave_packages");
  persistent global_list = fullfile (OCTAVE_HOME (), "share", "octave", ...
                                     "octave_packages");

  ## If user is superuser (posix) or the process has elevated rights (Windows),
  ## set global_install to true.
  if (ispc () && ! isunix ())
    global_install = __is_elevated_process__ ();
  else
    global_install = (geteuid () == 0);
  endif

  if (! user_prefix)
    [prefix, archprefix] = default_prefix (global_install);
    prefix = tilde_expand (prefix);
    archprefix = tilde_expand (archprefix);
  endif

  mlock ();

  confirm_recursive_rmdir (false, "local");

  ## valid actions in alphabetical order
  available_actions = {"build", "describe", "global_list",  "install", ...
                       "list", "load", "local_list", "prefix", "rebuild", ...
                       "test", "uninstall", "unload", "update"};

  ## Parse input arguments
  if (isempty (varargin) || ! iscellstr (varargin))
    print_usage ();
  endif
  files = {};
  deps = true;
  action = "none";
  verbose = false;
  octave_forge = false;
  for i = 1:numel (varargin)
    switch (varargin{i})
      case "-nodeps"
        deps = false;
      ## TODO completely remove these warnings after some releases.
      case "-noauto"
        warning ("Octave:deprecated-option",
                 ["pkg: autoload is no longer supported.  The -noauto "...
                  "option is no longer required."]);
      case "-auto"
        warning ("Octave:deprecated-option",
                 ["pkg: autoload is no longer supported.  Add a "...
                  "'pkg load ...' command to octaverc instead."]);
      case "-verbose"
        verbose = true;
        ## Send verbose output to pager immediately.  Change setting locally.
        page_output_immediately (true, "local");
      case "-forge"
        if (! __octave_config_info__ ("CURL_LIBS"))
          error ("pkg: can't download from Octave Forge without the cURL library");
        endif
        octave_forge = true;
      case "-local"
        global_install = false;
        if (! user_prefix)
          [prefix, archprefix] = default_prefix (global_install);
        endif
      case "-global"
        global_install = true;
        if (! user_prefix)
          [prefix, archprefix] = default_prefix (global_install);
        endif
      case available_actions
        if (! strcmp (action, "none"))
          error ("pkg: more than one action specified");
        endif
        action = varargin{i};
      otherwise
        files{end+1} = varargin{i};
    endswitch
  endfor

  if (octave_forge && ! any (strcmp (action, {"install", "list"})))
    error ("pkg: '-forge' can only be used with install or list");
  endif

  ## Take action
  switch (action)
    case "list"
      if (octave_forge)
        if (nargout)
          local_packages = list_forge_packages ();
        else
          list_forge_packages ();
        endif
      else
        if (nargout == 1)
          local_packages = installed_packages (local_list, global_list, files);
        elseif (nargout > 1)
          [local_packages, global_packages] = installed_packages (local_list,
                                                                  global_list,
                                                                  files);
        else
          installed_packages (local_list, global_list, files);
        endif
      endif

    case "install"
      if (isempty (files))
        error ("pkg: install action requires at least one filename");
      endif

      local_files = {};
      tmp_dir = tempname ();
      unwind_protect

        if (octave_forge)
          [urls, local_files] = cellfun ("get_forge_download", files,
                                         "uniformoutput", false);
          if (verbose)
            fprintf ("downloading tarball(s) from:%s\n", ...
                     sprintf ("\n- %s", urls{:}));
          endif
          [files, succ] = cellfun ("urlwrite", urls, local_files,
                                   "uniformoutput", false);
          succ = [succ{:}];
          if (! all (succ))
            i = find (! succ, 1);
            error ("pkg: could not download file %s from URL %s",
                   local_files{i}, urls{i});
          endif
        else
          ## If files do not exist, maybe they are not local files.
          ## Try to download them.
          not_local_files = cellfun (@(x) isempty (glob (x)), files);
          if (any (not_local_files))
            [success, msg] = mkdir (tmp_dir);
            if (success != 1)
              error ("pkg: failed to create temporary directory: %s", msg);
            endif

            for file = files(not_local_files)
              file = file{1};
              [~, fname, fext] = fileparts (file);
              tmp_file = fullfile (tmp_dir, [fname fext]);
              local_files{end+1} = tmp_file;
              looks_like_url = regexp (file, '^\w+://');
              if (looks_like_url)
                [~, success, msg] = urlwrite (file, local_files{end});
                if (success != 1)
                  error ("pkg: failed downloading '%s': %s", file, msg);
                endif
                ## Verify that download is a tarball,
                ## to protect against ISP DNS hijacking.
                ## FIXME: Need a test which does not rely on external OS.
                #{
                if (isunix ())
                  [ok, file_descr] = ...
                    system (sprintf ('file "%s" | cut -d ":" -f 2', ...
                                     local_files{end}));
                  if (! ok)
                    if (strfind (file_descr, "HTML"))
                      error (["pkg: Invalid package file downloaded from " ...
                              "%s\n" ...
                              "File is HTML, not a tar archive."], ...
                             file);
                    endif
                  else
                    ## Ignore: maybe something went wrong with the "file" call.
                  endif
                endif
                #}
              else
                looks_like_pkg_name = regexp (file, '^[\w-]+$');
                if (looks_like_pkg_name)
                  error (["pkg: file not found: %s.\n" ...
                          "This looks like an Octave Forge package name." ...
                          "  Did you mean:\n" ...
                          "       pkg install -forge %s"], ...
                         file, file);
                else
                  error ("pkg: file not found: %s", file);
                endif
              endif
              files{strcmp (files, file)} = local_files{end};

            endfor
          endif
        endif

        ## make sure the PREFIX and the ARCHPREFIX directories are created
        if (! isfolder (prefix))
          mkdir (prefix);
        endif
        if (! isfolder (archprefix))
          mkdir (archprefix);
        endif

        install (files, deps, prefix, archprefix, verbose, local_list,
                 global_list, global_install);

      unwind_protect_cleanup
        [~] = cellfun ("unlink", local_files);
        if (exist (tmp_dir, "file"))
          [~] = rmdir (tmp_dir, "s");
        endif
      end_unwind_protect

    case "uninstall"
      if (isempty (files))
        error ("pkg: uninstall action requires at least one package name");
      endif
      uninstall (files, deps, verbose, local_list, global_list, global_install);

    case "load"
      if (isempty (files))
        error ("pkg: load action requires at least one package name");
      endif
      load_packages (files, deps, local_list, global_list);

    case "unload"
      if (isempty (files))
        error ("pkg: unload action requires at least one package name");
      endif
      unload_packages (files, deps, local_list, global_list);

    case "prefix"
      if (isempty (files) && ! nargout)
        printf ("Installation prefix:             %s\n", prefix);
        printf ("Architecture dependent prefix:   %s\n", archprefix);
      elseif (isempty (files) && nargout)
        local_packages = prefix;
        global_packages = archprefix;
      elseif (numel (files) >= 1 && ischar (files{1}))
        prefix = tilde_expand (files{1});
        local_packages = prefix = make_absolute_filename (prefix);
        user_prefix = true;
        if (numel (files) >= 2 && ischar (files{2}))
          archprefix = make_absolute_filename (tilde_expand (files{2}));
        endif
      else
        error ("pkg: prefix action requires a directory input, or an output argument");
      endif

    case "local_list"
      if (isempty (files) && ! nargout)
        disp (local_list);
      elseif (isempty (files) && nargout)
        local_packages = local_list;
      elseif (numel (files) == 1 && ! nargout && ischar (files{1}))
        local_list = tilde_expand (files{1});
        if (! exist (local_list, "file"))
          try
            ## Force file to be created
            fclose (fopen (local_list, "wt"));
          catch
            error ("pkg: cannot create file %s", local_list);
          end_try_catch
        endif
        local_list = canonicalize_file_name (local_list);
      else
        error ("pkg: specify a local_list file, or request an output argument");
      endif

    case "global_list"
      if (isempty (files) && ! nargout)
        disp (global_list);
      elseif (isempty (files) && nargout)
        local_packages = global_list;
      elseif (numel (files) == 1 && ! nargout && ischar (files{1}))
        global_list = files{1};
        if (! exist (global_list, "file"))
          try
            ## Force file to be created
            fclose (fopen (files{1}, "wt"));
          catch
            error ("pkg: cannot create file %s", global_list);
          end_try_catch
        endif
        global_list = canonicalize_file_name (global_list);
      else
        error ("pkg: specify a global_list file, or request an output argument");
      endif

    case "rebuild"
      if (global_install)
        global_packages = rebuild (prefix, archprefix, global_list, files,
                                   verbose);
        global_packages = save_order (global_packages);
        if (ispc)
          ## On Windows ensure LFN paths are saved rather than 8.3 style paths
          global_packages = standardize_paths (global_packages);
        endif
        global_packages = make_rel_paths (global_packages);
        global_list_dir = fileparts (global_list);
        if (! isempty (global_list_dir) && ! exist (global_list_dir, "dir"))
          mkdir (global_list_dir);
        endif
        save (global_list, "global_packages");
        if (nargout)
          local_packages = global_packages;
        endif
      else
        local_packages = rebuild (prefix, archprefix, local_list, files,
                                  verbose);
        local_packages = save_order (local_packages);
        if (ispc)
          local_packages = standardize_paths (local_packages);
        endif
        local_list_dir = fileparts (local_list);
        if (! isempty (local_list_dir) && ! exist (local_list_dir, "dir"))
          mkdir (local_list_dir);
        endif
        save (local_list, "local_packages");
        if (! nargout)
          clear ("local_packages");
        endif
      endif

    case "build"
      if (numel (files) < 2)
        error ("pkg: build action requires build directory and at least one filename");
      endif
      build (files{1}, files(2:end), verbose);

    case "describe"
      ## FIXME: name of the output variables is inconsistent with their content
      if (nargout)
        [local_packages, global_packages] = describe (files, verbose,
                                                      local_list, global_list);
      else
        describe (files, verbose, local_list, global_list);
      endif

    case "update"
      installed_pkgs_lst = installed_packages (local_list, global_list);

      ## If -global or -local, limit updates to global or local list pkgs
      globalflag = any (strcmp (varargin, "-global"));
      localflag = any (strcmp (varargin, "-local"));
      if (globalflag || localflag)
        if (globalflag && localflag)
          error ("pkg: cannot specify both global and local options.");
        elseif (globalflag)
          [~, installed_pkgs_lst] = installed_packages (local_list, global_list);
        else
          [installed_pkgs_lst, ~] = installed_packages (local_list, global_list);
        endif
      else
        installed_pkgs_lst = installed_packages (local_list, global_list);
      endif

      ## Explicit list of packages to update, rather than all packages
      if (numel (files) > 0)
        update_lst = {};
        installed_names = cellfun (@(idx) idx.name, installed_pkgs_lst,
                                   "UniformOutput", false);
        for i = 1:numel (files)
          idx = find (strcmp (files{i}, installed_names), 1);
          if (isempty (idx))
            warning ("pkg: package %s is not installed - skipping update",
                     files{i});
          else
            update_lst = [ update_lst, installed_pkgs_lst(idx) ];
          endif
        endfor
        installed_pkgs_lst = update_lst;
      endif

      for i = 1:numel (installed_pkgs_lst)
        installed_pkg_name = installed_pkgs_lst{i}.name;
        installed_pkg_version = installed_pkgs_lst{i}.version;
        try
          forge_pkg_version = get_forge_pkg (installed_pkg_name);
        catch
          warning ("pkg: package %s not found on Octave Forge - skipping update\n",
                   installed_pkg_name);
          forge_pkg_version = "0";
        end_try_catch
        if (compare_versions (forge_pkg_version, installed_pkg_version, ">"))
          options_to_pass = varargin (strncmp (varargin, "-", 1));
          options_to_pass(end+1) = "-forge";
          feval (@pkg, "install", options_to_pass{:}, installed_pkg_name);
        endif
      endfor

    case "test"
      if (isempty (files))
        error ("pkg: test action requires at least one package name");
      endif
      ## Make sure the requested packages are loaded
      orig_path = path ();
      load_packages (files, deps, local_list, global_list);
      ## Test packages one by one
      installed_pkgs_lst = installed_packages (local_list, global_list, files);
      unwind_protect
        for i = 1:numel (installed_pkgs_lst)
          printf ("Testing functions in package '%s':\n", files{i});
          installed_pkgs_dirs = {installed_pkgs_lst{i}.dir, ...
                                 installed_pkgs_lst{i}.archprefix};
          installed_pkgs_dirs = ...
            installed_pkgs_dirs (! cellfun (@isempty, installed_pkgs_dirs));
          ## For local installs installed_pkgs_dirs contains the same subdirs
          installed_pkgs_dirs = unique (installed_pkgs_dirs);
          if (! isempty (installed_pkgs_dirs))
            ## FIXME invoke another test routine once that is available.
            ## Until then __run_test_suite__.m will do the job fine enough
            __run_test_suite__ ({installed_pkgs_dirs{:}}, {});
          endif
        endfor
      unwind_protect_cleanup
        ## Restore load path back to its original value before loading packages
        path (orig_path);
      end_unwind_protect

    otherwise
      error ("pkg: invalid action.  See 'help pkg' for available actions");
  endswitch

endfunction
