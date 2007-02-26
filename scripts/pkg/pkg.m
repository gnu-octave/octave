## Copyright (C) 2005 Søren Hauberg
## 
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2 of the License, or
## (at your option) any later version.
## 
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
## 
## You should have received a copy of the GNU General Public License
## along with this program; if not, write to the Free Software
## Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

## -*- texinfo -*-
## @deftypefn  {Command} pkg @var{command} @var{pkg_name}
## @deftypefnx {Command} pkg @var{command} @var{option} @var{pkg_name}
## This command interacts with the package manager. Different actions will
## be taking depending on the value of @var{command}.
##
## @table @samp
## @item install
## Install named packages.  For example,
## @example
## pkg install image-1.0.0.tar.gz
## @end example
## @noindent
## installs the package found in the file @code{image-1.0.0.tar.gz}.
##
## If @var{option} is @code{-nodeps} the package manager will disable the
## dependency checking. That way it is possible to install a package even
## if it depends on another package that's not installed on the system.
## @strong{Use this option with care.}
##
## If @var{option} is @code{-noauto} the package manager will not
## automatically load the installed package when starting Octave,
## even if the package requests that it is.
##
## If @var{option} is @code{-auto} the package manager will
## automatically load the installed package when starting Octave,
## even if the package requests that it isn't.
## @item uninstall
## Uninstall named packages.  For example,
## @example
## pkg uninstall image
## @end example
## @noindent
## removes the @code{image} package from the system. If another installed
## package depends on the @code{image} package an error will be issued.
## The package can be uninstalled anyway by using the @code{-nodeps} option.
## @item load
## Add named packages to the path. After loading a package it is
## possible to use the functions provided by the package. For example,
## @example
## pkg load image
## @end example
## @noindent
## adds the @code{image} package to the path. It is possible to load all
## installed packages at once with the command
## @example
## pkg load all
## @end example
## @item unload
## Removes named packages from the path. After unloading a package it is
## no longer possible to use the functions provided by the package.
## This command behaves like the @code{load} command.
## @item list
## Show a list of the currently installed packages. By requesting one or two
## output argument it is possible to get a list of the currently installed
## packages. For example,
## @example
## installed_packages = pkg list;
## @end example
## @noindent
## returns a cell array containing a structure for each installed package.
## The command
## @example
## [@var{user_packages}, @var{system_packages}] = pkg list
## @end example
## @noindent
## splits the list of installed packages into those who are installed by
## the current user, and those installed by the system administrator.
## @item prefix
## Set the installation prefix directory. For example,
## @example
## pkg prefix ~/my_octave_packages
## @end example
## @noindent
## sets the installation prefix to @code{~/my_octave_packages}.
## Packages will be installed in this directory.
##
## It is possible to get the current installation prefix by requesting an
## output argument.  For example,
## @example
## p = pkg prefix
## @end example
## @item local_list
## Set the file in which to look for information on the locally
## installed packages. Locally installed packages are those that are
## typically available only to the current user. For example
## @example
## pkg local_list ~/.octave_packages
## @end example
## It is possible to get the current value of local_list with the following
## @example
## pkg local_list
## @end example
## @item global_list
## Set the file in which to look for, for information on the globally
## installed packages. Globally installed packages are those that are
## typically available to all users. For example
## @example
## pkg global_list /usr/share/octave/octave_packages
## @end example
## It is possible to get the current value of global_list with the following
## @example
## pkg global_list
## @end example
## @end table
## @end deftypefn

## PKG_ADD: mark_as_command pkg
## PKG_ADD: pkg ("load", "auto");

function [local_packages, global_packages] = pkg(varargin)
    ## Installation prefix (XXX: what should these be on windows?)
    persistent prefix = -1;
    persistent local_list = tilde_expand("~/.octave_packages");
    persistent global_list = fullfile (OCTAVE_HOME (), "/share/octave/octave_packages");
    mlock;

    if (prefix == -1)
        if (issuperuser())
            prefix = fullfile (OCTAVE_HOME (), "/share/octave/packages");
        else
            prefix = "~/octave";
        endif
    endif
    prefix = tilde_expand(prefix);

    ## Handle input
    if (length(varargin) == 0 || !iscellstr(varargin))
        print_usage();
    endif
    files = {};
    deps = true;
    auto = 0;
    action = "none";
    for i = 1:length(varargin)
        switch (varargin{i})
            case "-nodeps"
                deps = false;
            case "-noauto"
	        auto = -1;
            case "-auto"
	        auto = 1;
            case {"list", "install", "uninstall", "load", "unload", ...
                  "prefix", "local_list", "global_list"}
                action = varargin{i};
            otherwise
                files{end+1} = varargin{i};
        endswitch
    endfor
    
    ## Take action
    switch (action)
        case "list"
            if (nargout == 0)
                installed_packages(local_list, global_list);
            elseif (nargout == 1)
                local_packages = installed_packages(local_list, global_list);
            elseif (nargout == 2)
                [local_packages, global_packages] = installed_packages(local_list, global_list);
            else
                error("Too many output arguments requested.");
            endif
        case "install"
            if (length(files) == 0)
                error("You must specify at least one filename when calling 'pkg install'");
            endif
            install(files, deps, auto, prefix, local_list, global_list);
        case "uninstall"
            if (length(files) == 0)
                error("You must specify at least one package when calling 'pkg uninstall'");
            endif
            uninstall(files, deps, local_list, global_list);
        case "load"
            if (length(files) == 0)
              error("You must specify at least one package, 'all' or 'auto' when calling 'pkg load'");
            endif
            load_packages(files, deps, local_list, global_list);
        case "unload"
            if (length(files) == 0)
                error("You must specify at least one package or 'all' when calling 'pkg unload'");
            endif
            unload_packages(files, deps, local_list, global_list);
        case "prefix"
            if (length(files) == 0 && nargout == 0)
                disp(prefix);
            elseif (length(files) == 0 && nargout == 1)
                local_packages = prefix;
            elseif (length(files) == 1 && nargout == 0 && ischar(files{1}))
                prefix = files{1};
                #if (!strcmp(prefix(end), "/")) prefix(end+1) = "/"; endif
            else
                error("You must specify a prefix directory, or request an output argument");
            endif
        case "local_list"
            if (length(files) == 0 && nargout == 0)
                disp(local_list);
            elseif (length(files) == 0 && nargout == 1)
                local_packages = local_list;
            elseif (length(files) == 1 && nargout == 0 && ischar(files{1}))
                local_list = files{1};
            else
                error("You must specify a local_list file, or request an output argument");
            endif
        case "global_list"
            if (length(files) == 0 && nargout == 0)
                disp(global_list);
            elseif (length(files) == 0 && nargout == 1)
                local_packages = global_list;
            elseif (length(files) == 1 && nargout == 0 && ischar(files{1}))
                global_list = files{1};
            else
                error("You must specify a global_list file, or request an output argument");
            endif
        otherwise
            error("You must specify a valid action for 'pkg'. See 'help pkg' for details");
    endswitch
endfunction

function auto = isautoload(desc)
  auto = false;
  if (isfield(desc{1},"autoload"))
    a = desc{1}.autoload;
    if ((isnumeric(a) && a > 0) || 
	(ischar(a) && (strcmp(tolower(a),"true") || 
		       strcmp(tolower(a),"on") || 
		       strcmp(tolower(a),"yes") ||
		       strcmp(tolower(a),"1"))))
      auto = true;
    endif
  endif
endfunction

function install(files, handle_deps, autoload, prefix, local_list, global_list)
    global_install = issuperuser();
 
    # Check that the directory in prefix exist. If it doesn't: create it!
    if (!exist(prefix, "dir"))
        warning("Creating installation directory %s", prefix);
        [status, msg] = mkdir(prefix);
        if (status != 1)
            error("Could not create installation directory: %s", msg);
        endif
    endif

    ## Get the list of installed packages
    [local_packages, global_packages] = installed_packages(local_list, 
							   global_list);
    installed_packages = {local_packages{:}, global_packages{:}};        
    
    if (global_install)
        packages = global_packages;
    else
        packages = local_packages;
    endif
   
    ## Uncompress the packages and read the DESCRIPTION files
    tmpdirs = packdirs = descriptions = {};
    try
        ## Unpack the package files and read the DESCRIPTION files
        files = glob(files);
        packages_to_uninstall = [];
        for i = 1:length(files)
            tgz = files{i};
            
            ## Create a temporary directory 
            tmpdir = tmpnam();
            tmpdirs{end+1} = tmpdir;
            [status, msg] = mkdir(tmpdir);
            if (status != 1)
                error("Couldn't create temporary directory: %s", msg);
            endif

            ## Uncompress the package
            untar(tgz, tmpdir);

            ## Get the name of the directories produced by tar
            [dirlist, err, msg] = readdir(tmpdir);
            if (err)
                error("Couldn't read directory produced by tar: %s", msg);
            endif

	    if (length(dirlist) > 3)
	      error("Bundles of packages are not allowed")
	    endif
            
            for k = 3:length(dirlist) # the two first entries of dirlist are "." and ".."
                packdir = fullfile(tmpdir, dirlist{k});
                packdirs{end+1} = packdir;

                ## Make sure the package contains necessary files
                verify_directory(packdir);

                ## Read the DESCRIPTION file
                filename = fullfile(packdir, "DESCRIPTION");
                desc = get_description(filename);

                ## Verify that package name corresponds with filename
                [dummy, nm] = fileparts(tgz); 
                if ((length(nm) >= length(desc.name)) &&
                    ! strcmp(desc.name,nm(1:length(desc.name))))
                    error("Package name '%s' doesn't correspond to its filename '%s'", desc.name, nm);
                endif

                ## Set default installation directory
                desc.dir = fullfile(prefix, [desc.name "-" desc.version]);
            
                ## Save desc
                descriptions{end+1} = desc;

                ## Are any of the new packages already installed?
                ## If so we'll remove the old version.
                for j = 1:length(packages)
                    if (strcmp(packages{j}.name, desc.name))
                        packages_to_uninstall(end+1) = j;
                    endif
                endfor
            endfor        
        endfor
    catch
        ## Something went wrong, delete tmpdirs
        for i = 1:length(tmpdirs)
            rm_rf(tmpdirs{i});
        endfor
        error(lasterr()(8:end));
    end_try_catch

    ## Check dependencies
    if (handle_deps)
        ok = true;
        error_text = "";
        for i = 1:length(descriptions)
            desc = descriptions{i};
            idx1 = complement(packages_to_uninstall, 1:length(installed_packages));
            idx2 = complement(i, 1:length(descriptions));
            pseudo_installed_packages = {installed_packages{idx1} descriptions{idx2}};
            bad_deps = get_unsatisfied_deps(desc, pseudo_installed_packages);
            ## Are there any unsatisfied dependencies?
            if (!isempty(bad_deps))
                ok = false;
                for i = 1:length(bad_deps)
                    dep = bad_deps{i};
                    error_text = [error_text "  " desc.name " needs " ...
                                  dep.package  " " dep.operator " " ...
                                  dep.version  "\n"];
                endfor
            endif
        endfor
        
        ## Did we find any unsatisfied dependencies?
        if (!ok)
            error("The following dependencies where unsatisfied:\n  %s", error_text);
        endif
    endif

    ## Prepare each package for installation
    try
        for i = 1:length(descriptions)
            desc = descriptions{i};
            pdir = packdirs{i};
            prepare_installation (desc, pdir);
            configure_make (desc, pdir);
        endfor
    catch
        ## Something went wrong, delete tmpdirs
        for i = 1:length(tmpdirs)
            rm_rf(tmpdirs{i});
        endfor
        error(lasterr()(8:end));
    end_try_catch

    ## Uninstall the packages that will be replaced
    try
        for i = packages_to_uninstall
            uninstall({installed_packages{i}.name}, false, local_list, 
            global_list);
        endfor
    catch
        ## Something went wrong, delete tmpdirs
        for i = 1:length(tmpdirs)
            rm_rf(tmpdirs{i});
        endfor
        error(lasterr()(8:end));
    end_try_catch

    ## Install each package
    try
        for i = 1:length(descriptions)
            desc = descriptions{i};
            pdir = packdirs{i};
            copy_files(desc, pdir);
            create_pkgadddel(desc, pdir, "PKG_ADD");
            create_pkgadddel(desc, pdir, "PKG_DEL");
            finish_installation (desc, pdir)
        endfor
    catch
        ## Something went wrong, delete tmpdirs
        for i = 1:length(tmpdirs)
            rm_rf(tmpdirs{i});
        endfor
        for i = 1:length(descriptions)
            rm_rf(descriptions{i}.dir);
        endfor
        error(lasterr()(8:end));
    end_try_catch

    ## Check if the installed directory is empty. If it is remove it
    ## from the list
    for i = length(descriptions):-1:1
      if (dirempty(descriptions{i}.dir,{"packinfo","doc"}))
        rm_rf(descriptions{i}.dir);
        descriptions(i) = [];
      endif
    endfor

    ## If the package requested that it is autoloaded, or the installer
    ## requested that it is, then mark the package as autoloaded.
    for i = length(descriptions):-1:1
      if (autoload > 0 || (autoload == 0 && isautoload(descriptions(i))))
	fclose(fopen(fullfile(descriptions{i}.dir, "packinfo", 
			      ".autoload"),"wt"));
      endif
    endfor

    ## Add the packages to the package list
    try
	    if (global_install)
            idx = complement(packages_to_uninstall, 1:length(global_packages));
	        global_packages = {global_packages{idx} descriptions{:}};
            save(global_list, "global_packages");
        else
            idx = complement(packages_to_uninstall, 1:length(local_packages));
	        local_packages = {local_packages{idx} descriptions{:}};
            save(local_list, "local_packages");
        endif
    catch
        ## Something went wrong, delete tmpdirs
        for i = 1:length(tmpdirs)
            rm_rf(tmpdirs{i});
        endfor
        for i = 1:length(descriptions)
            rm_rf(descriptions{i}.dir);
        endfor
        if (global_install)
            error("Couldn't append to %s: %s", global_list, lasterr()(8:end));
        else
            error("Couldn't append to %s: %s", local_list, lasterr()(8:end));
        endif
    end_try_catch

    ## All is well, let's clean up
    for i = 1:length(tmpdirs)
        [status, msg] = rm_rf(tmpdirs{i});
        if (status != 1)
            warning("Couldn't clean up after my self: %s\n", msg);
        endif
    endfor

    ## Add the newly installed packages to the path, so the user
    ## can begin usings them.
    if (length(descriptions) > 0)
      dirs = cell(1, length(descriptions));
      for i = 1:length(descriptions)
        dirs{i} = descriptions{i}.dir;
      endfor
      addpath(dirs{:});
    endif
endfunction

function uninstall(pkgnames, handle_deps, local_list, global_list)
    ## Get the list of installed packages
    [local_packages, global_packages] = installed_packages(local_list, 
							   global_list);
    if (issuperuser())
        installed_packages = {local_packages{:}, global_packages{:}};
    else
        installed_packages = local_packages;
    endif

    num_packages = length(installed_packages);
    delete_idx = [];
    for i = 1:num_packages
        cur_name = installed_packages{i}.name;
        if (any(strcmp(cur_name, pkgnames)))
            delete_idx(end+1) = i;
        endif
    endfor

    ## Are all the packages that should be uninstalled already installed?
    if (length(delete_idx) != length(pkgnames))
        if (issuperuser())
            ## Try again for a locally installed package
            installed_packages = local_packages

            num_packages = length(installed_packages);
            delete_idx = [];
            for i = 1:num_packages
                cur_name = installed_packages{i}.name;
                if (any(strcmp(cur_name, pkgnames)))
                    delete_idx(end+1) = i;
                endif
            endfor
            if (length(delete_idx) != length(pkgnames))
                ## XXX: We should have a better error message
                error("Some of the packages you want to uninstall are not installed.");
            endif
        else
            ## XXX: We should have a better error message
            error("Some of the packages you want to uninstall are not installed.");
        endif
    endif

    ## Compute the packages that will remain installed
    idx = complement(delete_idx, 1:num_packages);
    remaining_packages = {installed_packages{idx}};
    
    ## Check dependencies
    if (handle_deps)
        error_text = "";
        for i = 1:length(remaining_packages)
            desc = remaining_packages{i};
            bad_deps = get_unsatisfied_deps(desc, remaining_packages);
            
            ## Will the uninstallation break any dependencies?
            if (!isempty(bad_deps))
                for i = 1:length(bad_deps)
                    dep = bad_deps{i};
                    error_text = [error_text "  " desc.name " needs " ...
                                  dep.package  " " dep.operator " " ...
                                  dep.version  "\n"];
                endfor
            endif
        endfor

        if (! isempty(error_text))
            error("The following dependencies where unsatisfied:\n  %s", error_text);
        endif
    endif

    ## Delete the directories containing the packages
    for i = delete_idx
        desc = installed_packages{i};
        ## If an 'on_uninstall.m' exist, call it!
        if (exist(fullfile(desc.dir, "packinfo", "on_uninstall.m"), "file"))
            try
                wd = pwd();
                cd(fullfile(desc.dir, "packinfo"));
                on_uninstall(desc);
                cd(wd);
            catch
                # XXX: Should this rather be an error?
                warning("The 'on_uninstall' script retsurned the following error: %s", lasterr);
                cd(wd);
            end_try_catch
        endif
        ## Do the actual deletion
        rmpath(desc.dir);
        if (exist (desc.dir, "dir"))
            [status, msg] = rm_rf(desc.dir);
            if (status != 1)
                error("Couldn't delete directory %s: %s", desc.dir, msg);
            endif
        else
            warning("Directory %s previously lost", desc.dir);
        endif
    endfor

    ## Write a new ~/.octave_packages
    if (issuperuser())
        if (length(remaining_packages) == 0)
            unlink(global_list);
        else
            global_packages = remaining_packages;
            save(global_list, "global_packages");
        endif
    else
        if (length(remaining_packages) == 0)
            unlink(local_list);
        else
            local_packages = remaining_packages;
            save(local_list, "local_packages");
        endif
    endif
    
endfunction

##########################################################
##         A U X I L A R Y    F U N C T I O N S         ##
##########################################################

function prepare_installation(desc, packdir)
    ## Is there a pre_install to call?
    if (exist([packdir "pre_install.m"], "file"))
        wd = pwd();
        try
            cd(packdir);
            pre_install(desc); 
            cd(wd);
        catch
            cd(wd);
            error("The pre-install function returned the following error: %s", lasterr);
        end_try_catch
    endif

    ## If the directory "inst" doesn't exist, we create it
    if (!exist([packdir "inst"], "dir"))
        [status, msg] = mkdir([packdir "inst"]);
        if (status != 1)
            rm_rf(desc.dir);
            error("The 'inst' directory did not exist and could not be created: %s", msg);
        endif
    endif
endfunction

function configure_make (desc, packdir)   
	## Perform ./configure, make, make install in "src"
    if (exist(fullfile(packdir, "src"), "dir"))
        src = fullfile(packdir, "src");
        ## configure
        if (exist(fullfile(src, "configure"), "file"))
            [status, output] = system(["cd " src " ;./configure --prefix=" desc.dir]);
            if (status != 0)
                rm_rf(desc.dir);
                error("The configure script returned the following error: %s", output);
            endif
        endif

        ## make
        if (exist(fullfile(src, "Makefile"), "file"))
            [status, output] = system(["export INSTALLDIR=" desc.dir "; make -C " src]);
            if (status != 0)
                rm_rf(desc.dir);
                error("'make' returned the following error: %s", output);
            endif
            %# make install
            %[status, output] = system(["export INSTALLDIR=" desc.dir "; make install -C " src]);
            %if (status != 0)
            %    rm_rf(desc.dir);
            %    error("'make install' returned the following error: %s", output);
            %endif
        endif

        ## Copy files to "inst" (this is instead of 'make install')
        files = fullfile(src, "FILES");
        instdir = fullfile(packdir, "inst");
        if (exist(files, "file"))
            ## Get file names
            [fid, msg] = fopen(files, "r");
            if (fid < 0)
                error("Couldn't open %s: %s", files, msg);
            endif
            filenames = char(fread(fid))';
            fclose(fid);
	    if (filenames(end) == "\n")
	      filenames(end) = [];
	    endif
            ## Copy the files
            fn = split_by(filenames, "\n");
	    delete_idx =  [];
            for i = 1:length(fn)
	      if (!all(isspace(fn{i})))
                fn{i} = fullfile(src, fn{i});
	      else
		delete_idx(end+1) = i;
              endif
            endfor
	    fn(delete_idx) = [];
            filenames = sprintf("%s ", fn{:});
        else
            m = dir(fullfile(src, "*.m"));
            oct = dir(fullfile(src, "*.oct"));
            filenames = "";
            if (length(m) > 0)
                filenames = sprintf(fullfile(src, "%s "), m.name);
            endif
            if (length(oct) > 0)
                filenames = [filenames " " sprintf(fullfile(src, "%s "), oct.name)];
            endif
        endif
        filenames = split_by(filenames, " ");
            
        if (!all(isspace(filenames)))
            mkdir(instdir);
            [status, output] = copyfile(filenames, instdir);
            if (status != 1)
                rm_rf(desc.dir);
                error("Couldn't copy files from 'src' to 'inst': %s", output);
            endif
        endif
    endif
endfunction

function pkg = extract_pkg (nm, pat)
  fid = fopen (nm, "rt");
  pkg = "";
  if (fid >= 0)
    while (! feof(fid))
      ln = fgetl (fid);
      if (ln > 0)
	t = regexp(ln, pat, "tokens");
	if (!isempty(t))
          pkg = [pkg, "\n", t{1}{1}];
	endif
      endif
    endwhile
    if (!isempty(pkg))
      pkg = [pkg, "\n"];
    endif
    fclose (fid);
  endif
endfunction

function create_pkgadddel (desc, packdir, nm)
  pkg = fullfile(desc.dir, nm);
  fid = fopen(pkg, "wt");

  if (fid >= 0)
    ## Search all dot-m files for PKG commands
    lst = dir (fullfile(packdir, "inst", "*.m"));
    for i=1:length(lst)
      nam = fullfile(packdir, "inst", lst(i).name);
      fwrite (fid, extract_pkg (nam, ['^[#%][#%]* *' nm ': *(.*)$']));
    endfor

    ## Search all C++ source files for PKG commands
    lst = dir (fullfile(packdir, "src", "*.cc"));
    for i=1:length(lst)
      nam = fullfile(packdir, "src", lst(i).name);
      fwrite (fid, extract_pkg (nam, ['^//* *' nm ': *(.*)$']));
      fwrite (fid, extract_pkg (nam, ['^/\** *' nm ': *(.*) *\*/$']));
    endfor

    ## Add developer included PKG commands
    packdirnm = fullfile(packdir, nm);
    if (exist(packdirnm, "file"))
      fid2 = fopen(packdirnm,"rt");
      if (fid2 >= 0)
        while (! feof(fid2))
          ln = fgets (fid2);
          if (ln > 0)
            fwrite(fid, ln);
          endif
        endwhile
        fclose(fid2);
      endif
    endif
    fclose(fid);

    ## If the file is empty remove it
    t = dir (pkg);
    if (t.bytes <= 0)
      unlink (pkg);
    endif
  endif
endfunction

function copy_files (desc, packdir, bindir)
    ## Create the installation directory
    if (! exist (desc.dir, "dir"))
        [status, output] = mkdir (desc.dir);
        if (status != 1)
            error("Couldn't create installation directory %s : %s", 
            desc.dir, output);
        endif
    endif

    ## Copy the files from "inst" to installdir
    instdir = fullfile(packdir, "inst");
    if (!dirempty(instdir))
      [status, output] = copyfile(fullfile(instdir, "*"), desc.dir);
      if (status != 1)
          rm_rf(desc.dir);
          error("Couldn't copy files to the installation directory");
      endif
    endif

    ## Create the "packinfo" directory
    packinfo = fullfile(desc.dir, "packinfo");
    [status, msg] = mkdir (packinfo);
    if (status != 1)
        rm_rf(desc.dir);
        error("Couldn't create packinfo directory: %s", msg);
    endif

    ## Copy DESCRIPTION
    [status, output] = copyfile(fullfile(packdir, "DESCRIPTION"), packinfo);
    if (status != 1)
       rm_rf(desc.dir);
       error("Couldn't copy DESCRIPTION: %s", output);
    endif

    ## Copy COPYING
    [status, output] = copyfile(fullfile(packdir, "COPYING"), packinfo);
    if (status != 1)
       rm_rf(desc.dir);
       error("Couldn't copy COPYING: %s", output);
    endif

    ## If the file ChangeLog exists, copy it
    fChangeLog = fullfile(packdir, "ChangeLog");
    if (exist(fChangeLog, "file"))
        [status, output] = copyfile(fChangeLog, packinfo);
        if (status != 1)
            rm_rf(desc.dir);
            error("Couldn't copy ChangeLog file: %s", output);
        endif
    endif

    ## Is there an INDEX file to copy or should we generate one?
    fINDEX = fullfile(packdir, "INDEX");
    if (exist(fINDEX, "file"))
        [status, output] = copyfile(fINDEX, packinfo);
        if (status != 1)
            rm_rf(desc.dir);
            error("Couldn't copy INDEX file: %s", output);
        endif
    else
        try
            write_INDEX(desc, fullfile(packdir, "inst"), fullfile(packinfo, "INDEX"));
        catch
            rm_rf(desc.dir);
            error(lasterr);
        end_try_catch
    endif
    
    ## Is there an 'on_uninstall.m' to install?
    fon_uninstall = fullfile(packdir, "on_uninstall.m");
    if (exist(fon_uninstall, "file"))
        [status, output] = copyfile(fon_uninstall, packinfo);
        if (status != 1)
            rm_rf(desc.dir);
            error("Couldn't copy on_uninstall.m: %s", output);
        endif
    endif

    ## Is there a doc/ directory that needs to be installed
    docdir = fullfile(packdir, "doc");
    if (exist(docdir, "dir") && !dirempty(docdir))
       [status, output] = copyfile(docdir, desc.dir);
    endif

    ## Is there a bin/ directory that needs to be installed
    bindir = fullfile(packdir, "bin");
    if (exist(bindir, "dir") && !dirempty(bindir))
       [status, output] = copyfile(bindir, desc.dir);
    endif
endfunction

function finish_installation (desc, packdir)
    ## Is there a post-install to call?
    if (exist([packdir "post_install.m"], "file"))
        wd = pwd();
        try
            cd(packdir);
            post_install(desc);
            cd(wd);
        catch
            cd(wd);
            rm_rf(desc.dir);
            error("The post_install function returned the following error: %s", lasterr);
        end_try_catch
    endif
endfunction

function out = issuperuser()
    out = strcmp(getenv("USER"), "root");
endfunction

## This function makes sure the package contains the
## essential files.
function verify_directory(dir)
    needed_files = {"COPYING", "DESCRIPTION"};
    for f = needed_files
        if (!exist(fullfile(dir, f{1}), "file"))
            error("Package is missing file: %s", f{1});
        endif
    endfor
endfunction

## This function parses the DESCRIPTION file
function desc = get_description(filename)
    [fid, msg] = fopen(filename, "r");
    if (fid == -1)
        error("The DESCRIPTION file %s could not be read: %s", filename, msg);
    endif

    desc = struct();
    
    line = fgetl(fid);
    while (line != -1)
        ## Comments
        if (line(1) == "#")
            # Do nothing
        ## Continuation lines
        elseif (isspace(line(1)))
            if (exist("keyword", "var") && isfield(desc, keyword))
                desc.(keyword) = [desc.(keyword) " " rstrip(line)];
            endif
        ## Keyword/value pair
        else
            colon = find(line == ":");
            if (length(colon) == 0)
                disp("Skipping line.");
            else
                colon = colon(1);
                keyword = tolower(strip(line(1:colon-1)));
                value   = strip(line(colon+1:end));
                if (length(value) == 0)
                    fclose(fid);
                    error("The keyword %s have empty value", desc.keywords{end});
                endif
                desc.(keyword) = value;
            endif
        endif
        line = fgetl(fid);
    endwhile    
    fclose(fid);
    
    ## Make sure all is okay
    needed_fields = {"name", "version", "date", "title", ...
                     "author", "maintainer", "description"};
    for f = needed_fields
        if (!isfield(desc, f{1}))
            error("Description is missing needed field %s", f{1});
        endif
    endfor
    desc.version = fix_version(desc.version);
    if (isfield(desc, "depends"))
        desc.depends = fix_depends(desc.depends);
    else
        desc.depends = "";
    endif
    desc.name = tolower(desc.name);
endfunction

## Makes sure the version string v is a valid x.y.z version string
## Examples: "0.1" => "0.1.0", "monkey" => error(...)
function out = fix_version(v)
    dots = find(v == ".");
    if (length(dots) == 1)
        major = str2num(v(1:dots-1));
        minor = str2num(v(dots+1:end));
        if (length(major) != 0 && length(minor) != 0)
            out = sprintf("%d.%d.0", major, minor);
            return
        endif
    elseif (length(dots) == 2)
        major = str2num(v(1:dots(1)-1));
        minor = str2num(v(dots(1)+1:dots(2)-1));
        rev   = str2num(v(dots(2)+1:end));
        if (length(major) != 0 && length(minor) != 0 && length(rev) != 0)
            out = sprintf("%d.%d.%d", major, minor, rev);
            return
        endif
    endif
    error("Bad version string: %s", v);
endfunction

## Makes sure the depends field is of the right format.
## This function returns a cell of structures with the following fields:
##   package, version, operator
function deps_cell = fix_depends(depends)
    deps = split_by(tolower(depends), ",");
    deps_cell = cell(1, length(deps));
    
    ## For each dependency
    for i = 1:length(deps)
        dep = deps{i};
        lpar = find(dep == "(");
        rpar = find(dep == ")");
        ## Does the dependency specify a version
        ## Example: package(>= version)
        if (length(lpar) == 1 && length(rpar) == 1)
            package = tolower(strip(dep(1:lpar-1)));
            sub = dep( lpar(1)+1:rpar(1)-1 );
            parts = split_by(sub, " ");
            idx = [];
            for r = 1:size(parts,1)
                if (length(parts{r}) > 0)
                    idx(end+1) = r;
                endif
            endfor
             
            if (length(idx) != 2)
                error(["There's something wrong with the DESCRIPTION file. " ...
                       "The dependency %s has the wrong syntax.\n"], dep);
            endif
            operator = parts{idx(1)};
            if (!any(strcmp(operator, {">", ">=", "<=", "<", "=="})))
                error("Unsupported operator: %s", operator);
            endif
            version  = fix_version(parts{idx(2)});
             
        ## If no version is specified for the dependency
        ## we say that the version should be greater than 
        ## or equal to 0.0.0
        else
            package = tolower(strip(dep));
            operator = ">=";
            version  = "0.0.0";
        endif
        deps_cell{i} = struct("package", package, "operator", operator, "version", version);
    endfor
endfunction

## Strips the text of spaces from the right
## Example: "  hello world  " => "  hello world" (XXX: is this the same as deblank?)
function text = rstrip(text)
    chars = find(!isspace(text));
    if (length(chars) > 0)
        text = text(chars(1):end); # XXX: shouldn't it be text = text(1:chars(end));
    else
        text = "";
    endif
endfunction

## Strips the text of spaces from the left and the right
## Example: "  hello world  " => "hello world"
function text = strip(text)
    chars = find(!isspace(text));
    if (length(chars) > 0)
        text = text(chars(1):chars(end));
    else
        text = "";
    endif
endfunction

## Splits the text into a cell array of strings by sep
## Example: "A, B" => {"A", "B"} (with sep = ",")
function out = split_by(text, sep)
    text_matrix = split(text, sep);
    num_words = size(text_matrix,1);
    out = cell(num_words, 1);
    for i = 1:num_words
        out{i} = strip(text_matrix(i, :));
    endfor
endfunction

## Creates an INDEX file for a package that doesn't provide one.
##   'desc'  describes the package.
##   'dir'   is the 'inst' direcotyr in temporary directory.
##   'INDEX' is the name (including path) of resulting INDEX file.
function write_INDEX(desc, dir, INDEX)
    ## Get names of functions in dir
    [files, err, msg] = readdir(dir);
    if (err)
        error("Couldn't read directory %s: %s", dir, msg);
    endif
    
    functions = {};
    for i = 1:length(files)
        file = files{i};
        lf = length(file);
        if (lf > 2 && strcmp( file(end-1:end), ".m" ))
            functions{end+1} = file(1:end-2);
        elseif (lf > 4 && strcmp( file(end-3:end), ".oct" ))
            functions{end+1} = file(1:end-4);
        endif
    endfor
    
    ## Does desc have a categories field?
    if (!isfield(desc, "categories"))
        error("The DESCRIPTION file must have a Categories field, when no INDEX file is given.");
    endif
    categories = split_by(desc.categories, ",");
    if (length(categories) < 1)
        error("The Category field is empty.");
    endif
    
    ## Write INDEX
    fid = fopen(INDEX, "w");
    if (fid == -1)
        error("Couldn't open %s for writing.", INDEX);
    endif
    fprintf(fid, "%s >> %s\n", desc.name, desc.title);
    fprintf(fid, "%s\n", categories{1});
    fprintf(fid, "  %s\n", functions{:});
    fclose(fid);
endfunction

function bad_deps = get_unsatisfied_deps(desc, installed_packages)
    bad_deps = {};

    ## For each dependency
    for i = 1:length(desc.depends)
        dep = desc.depends{i};

        ## Is the current dependency Octave?
        if (strcmp(dep.package, "octave"))
            if (!compare_versions(OCTAVE_VERSION, dep.version, dep.operator))
                bad_deps{end+1} = dep;
            endif
        ## Is the current dependency not Octave?
        else
            ok = false;
            for i = 1:length(installed_packages)
                cur_name = installed_packages{i}.name;
                cur_version = installed_packages{i}.version;
                if (strcmp(dep.package, cur_name) && compare_versions(cur_version, dep.version, dep.operator))
                    ok = true;
                    break;
                endif
            endfor
            if (!ok)
                bad_deps{end+1} = dep;
            endif
        endif
    endfor
endfunction

function [out1, out2] = installed_packages(local_list, global_list)
    ## Get the list of installed packages
    try
        local_packages = load(local_list).local_packages;
    catch
        local_packages = {};
    end_try_catch
    try
	if (strcmp(local_list, global_list))
	    global_packages = {};
	else
            global_packages = load(global_list).global_packages;
        endif
    catch
        global_packages = {};
    end_try_catch
    installed_packages = {local_packages{:} global_packages{:}};        

    ## Eliminate duplicates in the installed package list.
    ## Locally installed packages take precedence
    dup = [];
    for i=1:length(installed_packages)
      if (find(dup,i))
	continue;
      endif
      for j=(i+1):length(installed_packages)
        if (find(dup,j))
	  continue;
        endif
        if (strcmp(installed_packages{i}.name,installed_packages{j}.name))
	  dup = [dup, j];
	endif
      endfor
    endfor
    if (! isempty(dup))
      installed_packages(dup) = [];
    endif  
  
    ## Should we return something?
    if (nargout == 2)
        out1 = local_packages;
        out2 = global_packages;
        return;
    elseif (nargout == 1)
        out1 = installed_packages;
        return;
    endif
    
    ## We shouldn't return something, so we'll print something
    num_packages = length(installed_packages);
    if (num_packages == 0)
        printf("No packages installed.\n");
        return;
    endif
    
    ## Compute the maximal lengths of name, version, and dir
    h1 = "Package Name";
    h2 = "Version";
    h3 = "Installation directory";
    max_name_length = length(h1); 
    max_version_length = length(h2);
    names = cell(num_packages,1); 
    for i = 1:num_packages
        max_name_length    = max(max_name_length, 
                                 length(installed_packages{i}.name));
        max_version_length = max(max_version_length,
                                 length(installed_packages{i}.version));
	names{i} = installed_packages{i}.name;
    endfor
    h1 = postpad (h1, max_name_length,' ');
    h2 = postpad (h2, max_version_length, ' ');;
    
    ## Print a header
    header = sprintf("%s | %s | %s\n", h1, h2, h3);
    printf(header);
    tmp = sprintf(repmat("-", 1, length(header)-1));
    tmp(length(h1)+2) = "+"; tmp(length(h1)+length(h2)+5) = "+";
    printf("%s\n", tmp);
    
    ## Print the packages
    format = sprintf("%%%ds | %%%ds | %%s\n", max_name_length, max_version_length);
    [dummy, idx] = sort(names);
    for i = 1:num_packages
        cur_name    = installed_packages{idx(i)}.name;
        cur_version = installed_packages{idx(i)}.version;
        cur_dir     = installed_packages{idx(i)}.dir;
        printf(format, cur_name, cur_version, cur_dir);
    endfor
endfunction

function load_packages(files, handle_deps, local_list, global_list)
    installed_packages = installed_packages(local_list, global_list);
    num_packages = length(installed_packages);
    
    ## Read package names and installdirs into a more convenient format
    pnames = pdirs = cell(1, num_packages);
    for i = 1:num_packages
        pnames{i} = installed_packages{i}.name;
        pdirs{i} = installed_packages{i}.dir;
        pdeps{i} = installed_packages{i}.depends;
    endfor
    
    ## load all
    if (length(files) == 1 && strcmp(files{1}, "all"))
        dirs = pdirs;
    ## load auto
    elseif (length(files) == 1 && strcmp(files{1}, "auto"))
      dirs = {};
      for i = 1:length(installed_packages)
        if (exist(fullfile(pdirs{i}, "packinfo", ".autoload"), "file"))
	  dirs{end+1} = pdirs{i};
        endif
      endfor
    ## load package_name1 ...
    else
        dirs = {};
        for i = 1:length(files)
            idx = strcmp(pnames, files{i});
            if (!any(idx))
                error("Package %s is not installed", files{i});
            endif
            dirs{end+1} = pdirs{idx};
            if (handle_deps)
                pdep = pdeps{idx};
                for j = 1:length(pdep)
                    depname = pdep{j}.package;
                    if (strcmp(depname, "octave")) continue; endif
                    idx = strcmp(pnames, depname);
                    if (!any(idx))
                        error("Package %s could not be loaded since it depends on %s", ...
                              files{i}, depname);
                    endif
                    dirs{end+1} = pdirs{idx};
                endfor
            endif
        endfor
        dirs = unique(dirs);
    endif

    ## Load the packages
    if (length(dirs) > 0)
        addpath(dirs{:});
    endif

    ## Add local binaries, if any, to the EXEC_PATH
    for i = 1:length(dirs)
       if (exist (fullfile(dirs{i}, "bin"), "dir"))
         EXEC_PATH ([fullfile(dirs{i}, "bin") ":" EXEC_PATH()]);
       endif
    endfor
endfunction

function unload_packages(files, handle_deps, local_list, global_list)
    installed_packages = installed_packages(local_list, global_list);
    num_packages = length(installed_packages);
    
    ## Read package names and installdirs into a more convenient format
    pnames = pdirs = cell(1, num_packages);
    for i = 1:num_packages
        pnames{i} = installed_packages{i}.name;
        pdirs{i} = installed_packages{i}.dir;
        pdeps{i} = installed_packages{i}.depends;
    endfor
    
    ## Get the current octave path
    p = split_by(path(), pathsep());

    ## unload all
    if (length(files) == 1 && strcmp(files{1}, "all"))
        dirs = pdirs;
    ## unload package_name1 ...
    else
        dirs = {};
        for i = 1:length(files)
            idx = strcmp(pnames, files{i});
            if (!any(idx))
                error("Package %s is not installed", files{i});
            endif
            dirs{end+1} = pdirs{idx};
        endfor
    endif

    ## Unload the packages
    for i = 1:length(dirs)
        d = dirs{i};
        idx = strcmp(p, d);
        if (any(idx))
            rmpath(d);
            # XXX: We should also check if we need to remove items from EXEC_PATH
        endif
    endfor
endfunction

function [status_out, msg_out] = rm_rf (dir)
  crr = confirm_recursive_rmdir ();
  unwind_protect
    confirm_recursive_rmdir (false);
    [status, msg] = rmdir (dir, "s");
  unwind_protect_cleanup
    confirm_recursive_rmdir (crr);
  end_unwind_protect
  if (nargout > 0)
    status_out = status;
  endif
  if (nargout > 1)
    msg_out = msg;
  endif
endfunction

function emp = dirempty (nm, ign)
  if (nargin < 2)
    ign = {".",".."};
  else
    ign = [{".",".."},ign];
  endif
  l = dir (nm);
  for i=1:length(l)
    found = false;
    for j=1:length(ign)
      if (strcmp(l(i).name,ign{j}))
        found = true;
        break;
      endif
    endfor
    if (!found)
      emp = false;
      return
    endif
  endfor
  emp = true;
  return;
endfunction
