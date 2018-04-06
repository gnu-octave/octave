## Copyright (C) 2018 JuanPi Carbajal
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

############################################################
## Test suite for pkg.m
## Tests are organized first by action, and then by options.
## All actions should be tested, and ideally all options are tested.
############################################################

%!shared old_prefix, old_archprefix, old_local_list, prefix, restorecfg, restorecache, rmtmpdir, mfile_pkg_name, mfile_pkg_zip
%!
%! ## Do all tests in a temporary directory
%! [old_prefix, old_archprefix] = pkg ("prefix");
%! restorecfg = onCleanup (@() pkg ("prefix", old_prefix, old_archprefix));
%! old_local_list = pkg ("local_list");
%! restorecache = onCleanup (@() pkg ("local_list", old_local_list));
%! prefix = tempname ();
%! [status] = mkdir (prefix);
%! if (! status)
%!   error ("pkg.tst: Could not create temporary directory for pkg testing");
%!   return;  # abort further testing
%! endif
%! pkg ("prefix", prefix, prefix);
%! pkg ("local_list", fullfile (prefix, "octave_packages"));
%! rmtmpdir = @onCleanup (@() confirm_recursive_rmdir (0, "local") && rmdir (prefix, "s"));
%!
%! ## Create zip file packages of testing directories in prefix directory
%! mfile_pkg_name = {"mfile_basic_test", "mfile_minimal_test"};
%! mfile_pkg_zip = fullfile (prefix, strcat (mfile_pkg_name, ".zip"));
%! for i = 1:numel (mfile_pkg_name)
%!   zip (mfile_pkg_zip{i}, mfile_pkg_name{i});
%! endfor

## Avoids printing to stdout when installing
%!function silent_pkg_install (varargin) 
%!  evalc (["pkg install", sprintf(" %s", varargin{:})]);
%!endfunction

## Action install/uninstall
%!test
%! for i = 1:numel (mfile_pkg_name)
%!   silent_pkg_install (mfile_pkg_zip{i});
%!   pkg ("uninstall", mfile_pkg_name{i});
%! endfor
%!
%!error pkg ("install", "nonexistent.zip")

# -local
%!test
%! for i = 1:numel (mfile_pkg_name)
%!   silent_pkg_install ("-local", mfile_pkg_zip{i});
%!   pkg ("uninstall", mfile_pkg_name{i});
%! endfor

# -forge (need check for options?)
## FIXME: Need test
# We do not test this yet ... fails if no internet connection
# use dataframe which is an mfile only package
#%!test
#%! silent_pkg_install ("-forge", "dataframe");
#%! pkg ("uninstall", "dataframe");

# -nodeps
## FIXME: Need test

# -verbose
## FIXME: Need test

## Action load/unload (within install/uninstall)
%!test
%! for i = 1:numel (mfile_pkg_name)
%!  name = mfile_pkg_name{i};
%!  silent_pkg_install ("-local", mfile_pkg_zip{i});
%!  unwind_protect
%!    pkg ("load", name);
%!    pkg ("unload", name);
%!  unwind_protect_cleanup
%!    pkg ("uninstall", name);
%!  end_unwind_protect
%! endfor
%!
%!error <package foobar is not installed> pkg ("load", "foobar");

# -nodeps
## FIXME: Need test

# -verbose
## FIXME: Need test

## Action list
%!test 
%! [user_packages, system_packages] = pkg ("list");

# -forge
#%!test
#%! oct_forge_pkgs = pkg ("list", "-forge");

## Action describe
%!test
%! silent_pkg_install ("-local", mfile_pkg_zip{1});
%! [desc, flag] = pkg ("describe", mfile_pkg_name{1});
%! ## FIXME: this only tests that the describe command runs,
%! ##        not that the output is in anyway correct.
%! pkg ("uninstall", mfile_pkg_name{1});

# -verbose
## FIXME: Need test

## Action prefix
%!test
%! pfx_old = pkg ("prefix");
%! unwind_protect
%!   pfx_new = pkg ("prefix", pwd ());
%!   assert (pfx_new, pwd ());
%! unwind_protect_cleanup
%!   pfx = pkg ("prefix", pfx_old);
%! end_unwind_protect

## Action build
## FIXME: Need test
# pkg build -verbose /tmp image-*

## Action rebuild
## FIXME: Need test
# pkg rebuild signal

## Future commands
%!error pkg ("whereis", "myfunc.m")
%!error pkg ("whereis", "-forge", "myfunc.m")

############################################################
## End of Tests
############################################################
