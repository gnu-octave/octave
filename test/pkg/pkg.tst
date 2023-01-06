########################################################################
##
## Copyright (C) 2018-2023 The Octave Project Developers
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

############################################################
## Test suite for pkg.m
## Tests are organized first by action, and then by options.
## All actions should be tested, and ideally all options are tested.
############################################################

%!shared old_prefix, old_archprefix, old_local_list, old_global_list, prefix, restorecfg, restorecache, restoreglobalcache, rmtmpdir, mfile_pkg_name, mfile_pkg_tgz

%!function test_cleanup (prefix)
%! confirm_recursive_rmdir (0, "local");
%! sts = rmdir (prefix, "s");
%!endfunction

%!testif HAVE_Z
%! ## Do all tests in a temporary directory
%! [old_prefix, old_archprefix] = pkg ("prefix");
%! restorecfg = onCleanup (@() pkg ("prefix", old_prefix, old_archprefix));
%! old_local_list = pkg ("local_list");
%! restorecache = onCleanup (@() pkg ("local_list", old_local_list));
%! old_global_list = pkg ("global_list");
%! restoreglobalcache = onCleanup (@() pkg ("global_list", old_global_list));
%! prefix = tempname ();
%! [status] = mkdir (prefix);
%! if (! status)
%!   error ("pkg.tst: Could not create temporary directory for pkg testing");
%!   return;  # abort further testing
%! endif
%! pkg ("prefix", prefix, prefix);
%! pkg ("local_list", fullfile (prefix, "octave_packages"));
%! pkg ("global_list", fullfile (prefix, "octave_packages"));
%! rmtmpdir = @onCleanup (@() test_cleanup (prefix));
%!
%! ## Create tar.gz file packages of testing directories in prefix directory
%! mfile_pkg_name = {"mfile_basic_test", "mfile_minimal_test"};
%! mfile_pkg_tar = fullfile (prefix, strcat (mfile_pkg_name, ".tar"));
%! mfile_pkg_tgz = strcat (mfile_pkg_tar, ".gz");
%! for i = 1:numel (mfile_pkg_name)
%!   tar (mfile_pkg_tar{i}, mfile_pkg_name{i});
%!   gzip (mfile_pkg_tar{i});
%! endfor

## Avoids printing to stdout when installing
%!function silent_pkg_install (varargin)
%!  evalc (["pkg install", sprintf(" %s", varargin{:})]);
%!endfunction

## Action install/uninstall
%!testif HAVE_Z
%! for i = 1:numel (mfile_pkg_name)
%!   silent_pkg_install (mfile_pkg_tgz{i});
%!   if (isunix ())
%!     system (["chmod -Rf u+w '" prefix "'"]);   ## FIXME: Work around bug #53578
%!   endif
%!   pkg ("uninstall", mfile_pkg_name{i});
%! endfor
%!
%!error pkg ("install", "nonexistent.zip")

## -local
%!testif HAVE_Z
%! for i = 1:numel (mfile_pkg_name)
%!   silent_pkg_install ("-local", mfile_pkg_tgz{i});
%!   if (isunix)
%!     system (["chmod -Rf u+w '" prefix "'"]);   ## FIXME: Work around bug #53578
%!   endif
%!   pkg ("uninstall", mfile_pkg_name{i});
%! endfor

## -forge (need check for options?)
## FIXME: Need test
## We do not test this yet ... fails if no internet connection
## use dataframe which is an mfile only package
#%!test
#%! silent_pkg_install ("-forge", "dataframe");
#%! pkg ("uninstall", "dataframe");

## -nodeps
## FIXME: Need test

## -verbose
## FIXME: Need test

## Action load/unload (within install/uninstall)
%!testif HAVE_Z
%! save_default_options ("-binary", "local");
%! for i = 1:numel (mfile_pkg_name)
%!   name = mfile_pkg_name{i};
%!   silent_pkg_install ("-local", mfile_pkg_tgz{i});
%!   unwind_protect
%!     pkg ("load", name);
%!     pkg ("unload", name);
%!   unwind_protect_cleanup
%!     if (isunix)
%!       system (["chmod -Rf u+w '" prefix "'"]); ## FIXME: Work around bug #53578
%!     endif
%!     pkg ("uninstall", name);
%!   end_unwind_protect
%! endfor
%!
%!error <package foobar is not installed> pkg ("load", "foobar")

## -nodeps
## FIXME: Need test

## -verbose
## FIXME: Need test

## Action list
%!test
%! [user_packages, system_packages] = pkg ("list");

## -forge
#%!test
#%! oct_forge_pkgs = pkg ("list", "-forge");

## Action describe
%!testif HAVE_Z
%! silent_pkg_install ("-local", mfile_pkg_tgz{1});
%! [desc, flag] = pkg ("describe", mfile_pkg_name{1});
%! ## FIXME: this only tests that the describe command runs,
%! ##        not that the output is in anyway correct.
%! if (isunix)
%!   system (["chmod -Rf u+w '" prefix "'"]);     ## FIXME: Work around bug #53578
%! endif
%! pkg ("uninstall", mfile_pkg_name{1});

## -verbose
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
## pkg build -verbose /tmp image-*

## Action rebuild
## FIXME: Need test
## pkg rebuild signal

## Future commands
%!error pkg ("whereis", "myfunc.m")
%!error pkg ("whereis", "-forge", "myfunc.m")

############################################################
## End of Tests
############################################################
