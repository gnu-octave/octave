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

## Test suite for pkg.m
# Test are organized first by action and then by options
# All actions should be tested and ideally all options are tested
#

%!shared mfile_pkg_name, mfile_pkg_zip, silent_pkg_install
%! # A cell with all the packages for testing
%! mfile_pkg_name = {'mfile_basic_test','mfile_minimal_test'};
%! for i = 1:numel(mfile_pkg_name)
%!  name = mfile_pkg_name{i};
%!  mfile_pkg_zip{i} = sprintf ('%s.zip', name);
%!  zip (mfile_pkg_zip{i}, name);
%! endfor
%! # Avoids printing to stdout when installing
%! silent_pkg_install =@(args) evalc (sprintf(['pkg install' ...
%!                                    repmat(' %s',1,numel(args))], args{:}));

## install/uninstall
%!error pkg('install','nonexistent.zip')
%!test
%! for i = 1:numel(mfile_pkg_name)
%!  silent_pkg_install (mfile_pkg_zip(i));
%!  pkg ('uninstall', mfile_pkg_name{i});
%! endfor

##
# -local
%!test
%! for i = 1:numel(mfile_pkg_name)
%!  silent_pkg_install ({'-local', mfile_pkg_zip{i}});
%!  pkg ('uninstall', mfile_pkg_name{i});
%! endfor

##
# -forge (need check for options?)
# We do not test this yet ... fails if no internet connection
# use dataframe which is a mfile only package
#%!test
#%! silent_pkg_install ({'-forge', 'dataframe'});
#%! pkg ('uninstall' , 'dataframe');

##
# -nodeps

##
# -verbose

## load/unload (within install/uninstall)
%!error pkg('load','notinstalled');
%!test
%! for i = 1:numel(mfile_pkg_name)
%!  name = mfile_pkg_name{i};
%!  silent_pkg_install ({'-local', mfile_pkg_zip{i}});
%!  unwind_protect
%!    pkg ('load', name);
%!    pkg ('unload', name);
%!  unwind_protect_cleanup
%!    pkg ('uninstall', name);
%!  end_unwind_protect
%! endfor

##
# -nodeps

##
# -verbose

## list
%!test 
%! [user_packages, system_packages] = pkg ('list');

##
# -forge
#%!test
#%! oct_forge_pkgs = pkg ('list', '-forge');

## describe
%!test
%! silent_pkg_install ({'-local', mfile_pkg_zip{1}});
%! [desc, flag] = pkg ('describe', mfile_pkg_name{1});
%! pkg ('uninstall', mfile_pkg_name{1});

##
# -verbose

## prefix
%!test
%! pfx_old = pkg ('prefix');
%! unwind_protect
%!   pfx_new = pkg('prefix', pwd ());
%!   if (! strcmp (pfx_new, pwd ()))
%!     error ()
%!   endif
%! unwind_protect_cleanup
%!   pfx = pkg ('prefix', pfx_old);
%! end_unwind_protect

## These commands need testing
# pkg build -verbose /tmp image-*
# pkg rebuild signal

## Future
%!error pkg ('whereis', 'myfunc.m')
%!error pkg ('whereis', '-forge', 'myfunc.m')

