## Copyright (C) 2005-2015 Søren Hauberg
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
## @deftypefn {} {} build (@var{builddir}, @var{tarballs}, @var{verbose})
## Prepare binary packages from Octave source packages.
##
## Boils down to (for each in @var{tarballs}):
##
## @enumerate
## @item untar the tarball in @var{builddir};
##
## @item build anything necessary (configure and make);
##
## @item repackage specifying the build arch in the tarball filename.
## @end enumerate
##
## @end deftypefn

function build (builddir, tarballs, verbose)

  if (nargin != 3)
    print_usage ();
  endif

  if (! exist (builddir, "dir"))
    warning ("creating build directory %s", builddir);
    [status, msg] = mkdir (builddir);
    if (status != 1)
      error ("could not create installation directory: %s", msg);
    endif
  endif

  for i = 1:numel(tarballs)
    filelist = unpack (tarballs{i}, builddir);
    [~, root_idx] = min (cellfun ("numel", filelist));
    package_root = filelist{root_idx};
    build_root = fullfile (builddir, filelist{root_idx});

    desc = get_description (fullfile (build_root, "DESCRIPTION"));

    ## If there is no configure or Makefile within src/, there is nothing
    ## to do to prepare a "binary" package.  We only repackage to add more
    ## info on the filename (version and arch).
    if (! exist (fullfile (build_root, "src", "configure"), "file")
        && ! exist (fullfile (build_root, "src", "Makefile"), "file"))
      arch_abi = "any-none";
    else
      arch_abi = getarch ();
      configure_make (desc, build_root, verbose);
      unlink (fullfile (build_root, "src", "configure"));
      unlink (fullfile (build_root, "src", "Makefile"));
    endif
    tfile = [desc.name "-" desc.version "-" arch_abi ".tar"];

    init_wd = pwd ();
    unwind_protect
      chdir (builddir);
      try
        tar (tfile, package_root);
        rmdir (package_root, "s");
        gzip (tfile);
        unlink (tfile);
      catch
        warning ("failed to create and compress %s", tfile);
      end_try_catch
    unwind_protect_cleanup
      chdir (init_wd);
    end_unwind_protect
  endfor

endfunction
