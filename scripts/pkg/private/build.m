########################################################################
##
## Copyright (C) 2016-2023 The Octave Project Developers
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

  if (! isfolder (builddir))
    warning ("creating build directory %s", builddir);
    [status, msg] = mkdir (builddir);
    if (status != 1)
      error ("could not create installation directory: %s", msg);
    endif
  endif

  for i = 1:numel (tarballs)
    filelist = unpack (tarballs{i}, builddir);

    ## We want the path for the package root but we can't assume that
    ## exists in the filelist (see patch #9030).  So we deduce it from
    ## the path of the DESCRIPTION file (smallest in case there's another
    ## file named DESCRIPTION somewhere).
    desc_pos = regexp (filelist, "DESCRIPTION$");
    desc_mask = ! cellfun ("isempty", desc_pos);
    [~, desc_r_idx] = min ([desc_pos{desc_mask}]);
    desc_path = fullfile (builddir, filelist(desc_mask){desc_r_idx});
    build_root = desc_path(1:end-12); # do not include the last filesep

    desc = get_description (desc_path);

    ## If there is no configure or Makefile within src/, there is nothing
    ## to do to prepare a "binary" package.  We only repackage to add more
    ## info to the tarball filename (version and arch).
    if (! exist (fullfile (build_root, "src", "configure"), "file")
        && ! exist (fullfile (build_root, "src", "Makefile"), "file"))
      arch_abi = "any-none";
    else
      arch_abi = getarch ();
      configure_make (desc, build_root, verbose);
      if exist (fullfile (build_root, "src", "configure"), "file")
        unlink (fullfile (build_root, "src", "configure"));
      endif
      if exist (fullfile (build_root, "src", "Makefile"), "file")
        unlink (fullfile (build_root, "src", "Makefile"));
      endif
    endif
    tar_name = [desc.name "-" desc.version "-" arch_abi ".tar"];
    tar_path = fullfile (builddir, tar_name);

    ## Figure out the directory name of the build.  Note that fileparts
    ## gets confused with the version string (the periods makes it think
    ## it's a file extension).
    [~, package_root, package_ext] = fileparts (build_root);
    package_root = [package_root, package_ext];

    tar (tar_path, package_root, builddir);
    gzip (tar_path, builddir);
    [~] = rmdir (build_root, "s");

    ## Currently does nothing because gzip() removes the original tar
    ## file but that should change in the future (bug #43431).
    [~] = unlink (tar_path);
  endfor

endfunction
