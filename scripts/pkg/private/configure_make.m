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
## @deftypefn {} {} configure_make (@var{desc}, @var{packdir}, @var{verbose})
## Undocumented internal function.
## @end deftypefn

function configure_make (desc, packdir, verbose)
  ## Perform ./configure, make, make install in "src".
  if (exist (fullfile (packdir, "src"), "dir"))
    src = fullfile (packdir, "src");
    octave_bindir = __octave_config_info__ ("bindir");
    ver = version ();
    ext = __octave_config_info__ ("EXEEXT");
    mkoctfile_program = fullfile (octave_bindir, ...
                                  sprintf ("mkoctfile-%s%s", ver, ext));
    octave_config_program = fullfile (octave_bindir, ...
                                      sprintf ("octave-config-%s%s", ver, ext));
    octave_binary = fullfile (octave_bindir, sprintf ("octave-%s%s", ver, ext));

    if (! exist (mkoctfile_program, "file"))
      __gripe_missing_component__ ("pkg", "mkoctfile");
    endif
    if (! exist (octave_config_program, "file"))
      __gripe_missing_component__ ("pkg", "octave-config");
    endif
    if (! exist (octave_binary, "file"))
      __gripe_missing_component__ ("pkg", "octave");
    endif

    if (verbose)
      mkoctfile_program = [mkoctfile_program " --verbose"];
    endif

    cenv = {"MKOCTFILE"; mkoctfile_program;
            "OCTAVE_CONFIG"; octave_config_program;
            "OCTAVE"; octave_binary;
            "INSTALLDIR"; desc.dir};
    scenv = sprintf ("%s='%s' ", cenv{:});

    ## Configure.
    if (exist (fullfile (src, "configure"), "file"))
      flags = "";
      if (isempty (getenv ("CC")))
        flags = [flags ' CC="' mkoctfile("-p", "CC") '"'];
      endif
      if (isempty (getenv ("CXX")))
        flags = [flags ' CXX="' mkoctfile("-p", "CXX") '"'];
      endif
      if (isempty (getenv ("AR")))
        flags = [flags ' AR="' mkoctfile("-p", "AR") '"'];
      endif
      if (isempty (getenv ("RANLIB")))
        flags = [flags ' RANLIB="' mkoctfile("-p", "RANLIB") '"'];
      endif
      cmd = ["cd '" src "'; " ...
             scenv "./configure --prefix=\"" desc.dir "\"" flags];
      [status, output] = shell (cmd, verbose);
      if (status != 0)
        rmdir (desc.dir, "s");
        disp (output);
        error ("pkg: error running the configure script for %s.", desc.name);
      endif
    endif

    ## Make.
    if (ispc ())
      jobs = 1;
    else
      jobs = nproc ("overridable");
    endif

    if (exist (fullfile (src, "Makefile"), "file"))
      [status, output] = shell (sprintf ("%s make --jobs %i --directory '%s'",
                                         scenv, jobs, src), verbose);
      if (status != 0)
        rmdir (desc.dir, "s");
        disp (output);
        error ("pkg: error running `make' for the %s package.", desc.name);
      endif
    endif
  endif
endfunction
