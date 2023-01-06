########################################################################
##
## Copyright (C) 2013-2023 The Octave Project Developers
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

## news() and citation() are very much alike.  They both do the same thing,
## just for different files.  This function does all the work.

function display_info_file (fcn, package, file)

  if (! ischar (package))
    error ("%s: PACKAGE must be a string", fcn);
  endif

  if (strcmpi (package, "octave"))
    octetcdir = __octave_config_info__ ("octetcdir");
    filepath  = fullfile (octetcdir, file);
  else
    installed = pkg ("list");
    names     = cellfun (@(x) x.name, installed, "UniformOutput", false);
    pos       = strcmpi (names, package);
    if (! any (pos))
      error ("%s: package '%s' is not installed", fcn, package);
    endif
    filepath = fullfile (installed{pos}.dir, "packinfo", file);
  endif

  if (! exist (filepath, "file"))
    if (strcmpi (package, "octave"))
      error ("%s: broken installation -- unable to locate %s file", fcn, file);
    else
      error ("%s: unable to locate %s file for package %s", fcn, file, package);
    endif
  endif

  fid = fopen (filepath, "r");
  while (ischar (line = fgets (fid)))
    puts (line);
  endwhile
  fclose (fid);

endfunction
