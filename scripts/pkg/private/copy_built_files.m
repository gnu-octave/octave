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
## @deftypefn {} {} copy_built_files (@var{desc}, @var{packdir}, @var{verbose})
## Undocumented internal function.
## @end deftypefn

function copy_built_files (desc, packdir, verbose)

  src = fullfile (packdir, "src");
  if (! exist (src, "dir"))
    return
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

    filenames = cellfun (@(x) fullfile (src, x),
                         {m.name, oct.name, mex.name},
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
      if (! exist (instdir, "dir"))
        mkdir (instdir);
      endif
      if (! all (isspace ([archindependent{:}])))
        if (verbose)
          printf ("copyfile");
          printf (" %s", archindependent{:});
          printf ("%s\n", instdir);
        endif
        [status, output] = copyfile (archindependent, instdir);
        if (status != 1)
          rmdir (desc.dir, "s");
          error ("Couldn't copy files from 'src' to 'inst': %s", output);
        endif
      endif
      if (! all (isspace ([archdependent{:}])))
        if (verbose)
          printf ("copyfile");
          printf (" %s", archdependent{:});
          printf (" %s\n", archdir);
        endif
        if (! exist (archdir, "dir"))
          mkdir (archdir);
        endif
        [status, output] = copyfile (archdependent, archdir);
        if (status != 1)
          rmdir (desc.dir, "s");
          error ("Couldn't copy files from 'src' to 'inst': %s", output);
        endif
      endif
  endif
endfunction
