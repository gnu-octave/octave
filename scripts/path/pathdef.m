## Copyright (C) 2005-2012 Bill Denney
## Copyright (C) 2007-2009 Ben Abbott
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
## @deftypefn {Function File} {@var{val} =} pathdef ()
## Return the default path for Octave.
## The path information is extracted from one of three sources.
## In order of preference, those are;
##
## @enumerate
## @item @file{~/.octaverc}
##
## @item @file{<octave-home>/@dots{}/<version>/m/startup/octaverc}
##
## @item Octave's path prior to changes by any octaverc.
## @end enumerate
## @seealso{path, addpath, rmpath, genpath, savepath, pathsep}
## @end deftypefn

function val = pathdef ()

  ## Locate the site octaverc file.
  pathdir = octave_config_info ("localstartupfiledir");
  site_octaverc = fullfile (pathdir, "octaverc");

  ## Locate the user ~\.octaverc file.
  user_octaverc = fullfile ("~", ".octaverc");

  ## Extract the specified paths from the site and user octaverc"s.
  site_path = __extractpath__ (site_octaverc);
  if (exist (user_octaverc, "file"))
    user_path = __extractpath__ (user_octaverc);
  else
    user_path = "";
  endif

  ## A path definition in the user octaverc has precedence over the
  ## site.

  if (! isempty (user_path))
    val = user_path;
  elseif (! isempty (site_path))
    val = site_path;
  else
    val = __pathorig__ ();
  endif

endfunction

## Extact the path information from the script/function @var{file},
## created by @file{savepath.m}.  If @var{file} is omitted,
## @file{~/.octaverc} is used.  If successful, @code{__extractpath__}
## returns the path specified in @var{file}.

## Author: Ben Abbott <bpabbott@mac.com>

function specifiedpath = __extractpath__ (savefile)

  ## The majority of this code was borrowed from savepath.m.
  ## FIXME -- is there some way to share the common parts instead of
  ## duplicating?

  beginstring = "## Begin savepath auto-created section, do not edit";
  endstring   = "## End savepath auto-created section";

  if (nargin == 0)
    savefile = tilde_expand ("~/.octaverc");
  endif

  ## Parse the file if it exists to see if we should replace a section
  ## or create a section.
  startline = 0;
  endline = 0;
  filelines = {};
  if (exist (savefile) == 2)
    ## read in all lines of the file
    [fid, msg] = fopen (savefile, "rt");
    if (fid < 0)
      error ("__extractpath__: could not open savefile, %s: %s", savefile, msg);
    endif
    unwind_protect
      linenum = 0;
      while (linenum >= 0)
        result = fgetl (fid);
        if (isnumeric (result))
          ## End at the end of file.
          linenum = -1;
        else
          linenum++;
          filelines{linenum} = result;
          ## Find the first and last lines if they exist in the file.
          if (strcmp (result, beginstring))
            startline = linenum + 1;
          elseif (strcmp (result, endstring))
            endline = linenum - 1;
          endif
        endif
      endwhile
    unwind_protect_cleanup
      closeread = fclose (fid);
      if (closeread < 0)
        error ("__extractpath__: could not close savefile after reading, %s",
               savefile);
      endif
    end_unwind_protect
  endif

  ## Extract the path specifiation.
  if (startline > endline || (startline > 0 && endline == 0))
    error ("__extractpath__: unable to parse file, %s", savefile);
  elseif (startline > 0)
    ## Undo doubling of single quote characters performed by savepath.
    specifiedpath = strrep (regexprep (cstrcat (filelines(startline:endline){:}),
                                       " *path *\\('(.*)'\\); *", "$1"),
                            "''", "'");
  else
    specifiedpath = "";
  endif

endfunction
