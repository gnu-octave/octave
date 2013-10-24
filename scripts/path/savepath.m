## Copyright (C) 2005-2013 Bill Denney
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
## @deftypefn  {Function File} {} savepath ()
## @deftypefnx {Function File} {} savepath (@var{file})
## @deftypefnx {Function File} {@var{status} =} savepath (@dots{})
## Save the unique portion of the current function search path that is
## not set during Octave's initialization process to @var{file}.
## If @var{file} is omitted, @file{~/.octaverc} is used.  If successful,
## @code{savepath} returns 0.
## @seealso{path, addpath, rmpath, genpath, pathdef}
## @end deftypefn

## Author: Bill Denney <bill@givebillmoney.com>

function retval = savepath (file)

  ret = 1;

  beginstring = "## Begin savepath auto-created section, do not edit";
  endstring   = "## End savepath auto-created section";

  if (nargin == 0)
    file = fullfile ("~", ".octaverc");
  endif

  ## parse the file if it exists to see if we should replace an
  ## existing section or create a new section
  startline = endline = 0;
  filelines = {};
  if (exist (file) == 2)
    [fid, msg] = fopen (file, "rt");
    if (fid < 0)
      error ("savepath: could not open file, %s: %s", file, msg);
    endif
    unwind_protect
      linenum = 0;
      while (ischar (line = fgetl (fid)))
        filelines{++linenum} = line;
        ## find the first and last lines if they exist in the file
        if (strcmp (line, beginstring))
          startline = linenum;
        elseif (strcmp (line, endstring))
          endline = linenum;
        endif
      endwhile
    unwind_protect_cleanup
      closeread = fclose (fid);
      if (closeread < 0)
        error ("savepath: could not close file after reading, %s", file);
      endif
    end_unwind_protect
  endif

  if (startline > endline || (startline > 0 && endline == 0))
    error ("savepath: unable to parse file, %s", file);
  endif

  ## put the current savepath lines into the file
  if (isempty (filelines)
      || (startline == 1 && endline == length (filelines)))
    ## savepath is the entire file
    pre = post = {};
  elseif (endline == 0)
    ## drop the savepath statements at the end of the file
    pre = filelines;
    post = {};
  elseif (startline == 1)
    pre = {};
    post = filelines(endline+1:end);
  elseif (endline == length (filelines))
    pre = filelines(1:startline-1);
    post = {};
  else
    ## insert in the middle
    pre = filelines(1:startline-1);
    post = filelines(endline+1:end);
  endif

  ## write the results
  [fid, msg] = fopen (file, "wt");
  if (fid < 0)
    error ("savepath: unable to open file for writing, %s, %s", file, msg);
  endif
  unwind_protect
    for i = 1:length (pre)
      fprintf (fid, "%s\n", pre{i});
    endfor

    ## Remove the portion of the path defined via the command line
    ## and/or the environment.
    workingpath = parsepath (path);
    command_line_path = parsepath (command_line_path ());
    octave_path = parsepath (getenv ("OCTAVE_PATH"));
    pathdef = pathdef ();
    if (isempty (pathdef))
      ## This occurs when running octave via run-octave. In this instance
      ## the entire path is specified via the command line and pathdef()
      ## is empty.
      [~, n] = setdiff (workingpath, octave_path);
      default_path = command_line_path;
    else
      [~, n] = setdiff (workingpath, union (command_line_path, octave_path));
      default_path = parsepath (pathdef);
    endif
    ## This is the path we'd like to preserve when octave is run.
    path_to_preserve = workingpath (sort (n));

    ## Determine the path to Octave's user and sytem wide pkgs.
    [pkg_user, pkg_system] = pkg ("list");
    pkg_user_path = cell (1, numel (pkg_user));
    pkg_system_path = cell (1, numel (pkg_system));
    for n = 1:numel (pkg_user)
      pkg_user_path{n} = pkg_user{n}.archprefix;
    endfor
    for n = 1:numel (pkg_system)
      pkg_system_path{n} = pkg_system{n}.archprefix;
    endfor
    pkg_path = union (pkg_user_path, pkg_system_path);

    ## Rely on Octave's initialization to include the pkg path elements.
    if (! isempty (pkg_path))
      [~, n] = setdiff (path_to_preserve, strcat (pkg_path, ":"));
      path_to_preserve = path_to_preserve(sort (n));
    endif

    ## Split the path to be saved into two groups. Those path elements that
    ## belong at the beginning and those at the end.
    if (! isempty (default_path))
      n1 = find (strcmp (default_path{1}, path_to_preserve));
      n2 = find (strcmp (default_path{end}, path_to_preserve));
      n_middle = round (0.5*(n1+n2));
      [~, n] = setdiff (path_to_preserve, default_path);
      path_to_save = path_to_preserve(sort (n));
      ## Remove pwd
      path_to_save = path_to_save(! strcmp (path_to_save, ["." pathsep]));
      n = ones (size (path_to_save));
      for m = 1:numel (path_to_save)
        n(m) = find (strcmp (path_to_save{m}, path_to_preserve));
      endfor
      path_to_save_begin = path_to_save(n <= n_middle);
      path_to_save_end   = path_to_save(n > n_middle);
    else
      path_to_save_begin = path_to_preserve;
      path_to_save_end   = {};
    endif
    path_to_save_begin = cell2mat (path_to_save_begin);
    path_to_save_end   = cell2mat (path_to_save_end);

    ## Use single quotes for PATH argument to avoid string escape
    ## processing.  Since we are using single quotes around the arg,
    ## double any single quote characters found in the string.
    fprintf (fid, "%s\n", beginstring);
    if (! isempty (path_to_save_begin))
      n = find (path_to_save_begin != pathsep, 1, "last");
      fprintf (fid, "  addpath ('%s', '-begin');\n",
               strrep (path_to_save_begin(1:n), "'", "''"));
    endif
    if (! isempty (path_to_save_end))
      n = find (path_to_save_end != pathsep, 1, "last");
      fprintf (fid, "  addpath ('%s', '-end');\n",
               strrep (path_to_save_end(1:n), "'", "''"));
    endif
    fprintf (fid, "%s\n", endstring);

    for i = 1:length (post)
      fprintf (fid, "%s\n", post{i});
    endfor
  unwind_protect_cleanup
    closeread = fclose (fid);
    if (closeread < 0)
      error ("savepath: could not close savefile after writing, %s", file);
    elseif (nargin == 0)
      warning ("savepath: current path saved to %s", file);
    endif
  end_unwind_protect

  ret = 0;

  if (nargout > 0)
    retval = ret;
  endif

endfunction

function path_elements = parsepath (p)
  pat = sprintf ('([^%s]+[%s$])', pathsep, pathsep);
  path_elements = regexpi (strcat (p, pathsep), pat, "match");
endfunction

