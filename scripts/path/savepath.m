## Copyright (C) 2005, 2006, 2007 Bill Denney
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
## @deftypefn {Function File} {} savepath (@var{file})
## Save the current function search path to @var{file}.  If @var{file}
## is omitted, @file{~/.octaverc} is used.  If successful,
## @code{savepath} returns 0.
## @seealso{path, addpath, rmpath, genpath, pathdef, pathsep}
## @end deftypefn

## Author: Bill Denney <bill@givebillmoney.com>

##PKGADD: mark_as_command savepath

function varargout = savepath (savefile)

  retval = 1;

  beginstring = "## Begin savepath auto-created section, do not edit";
  endstring   = "## End savepath auto-created section";

  if (nargin == 0)
    savefile = ["~", filesep, ".octaverc"];
  endif

  ## parse the file if it exists to see if we should replace a section
  ## or create a section
  startline = 0;
  endline = 0;
  filelines = {};
  if (exist (savefile) == 2)
    ## read in all lines of the file
    [fid, msg] = fopen (savefile, "rt");
    if (fid < 0)
      error ("savepath: could not open savefile, %s: %s", savefile, msg);
    endif
    unwind_protect
      linenum = 0;
      while (linenum >= 0)
	result = fgetl (fid);
	if (isnumeric (result))
	  ## end at the end of file
	  linenum = -1;
	else
	  linenum = linenum + 1;
	  filelines{linenum} = result;
	  ## find the first and last lines if they exist in the file
	  if (strcmp (result, beginstring))
	    startline = linenum;
	  elseif (strcmp (result, endstring))
	    endline = linenum;
	  endif
	endif
      endwhile
    unwind_protect_cleanup
      closeread = fclose (fid);
      if (closeread < 0)
	error ("savepath: could not close savefile after reading, %s",
	       savefile);
      endif
    end_unwind_protect
  endif

  if (startline > endline || (startline > 0 && endline == 0))
    error ("savepath: unable to parse file, %s", savefile);
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
  elseif (endline == length(filelines))
    pre = filelines(1:startline-1);
    post = {};
  else
    ## insert in the middle
    pre = filelines(1:startline-1);
    post = filelines(endline+1:end);
  endif

  ## write the results
  [fid, msg] = fopen (savefile, "wt");
  if (fid < 0)
    error ("savepath: unable to open file for writing, %s, %s", savefile, msg);
  endif
  unwind_protect
    for i = 1:length (pre)
      fprintf (fid, "%s\n", pre{i})
    endfor

    ## Use single quotes for PATH argument to avoid string escape
    ## processing.
    fprintf (fid, "%s\n  path ('%s');\n%s\n",
	     beginstring, path (), endstring);

    for i = 1:length (post)
      fprintf (fid, "%s\n", post{i});
    endfor
  unwind_protect_cleanup
    closeread = fclose (fid);
    if (closeread < 0)
      error ("savepath: could not close savefile after writing, %s", savefile);
    elseif (nargin == 0)
      warning ("savepath: current path saved to %s", savefile);
    endif
  end_unwind_protect

  retval = 0;

  if (nargout == 1)
    varargout{1} = retval;
  endif
  
endfunction  
