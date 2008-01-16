## Copyright (C) 2005, 2006, 2007 Bill Denney
## Copyright (C) 2007 Ben Abbott
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
## @deftypefn {Function File} {@var{val} =} __extractpath__ (@var{file})
## Extact the path information from the script/function @var{file},
## created by @file{savepath.m}. If @var{file} is omitted, 
## @file{~/.octaverc} is used.  If successful, @code{__extractpath__}
## returns the path specified in @var{file}.
## @seealso{path, addpath, rmpath, genpath, pathdef, savepath, pathsep}
## @end deftypefn

## Author: Ben Abbott <bpabbott@mac.com>

function specifiedpath = __extractpath__ (savefile)

  ## The majority of this code was borrowed from savepath.m

  beginstring = "## Begin savepath auto-created section, do not edit";
  endstring   = "## End savepath auto-created section";

  if (nargin == 0)
    savefile = tilde_expand ("~/.octaverc");
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
      error ("__extractpath__: could not open savefile, %s: %s", savefile, msg);
    endif
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
    closeread = fclose (fid);
    if (closeread < 0)
      error ("savepath: could not close savefile after reading, %s", savefile);
    endif
  endif

  ## extract the path specifiation
  if (startline > endline || (startline > 0 && endline == 0))
    error ("savepath: unable to parse file, %s", savefile);
  elseif startline > 0
    specifiedpath = filelines(startline:endline);
  else
    specifiedpath = {};
  endif

endfunction  
