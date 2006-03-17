## Copyright (C) 2004 John W. Eaton
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2, or (at your option)
## any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, write to the Free
## Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301, USA.

## -*- texinfo -*-
## @deftypefn {Function File} {} dir (@var{directory})
## @deftypefnx {Function File} {[@var{list}] =} dir (@var{directory})
## Display file listing for directory @var{directory}.  If a return
## value is requested, return a structure array with the fields
##
## @example
## @group
## name
## bytes
## date
## isdir
## statinfo
## @end group
## @end example
##
## @noindent
## in which @code{statinfo} is the structure returned from @code{stat}.
##
## If @var{directory} is not a directory, return information about the
## named @var{filename}.  @var{directory} may be a list of directories
## specified either by name or with wildcard characters (like * and ?)
## which will be expanded with glob.
## @seealso{ls, stat, readdir, glob, filesep}
## @end deftypefn

## Author: jwe

## XXX FIXME XXX -- this is quite slow for large directories, so perhaps
## it should be converted to C++.

function retval = dir (file)

  if (nargin == 0)
    file = ".";
  elseif (nargin > 1)
    usage ("dir (file)");
  endif

  ## Prep the retval.
  info = struct (zeros (0, 1));

  if (ischar (file))
    if (strcmp (file, "*"))
      file = ".";
    endif
    if (strcmp (file, "."))
      flst = {"."};
      nf = 1;
    else
      flst = glob (file);
      nf = length (flst);
    endif

    ## Determine the file list for the case where a single directory is
    ## specified.
    if (nf == 1)
      fn = flst{1};
      [st, err, msg] = lstat (fn);
      if (err < 0)
	warning ("dir: `lstat (%s)' failed: %s", fn, msg);
	nf = 0;
      elseif (st.modestr(1) == "d")
	flst = readdir (flst{1});
	nf = length (flst);
	for i = 1:nf
	  flst{i} = fullfile (fn, flst{i});
	endfor
      endif
    endif

    if (length (flst) > 0)
      ## Collect results.
      for i = nf:-1:1
	fn = flst{i};
	[st, err, msg] = lstat (fn);
	if (err < 0)
	  warning ("dir: `lstat (%s)' failed: %s", fn, msg);
	else
	  [dummy, fn, ext] = fileparts (fn);
	  fn = strcat (fn, ext);
	  info(i).name = fn;
	  info(i).date = strftime ("%d-%b-%Y %T", localtime (st.mtime));
	  info(i).bytes = st.size;
	  info(i).isdir = st.modestr(1) == "d";
	  info(i).statinfo = st;
	endif
      endfor
    endif

  else
    error ("dir: expecting directory or filename to be a char array");
  endif

  ## Return the output arguments.
  if (nargout > 0)
    ## Return the requested structure.
    retval = info;
  elseif (length (info) > 0)
    ## Print the structure to the screen.
    printf ("%s", list_in_columns ({info.name}));
  else
    warning ("dir: nonexistent file `%s'", file);
  endif

endfunction
