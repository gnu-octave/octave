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
## Software Foundation, 59 Temple Place - Suite 330, Boston, MA
## 02111-1307, USA.

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
## named file.
## @var{filename}.
## @end deftypefn
##
## @seealso{stat}

## Author: jwe

## XXX FIXME XXX -- this is quite slow for large directories, so perhaps
## it should be converted to C++.

function retval = dir (file)

  if (nargin == 1)
    if (isstr (file))
      flst = glob (file);
      nf = length (flst);
      if (nf == 0)
	if (nargout > 0)
	  ## XXX FIXME XXX -- need a way to create an empty (0x1)
	  ## structure array.
	  retval = [];
	else
	  warning ("dir: nonexistent file \"%s\"", file);
	endif
      else
	nt = 0;
	len = zeros (nf, 1);
	finfo = cell (nf, 1);
	## Collect results.
	for i = nf:-1:1
	  fn = flst{i};
	  if (isstr (fn))
	    [st, err, msg] = lstat (fn);
	    if (err < 0)
	      warning ("dir: nonexistent file \"%s\"", fn);
	    else
	      if (st.modestr(1) == "d")
		lst = readdir (fn);
		n = length (lst);
		for j = n:-1:1
		  tfn = lst{j};
		  ## The lstat call seems to be the bottleneck for large
		  ## directories.
		  [st, err, msg] = lstat (strcat (fn, "/", tfn));
		  if (err < 0)
		    warning ("dir: stat failed for %s (%s)", tfn, msg);
		  else
		    info(j).name = tfn;
		    ## Generating the pretty time string also takes a
		    ## significant amount of time for large directories.
		    info(j).date = strftime ("%d-%b-%Y %T",
					     localtime (st.mtime));
		    info(j).bytes = st.size;
		    info(j).isdir = st.modestr(1) == "d";
		    info(j).statinfo = st;
		  endif
		endfor
	      else
		info.name = fn;
		info.date = strftime ("%d-%b-%Y %T", localtime (st.mtime));
		info.bytes = st.size;
		info.isdir = st.modestr(1) == "d";
		info.statinfo = st;
	      endif
	      nt += (len(i) = length (info));
	      finfo{i} = info;
	    endif
	  else
	    error ("dir: expecting filename argument to be a string");
	  endif
	endfor
	## Condense results to a single struct array.
	## XXX FIXME XXX -- need a way to create an empty (0x) struct
	## array in case the file is nonexistent.
	if (nt == 0)
	  file_list = [];
	else
	  off = 1;
	  for i = 1:nf
	    tlen = len(i);
	    file_list(off:off+tlen-1) = finfo{i};
	    off += tlen;
	  endfor
	endif
	if (nargout > 0)
	  if (length (file_list) > 0)
	    retval = file_list;
	  else
	    retval = [];
	  endif
	else
	  ## XXX FIXME XXX -- need a way to neatly list these in columns.
	  for i = 1:nt
	    printf ("  %s\n", file_list(i).name);
	  endfor
	endif
      endif
    else
      error ("dir: expecting directory or filename to be a string");
    endif
  else
    usage ("dir (file)");
  endif

endfunction
