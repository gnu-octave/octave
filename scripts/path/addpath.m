## Copyright (C) 2005 Bill Denney
##
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2 of the License, or
## (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program; if not, write to the Free Software
## Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
##
## Based on code Copyright (C) 2000 Etienne Grossmann 

## -*- texinfo -*-
## @deftypefn {Function File} {} addpath (@var{dir1}, @dots{})
## @deftypefnx {Function File} {} addpath (@var{dir1}, @dots{}, @var{option})
## Add @var{dir1}, @dots{} to the current @code{LOADPATH}.  If
## @var{option} is @samp{"-begin"} or 0 (the default), prepend the
## directory name to the current path.  If @var{option} is @samp{"-end"}
## or 1, append the directory name to the current path.
## Directories added to the path must exist.
## @seealso{path, rmpath, savepath, pathsep}
## @end deftypefn

## Author: Etienne Grossmann <etienne@cs.uky.edu>
## Modified-By: Bill Denney <bill@givebillmoney.com>

## PKGADD: mark_as_command addpath

function ret = addpath (varargin)

  if (nargout > 0)
    ret = path ();
  endif

  nargs = nargin ();

  if (nargs > 0)

    append = false;
    option = varargin{end};
    if (ischar (option))
      if (strcmpi (option, "-end"))
	append = true;
	nargs--;
      elseif (strcmpi (option, "-begin"))
	nargs--;
      endif
    elseif (option == 1)
      append = true;
    endif

    psep = pathsep ();

    xpath = cellstr (split (path (), psep));
    n_path_elts = length (xpath);
    for i = 1:n_path_elts
      tmp = xpath{i};
      tmp = regexprep (tmp, "//+", "/");
      tmp = regexprep (tmp, "/$", "");
      xpath{i,1} = xpath{i};
      xpath{i,2} = tmp;
    endfor

    for i = 1:nargs
      dir_elts = cellstr (split (varargin{i}, psep));
      n_dir_elts = length (dir_elts);
      for j = 1:n_dir_elts
	dir = regexprep (dir_elts{j}, "//+", "/");
	dir = regexprep (dir, "/$", "");
        [s, status, msg] = stat (dir);
        if (status != 0)
          warning ("addpath: %s: %s", dir, msg);
          continue;
        elseif (! S_ISDIR (s.mode))
          warning ("addpath: %s: not a directory", dir);
          continue;
        endif
	elt_found = false;
	for k = n_path_elts:-1:1
	  if (strcmp (dir, xpath{k,2}))
	    xpath(k,:) = [];
	    n_path_elts--;
	    elt_found = true;
	  endif
	endfor
	if (append)
	  xpath = [xpath; {dir_elts{j}, dir}];
	else
	  xpath = [{dir_elts{j}, dir}; xpath];
	endif
      endfor
    endfor

    xpath{:,2} = psep;
    xpath = xpath';

    tmp = strcat (xpath{:});
    tmp(end) = "";

    tmp = strrep (tmp, DEFAULT_LOADPATH (), "");

    path (tmp);

  endif
  
endfunction
