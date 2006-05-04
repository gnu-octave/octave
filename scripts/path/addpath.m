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
## Prepend @var{dir1}, @dots{} to the current @code{LOADPATH}.
## If the directory is already in the path, it is moved to the specified
## location, prepending by default.
## 
## @example
## addpath (dir1, "-end", dir2, "-begin", dir3, "-END", dir4, "-BEGIN", dir5)
## @result{} Prepend dir1, dir3 and dir5 and append dir2 and dir4. 
## @end example
##
## An error will be returned if the string is not a directory, the
## directory doesn't exist or you don't have read access to it.
##
## BUG: This function can't add directories called @samp{-end} or
## @samp{-begin} (case insensitively).
##
## @seealso{LOADPATH, rmpath, savepath, setpath}
## @end deftypefn

## Author: Etienne Grossmann <etienne@cs.uky.edu>
## Modified-By: Bill Denney <bill@givebillmoney.com>

## PKGADD: mark_as_command addpath

function ret = addpath (varargin)

  if (nargout > 0)
    path = varargin{1};
    varargin = varargin(2:end);
  else
    path = LOADPATH;
  endif

  dir = "";
  if (length (varargin) > 0)
    append = 0;
    switch varargin{end}
    case { 0, "0", "-begin", "-BEGIN" }
      varargin = varargin(1:end-1);
    case { 1, "1", "-end", "-END" }
      varargin = varargin(1:end-1);
      append = 1;
    endswitch

    psep = pathsep();

    ## Avoid duplicates by stripping pre-existing entries
    path = rmpath (path, varargin{:});

    ## Check if the directories are valid
    for arg = 1:length (varargin)
      p = varargin{arg};
      if (nargout == 0 && ! isempty (p))
        [s, err, m] = stat (p);
        if (err != 0)
          warning ("addpath %s : %s\n", p, m);
          continue;
        elseif (index (s.modestr, "d") != 1)
          warning ("addpath %s : not a directory (mode=%s)\n", p, s.modestr);
          continue;
        elseif (! (s.modestr(8) == "r"
		   || (getgid == s.gid && s.modestr(5) == "r")
		   || (getuid == s.uid && s.modestr(2) == "r")))
          warning ("addpath %s : not readable (mode=%s)\n", p, s.modestr);
          continue;
        endif
      endif
      dir = sprintf ("%s%s%s", dir, psep, p);
    endfor
      
    ## Add the directories to the current path
    if (! isempty (dir))
      dir = dir(2:end);
      if (isempty (path) && ! isempty (dir))
        path = dir;
      else
        if strcmp (path, psep), path = ""; end
          if append
            path = sprintf ("%s%s%s", path, psep, dir);
          else
            path = sprintf ("%s%s%s", dir, psep, path);
          endif
      endif
    endif
  endif

  if nargout 
    ret = path; 
  else
    LOADPATH = path; 
  endif

endfunction

%!assert(addpath('','hello'),'hello');
%!assert(addpath('','hello','world'),['hello',pathsep(),'world'])
%!assert(addpath(pathsep(),'hello'),['hello',pathsep()]);
%!assert(addpath(pathsep(),'hello','-end'),[pathsep(),'hello']);
%!assert(addpath('hello','hello'),'hello');
%!assert(addpath('hello','world'),['world',pathsep(),'hello'])
%!assert(addpath('hello','world','-end'),['hello',pathsep(),'world'])
%!assert(addpath(['hello',pathsep()],'world','-end'),['hello',pathsep(),pathsep(),'world'])
%!assert(addpath(['hello',pathsep()],'hello','world','-end'),[pathsep(),'hello',pathsep(),'world'])

%!assert(addpath('',''),pathsep())
%!assert(addpath(pathsep(),''),pathsep())
%!assert(addpath('hello',''),[pathsep(),'hello'])
%!assert(addpath(['hello',pathsep(),'world'],''),[pathsep(),'hello',pathsep(),'world'])
%!assert(addpath(['hello',pathsep(),'world',pathsep()],''),[pathsep(),'hello',pathsep(),'world'])
%!assert(addpath(['hello',pathsep(),pathsep(),'world'],''),[pathsep(),'hello',pathsep(),'world'])
