## Copyright (C) 2000  Etienne Grossmann
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

## -*- texinfo -*-
## @deftypefn {Function File} {} rmpath(dir1, ...)
## Removes dir1,... from the current LOADPATH.
## 
## newpath = rmpath(path, dir1, ...)
## 
## Removes dir1,... from path.
## @end deftypefn

## Author:        Etienne Grossmann <etienne@cs.uky.edu>
## Last modified: June 2005

##PKGADD: mark_as_command rmpath

function ret = rmpath (varargin)

  if (nargout == 0)
    path = LOADPATH;
  else
    path = varargin{1};
  endif

  strip_system_path = 0;
  for arg = nargout + 1:length (varargin)
    p = varargin{arg};
    lp = length (p);

    ## '' is the system path
    if (lp==0)
      strip_system_path = 1;
    endif

    ## strip '...:p:...' -> '...:...'
    lo = 0 ;
    while (lo != length (path))	# Loop while I can substitute
      lo = length (path);
      path = strrep (path, sprintf(":%s:", p), ":");
    endwhile

    ## strip 'p:...' and '...:p' -> '...'
    if (length (path) > lp+1 && strcmp (path(1:lp+1), sprintf ("%s:", p)))
      path = path(lp+2:end);
    endif
    if (length (path) > lp+1 && strcmp (path(end-lp:end), sprintf (":%s", p)))
      path = path(1:end-lp-1);
    endif

    ## strip 'p:' and ':p' -> ':'
    if (length (path) == lp+1 && (strcmp (path, sprintf("%s:", p)) || strcmp (path, sprintf (":%s", p))))
      path = ':';
    endif

    ## strip 'p' -> ''
    if (length (path) == lp && strcmp (path, p))
      path = '';
    endif

  endfor

  if (strip_system_path && strcmp (path, ':'))
    path = '';
  endif

  if (nargout > 0)
    ret = path;
  elseif (! strcmp (LOADPATH, path))
    LOADPATH = path;
  endif
  
endfunction  

%!assert(rmpath(':',''),'');
%!assert(rmpath('hello:',''),'hello');
%!assert(rmpath('hello:world',''),'hello:world');
%!assert(rmpath(':hello:world',''),'hello:world');
%!assert(rmpath(':hello:world:',''),'hello:world');
%!assert(rmpath(':hello::world:',''),'hello:world');

%!assert(rmpath('hello','hello'),'');
%!assert(rmpath(':hello','hello'),':');
%!assert(rmpath('hello:','hello'),':');
%!assert(rmpath('hello:hello','hello'),'');
%!assert(rmpath('hello:hello:hello','hello'),'');
%!assert(rmpath('hello:hello:hello:hello','hello'),'');
%!assert(rmpath(':hello:hello','hello'),':');
%!assert(rmpath('hello:hello:','hello'),':');
%!assert(rmpath('hello','world'),'hello');
%!assert(rmpath(':hello','','hello'),'');
%!assert(rmpath(':hello','hello',''),'');

%!assert(rmpath('hello:world','hello','world'),'');
%!assert(rmpath('hello:world:','hello','world'),':');
%!assert(rmpath(':hello:world:','hello','world'),':');

%!assert(rmpath('hello:world','','hello','world'),'');
%!assert(rmpath('hello:world:','','hello','world'),'');
%!assert(rmpath(':hello:world:','','hello','world'),'');

%!assert(rmpath('hello:world','hello'),'world');
%!assert(rmpath('hello:world','world'),'hello');
%!assert(rmpath('hello:world:','hello'),'world:');
%!assert(rmpath('hello:world:','world'),'hello:');
%!assert(rmpath(':hello:world:','hello'),':world:');
%!assert(rmpath(':hello:world:','world'),':hello:');
