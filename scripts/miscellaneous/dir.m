## Copyright (C) 2004-2012 John W. Eaton
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
## @deftypefn  {Function File} {} dir (@var{directory})
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
## where @code{statinfo} is the structure returned from @code{stat}.
##
## If @var{directory} is not a directory, return information about the
## named @var{filename}.  @var{directory} may be a list of directories
## specified either by name or with wildcard characters (like * and ?)
## which will be expanded with glob.
##
## Note that for symbolic links, @code{dir} returns information about
## the file that the symbolic link points to instead of the link itself.
## However, if the link points to a nonexistent file, @code{dir} returns
## information about the link.
## @seealso{ls, stat, lstat, readdir, glob, filesep}
## @end deftypefn

## Author: jwe

## FIXME -- this is quite slow for large directories, so perhaps
## it should be converted to C++.

function retval = dir (directory)

  if (nargin == 0)
    directory = ".";
  elseif (nargin > 1)
    print_usage ();
  endif

  ## Prep the retval.
  info = struct (zeros (0, 1),
                 {"name", "date", "bytes", "isdir", "datenum", "statinfo"});

  if (ischar (directory))
    if (strcmp (directory, "*"))
      directory = ".";
    endif
    if (strcmp (directory, "."))
      flst = {"."};
      nf = 1;
    else
      flst = glob (directory);
      nf = length (flst);
    endif

    ## Determine the file list for the case where a single directory is
    ## specified.
    if (nf == 1)
      fn = flst{1};
      [st, err, msg] = stat (fn);
      if (err < 0)
        warning ("dir: 'stat (%s)' failed: %s", fn, msg);
        nf = 0;
      elseif (S_ISDIR (st.mode))
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
          warning ("dir: 'lstat (%s)' failed: %s", fn, msg);
        else
          ## If we are looking at a link that points to something,
          ## return info about the target of the link, otherwise, return
          ## info about the link itself.
          if (S_ISLNK (st.mode))
            [xst, err, msg] = stat (fn);
            if (! err)
              st = xst;
            endif
          endif
          [dummy, fn, ext] = fileparts (fn);
          fn = cstrcat (fn, ext);
          info(i,1).name = fn;
          lt = localtime (st.mtime);
          info(i,1).date = strftime ("%d-%b-%Y %T", lt);
          info(i,1).bytes = st.size;
          info(i,1).isdir = S_ISDIR (st.mode);
          info(i,1).datenum = datenum (lt.year + 1900, lt.mon + 1, lt.mday,
                                       lt.hour, lt.min, lt.sec);
          info(i,1).statinfo = st;
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
    warning ("dir: nonexistent directory '%s'", directory);
  endif

endfunction
