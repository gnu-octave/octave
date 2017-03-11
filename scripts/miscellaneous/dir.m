## Copyright (C) 2004-2017 John W. Eaton
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or
## (at your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn  {} {} dir
## @deftypefnx {} {} dir (@var{directory})
## @deftypefnx {} {[@var{list}] =} dir (@var{directory})
## Display file listing for directory @var{directory}.
##
## If @var{directory} is not specified then list the present working directory.
##
## If a return value is requested, return a structure array with the fields
##
## @table @asis
## @item name
## File or directory name.
##
## @item folder
## Location of file or directory
## 
## @item date
## Timestamp of file modification (string value).
##
## @item bytes
## File size in bytes.
##
## @item isdir
## True if name is a directory.
##
## @item datenum
## Timestamp of file modification as serial date number (double).
##
## @item statinfo
## Information structure returned from @code{stat}.
## @end table
##
## If @var{directory} is a filename, rather than a directory, then return
## information about the named file.  @var{directory} may also be a list rather
## than a single directory or file.
##
## @var{directory} is subject to shell expansion if it contains any wildcard
## characters @samp{*}, @samp{?}, @samp{[]}.  To find a literal example of a
## wildcard character the wildcard must be escaped using the backslash operator
## @samp{\}.
##
## Note that for symbolic links, @code{dir} returns information about the
## file that the symbolic link points to rather than the link itself.
## However, if the link points to a nonexistent file, @code{dir} returns
## information about the link.
## @seealso{ls, readdir, glob, what, stat, lstat}
## @end deftypefn

## Author: jwe

## FIXME: This is quite slow for large directories.
##        Perhaps it should be converted to C++?

function retval = dir (directory)

  if (nargin == 0)
    directory = ".";
  elseif (nargin > 1)
    print_usage ();
  endif

  if (! ischar (directory))
    error ("dir: DIRECTORY argument must be a string");
  endif

  ## Prep the retval.
  info = struct (zeros (0, 1),
           {"name", "folder" "date", "bytes", "isdir", "datenum", "statinfo"});

  if (strcmp (directory, "*"))
    directory = ".";
  endif
  if (strcmp (directory, "."))
    flst = {"."};
    nf = 1;
  else
    flst = __wglob__ (directory);
    nf = numel (flst);
  endif

  ## Determine the file list for the case where a single directory is specified.
  if (nf == 1)
    fn = flst{1};
    [st, err, msg] = stat (fn);
    if (err < 0)
      warning ("dir: 'stat (%s)' failed: %s", fn, msg);
      nf = 0;
    elseif (S_ISDIR (st.mode))
      flst = readdir (flst{1});
      nf = numel (flst);
      flst = strcat ([fn filesep], flst);
    endif
  endif

  if (numel (flst) > 0)

    fs = regexptranslate ("escape", filesep ("all"));
    re = sprintf ('(^.+[%s])([^%s.]*)([.][^%s]*)?$', fs, fs, fs);
    last_dir = last_absdir = "";
    info(nf,1).name = "";  # pre-declare size of struct array

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
          [xst, err] = stat (fn);
          if (! err)
            st = xst;
          endif
        endif
        tmpdir = regexprep (fn, re, '$1');
        fn = regexprep (fn, re, '$2$3');
        info(i).name = fn;
        if (! strcmp (last_dir, tmpdir))
          ## Caching mechanism to speed up function
          last_dir = tmpdir;
          last_absdir = make_absolute_filename (last_dir);
        endif
        info(i).folder = last_absdir;
        lt = localtime (st.mtime);
        info(i).date = strftime ("%d-%b-%Y %T", lt);
        info(i).bytes = st.size;
        info(i).isdir = S_ISDIR (st.mode);
        info(i).datenum = [lt.year + 1900, lt.mon + 1, lt.mday, ...
                             lt.hour, lt.min, lt.sec];
        info(i).statinfo = st;
      endif
    endfor
    ## A lot of gymnastics in order to call datenum just once.  2x speed up.
    dvec = [info.datenum]([[1:6:end]', [2:6:end]', [3:6:end]', ...
                           [4:6:end]', [5:6:end]', [6:6:end]']);
    dnum = datenum (dvec);
    ctmp = mat2cell (dnum, ones (nf,1), 1);
    [info.datenum] = ctmp{:};
  endif

  ## Return the output arguments.
  if (nargout > 0)
    ## Return the requested structure.
    retval = info;
  elseif (numel (info) > 0)
    ## Print the structure to the screen.
    printf ("%s", list_in_columns ({info.name}));
  else
    warning ("dir: nonexistent directory '%s'", directory);
  endif

endfunction


%!test
%! list = dir ();
%! assert (isstruct (list) && ! isempty (list));
%! assert (fieldnames (list),
%!         {"name"; "date"; "bytes"; "isdir"; "datenum"; "statinfo"});
%!
%! if (isunix ())
%!   assert ({list(1:2).name}, {".", ".."});
%!   assert ([list(1:2).isdir], [true true]);
%! endif
%!
%! ## test that specifying a filename works the same as using a directory.
%! found = find (! [list.isdir], 1);
%! if (! isempty (found))
%!   list2 = dir (list(found).name);
%!   assert (list(found), list2);
%! endif

## Test input validation
%!error <DIRECTORY argument must be a string> dir (1)
%!warning <nonexistent directory> dir ("_%UNLIKELY_DIR_NAME%_");
