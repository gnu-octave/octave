########################################################################
##
## Copyright (C) 2004-2023 The Octave Project Developers
##
## See the file COPYRIGHT.md in the top-level directory of this
## distribution or <https://octave.org/copyright/>.
##
## This file is part of Octave.
##
## Octave is free software: you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <https://www.gnu.org/licenses/>.
##
########################################################################

## -*- texinfo -*-
## @deftypefn  {} {} dir
## @deftypefnx {} {} dir @var{directory}
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
## characters @samp{*}, @samp{?}, @samp{[]}.  If these wildcard characters are
## escaped with a backslash @samp{\} (e.g., @samp{\*}) on a POSIX platform,
## then they are not treated as wildcards, but as the corresponding literal
## character.  On Windows, it is not possible to escape wildcard characters
## because backslash @samp{\} is treated as a file separator.  On Windows, use
## @code{ls} for file or folder names that contain characters that would be
## treated as wildcards by @code{dir}.
##
## Note that for symbolic links, @code{dir} returns information about the
## file that the symbolic link points to rather than the link itself.  However,
## if the link points to a nonexistent file, @code{dir} returns information
## about the link.
## @seealso{ls, readdir, glob, what, stat, lstat}
## @end deftypefn

## FIXME: This is quite slow for large directories.
##        Perhaps it should be converted to C++?

function list = dir (directory = ".")

  if (! ischar (directory))
    error ("dir: DIRECTORY argument must be a string");
  endif

  ## Prep the list.
  info = struct (zeros (0, 1),
           {"name", "folder" "date", "bytes", "isdir", "datenum", "statinfo"});

  if (strcmp (directory, "*"))
    directory = ".";
  endif
  if (strcmp (directory, "."))
    flst = {"."};
    nf = 1;
    dir_has_wildcard = false;
  else
    flst = __wglob__ (directory);
    nf = numel (flst);
    dir_has_wildcard = any (directory == '*');  # See Bug #58976.
  endif

  ## Determine the file list for the case where a single directory is specified.
  if (nf == 1)
    fn = flst{1};
    [st, err, msg] = stat (fn);
    if (err < 0)
      warning ("dir: 'stat (%s)' failed: %s", fn, msg);
      nf = 0;
    elseif (S_ISDIR (st.mode) && ! dir_has_wildcard)
      flst = readdir (flst{1});
      nf = numel (flst);
      flst = strcat ([fn filesep], flst);
    endif
  endif

  if (nf > 0)

    fs = regexptranslate ("escape", filesep ("all"));
    re = sprintf ('(^.+)[%s]([^%s.]*)([.][^%s]*)?$', fs, fs, fs);
    last_dir = last_absdir = "";
    info(nf,1).name = "";  # pre-declare size of struct array

    ## Collect results.
    cnt = 0;
    for i = 1:nf
      fn = flst{i};
      [st, err, msg] = lstat (fn);
      if (err < 0)
        warning ("dir: 'lstat (%s)' failed: %s", fn, msg);
        continue;
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
        if (is_same_file (fn, tmpdir))
          ## regexrep failed to match, no directory component.
          no_dir = true;
        else
          no_dir = false;
        endif
        fn = regexprep (fn, re, '$2$3');
        info(++cnt).name = fn;
        if (nargout > 0)
          if (no_dir && ! strcmp (fn, "."))
            tmpdir = ".";
          endif
          if (! is_same_file (last_dir, tmpdir))
            ## Caching mechanism to speed up function
            last_dir = tmpdir;
            last_absdir = canonicalize_file_name (last_dir);
          endif
          info(cnt).folder = last_absdir;
          lt = localtime (st.mtime);
          info(cnt).date = strftime ("%d-%b-%Y %T", lt);
          info(cnt).bytes = st.size;
          info(cnt).isdir = S_ISDIR (st.mode);
          info(cnt).datenum = [lt.year + 1900, lt.mon + 1, lt.mday, ...
                               lt.hour, lt.min, lt.sec];
          info(cnt).statinfo = st;
        endif
      endif
    endfor
    info((cnt+1):end) = [];  # remove any unused entries
    if (nargout > 0)
      ## A lot of gymnastics in order to call datenum just once.  2x speed up.
      dvec = [info.datenum]([[1:6:end]', [2:6:end]', [3:6:end]', ...
                             [4:6:end]', [5:6:end]', [6:6:end]']);
      dnum = datenum (dvec);
      ctmp = mat2cell (dnum, ones (cnt,1), 1);
      [info.datenum] = ctmp{:};
    endif
  endif

  ## Return the output arguments.
  if (nargout > 0)
    ## Return the requested structure.
    list = info;
  elseif (numel (info) > 0)
    ## Print the structure to the screen.
    printf ("%s", list_in_columns ({info.name}));
  else
    warning ("dir: nonexistent directory '%s'", directory);
  endif

endfunction


%!test
%! orig_dir = pwd ();
%! tmp_dir = tempname ();
%! unwind_protect
%!   assert (mkdir (tmp_dir));
%!   chdir (tmp_dir);
%!   fclose (fopen ("f1", "w"));
%!   list = dir ();
%!   assert (isstruct (list) && ! isempty (list));
%!   assert (fieldnames (list),
%!           {"name"; "folder"; "date"; "bytes"; "isdir"; "datenum"; "statinfo"});
%!
%!   if (isunix ())
%!     idx = find (strcmp ({list.name}, "."), 1);
%!     assert ({list(idx:idx+1).name}, {".", ".."});
%!     assert ([list(idx:idx+1).isdir], [true true]);
%!   endif
%!
%!   ## test that specifying a filename works the same as using a directory.
%!   found = find (! [list.isdir], 1);
%!   if (! isempty (found))
%!     list2 = dir (fullfile (list(found).folder, list(found).name));
%!     assert (list(found), list2);
%!   endif
%! unwind_protect_cleanup
%!   chdir (orig_dir);
%!   confirm_recursive_rmdir (false, "local");
%!   if (exist (tmp_dir))
%!     sts = rmdir (tmp_dir, "s");
%!   endif
%! end_unwind_protect

%!test <*58976>
%! orig_dir = pwd ();
%! tmp_dir = tempname ();
%! unwind_protect
%!   assert (mkdir (tmp_dir));
%!   assert (mkdir (fullfile (tmp_dir, "dir1")));
%!   assert (mkdir (fullfile (tmp_dir, "dir2")));
%!   chdir (fullfile (tmp_dir, "dir1"));
%!   fclose (fopen ("f1", "w"));
%!   chdir (tmp_dir);
%!
%!   ## Wildcard with multiple matches lists directories
%!   list = dir (fullfile (tmp_dir, "dir*"));
%!   assert (numel (list) == 2);
%!   assert ({list.name}, {"dir1", "dir2"});
%!
%!   ## Wildcard with single match lists directories
%!   assert (rmdir (fullfile (tmp_dir, "dir2")));
%!   list = dir (fullfile (tmp_dir, "dir*"));
%!   assert (numel (list) == 1);
%!   assert ({list.name}, {"dir1"});
%!
%!   ## No wildcard returns listing of directory contents
%!   list = dir (fullfile (tmp_dir, "dir1"));
%!   assert (any (strcmp ({list.name}, "f1")));
%! unwind_protect_cleanup
%!   chdir (orig_dir);
%!   confirm_recursive_rmdir (false, "local");
%!   if (exist (tmp_dir))
%!     sts = rmdir (tmp_dir, "s");
%!   endif
%! end_unwind_protect

%!test <*57666>
%! orig_dir = pwd ();
%! tmp_dir = tempname ();
%! unwind_protect
%!   assert (mkdir (tmp_dir));
%!   list = dir (tmp_dir);
%!   assert (list(1).name, ".");
%!   assert (list(1).folder, canonicalize_file_name (tmp_dir));
%! unwind_protect_cleanup
%!   if (exist (tmp_dir))
%!     sts = rmdir (tmp_dir);
%!   endif
%! end_unwind_protect

## Test input validation
%!error <DIRECTORY argument must be a string> dir (1)
%!warning <nonexistent directory> dir ("_%UNLIKELY_DIR_NAME%_");
