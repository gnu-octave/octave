########################################################################
##
## Copyright (C) 2006-2023 The Octave Project Developers
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
## @deftypefn  {} {@var{files} =} unpack (@var{file})
## @deftypefnx {} {@var{files} =} unpack (@var{file}, @var{dir})
## @deftypefnx {} {@var{files} =} unpack (@var{file}, @var{dir}, @var{filetype})
## Unpack the archive @var{file} based on its extension to the directory
## @var{dir}.
##
## If @var{file} is a list of strings, then each file is unpacked
## individually.  Shell wildcards in the filename such as @samp{*} or
## @samp{?} are accepted and expanded.
##
## If @var{dir} is not specified or is empty (@code{[]}), it defaults to the
## current directory.  If a directory is in the file list, then @var{filetype}
## must also be specified.
##
## The specific archive filetype is inferred from the extension of the file.
## The @var{filetype} may also be specified directly using a string which
## corresponds to a known extension.
##
## Valid filetype extensions:
##
## @table @code
## @item  @nospell{bz}
## @itemx @nospell{bz2}
## bzip archive
##
## @item @nospell{gz}
## gzip archive
##
## @item tar
## tar archive
##
## @item  tarbz
## @itemx tarbz2
## @itemx tbz
## @itemx tbz2
## tar + bzip archive
##
## @item  targz
## @itemx tgz
## tar + gzip archive
##
## @item z
## compress archive
##
## @item zip
## zip archive
## @end table
##
## The optional return value is a list of @var{files} unpacked.
## @seealso{bunzip2, gunzip, unzip, untar, bzip2, gzip, zip, tar}
## @end deftypefn

function filelist = unpack (file, dir = [], filetype = "")

  if (nargin < 1)
    print_usage ();
  endif

  if (! ischar (file) && ! iscellstr (file))
    error ("unpack: FILE must be a string or cell array of strings");
  endif

  ## Convert char arrays to cell strings to simplify further processing
  if (ischar (file))
    file = cellstr (file);
  endif
  if (numel (file) == 1)
    ## FIXME: The code below is not a perfect test for a URL
    if (isempty (strfind (file{1}, "://")))
      if (ispc ())
        gfile = __wglob__ (file);
      else
        gfile = glob (file);
      endif
      if (isempty (gfile))
        error ('unpack: FILE "%s" not found', file{1});
      else
        file = gfile;
      endif
    endif
  endif

  ## Recursively unpack cellstr arrays one file at a time
  if (numel (file) > 1)
    files = {};
    for i = 1:numel (file)
      if (! isempty (dir))
        tmpfiles = unpack (file{i}, dir);
      else
        tmpfiles = unpack (file{i}, fileparts (file{i}));
      endif
      files = {files{:} tmpfiles{:}};
    endfor

    ## Return output if requested.
    if (nargout > 0)
      filelist = files;
    endif
    return;

  else
    file = file{1};
  endif

  if (nargin == 3 && (! ischar (filetype) || ! isrow (filetype)))
    error ("unpack: FILETYPE must be a string");
  endif

  if (isfolder (file))
    if (isempty (filetype))
      error ("unpack: FILETYPE must be given for a directory");
    elseif (! strcmpi (filetype, "gz"))
      error ('unpack: FILETYPE must be "gz" for a directory');
    endif
    ext = ".gz";
  else
    [pathstr, name, ext] = fileparts (file);

    if (nargin == 3 && ! strcmpi (ext, filetype))
      ## override extension with given filetype
      if (isempty (ext))
        ext = filetype;
      else
        ext = regexprep (ext, '(\.?)\S*$', ['$1' filetype]);
      endif
    endif

    ## Check to see if it's .tar.gz, .tar.Z, etc.
    if (any (strcmpi ({".gz" ".Z" ".bz2" ".bz"}, ext)))
      [~, tmpname, tmpext] = fileparts (name);
      if (strcmpi (tmpext, ".tar"))
        name = tmpname;
        ext = [tmpext ext];
      endif
    endif

    ## If the file is a URL, download it and then work with that file.
    if (! isempty (strfind (file, "://")))
      ## FIXME: The above code is not a perfect test for a URL
      urlfile = file;
      tmpfile = fullfile (tempdir (), [name ext]);
      [file, success, msg] = urlwrite (urlfile, tmpfile);
      if (! success)
        error ('unpack: could not fetch "%s": %s', urlfile, msg);
      endif
    endif

  endif

  file = make_absolute_filename (file);

  if (isempty (dir))
    dir = ".";
  else
    dir = tilde_expand (dir);
  endif

  ## Instructions on what to do for any extension.
  ##
  ## The field names are the file extension without periods.
  ## The first cell is what is executed to unpack an archive verbosely.
  ## The second cell is what is executed to unpack an archive quietly.
  ## The third cell is the function to execute on output to get the files list.
  ## The fourth cell indicates if the files may need to be manually moved
  ##   (i.e., tar and unzip decompress into the current directory while
  ##    bzip2 and gzip decompress the file at its location).
  persistent commandlist;
  if (isempty (commandlist))
    commandlist.gz = {'gzip -d -k -v -f -r "%s"', ...
                      'gzip -d -k -f -r "%s"', ...
                      @__parse_gzip__, true};
    commandlist.z = commandlist.gz;
    commandlist.bz2 = {'bzip2 -d -k -v -f "%s"', ...
                       'bzip2 -d -k -f "%s"', ...
                       @__parse_bzip2__, true};
    commandlist.bz = commandlist.bz2;
    commandlist.tar = {'tar xvf "%s"', ...
                       'tar xf "%s"', ...
                       @__parse_tar__, false};
    commandlist.targz = {'gzip -d -c "%s" | tar xvf -', ...
                         'gzip -d -c "%s" | tar xf -', ...
                         @__parse_tar__, false};
    commandlist.tgz = commandlist.targz;
    commandlist.tarbz2 = {'bzip2 -d -c "%s" | tar xvf -', ...
                          'bzip2 -d -c "%s" | tar xf -', ...
                          @__parse_tar__, false};
    commandlist.tarbz = commandlist.tarbz2;
    commandlist.tbz2 = commandlist.tarbz2;
    commandlist.tbz = commandlist.tarbz2;
    commandlist.zip = {'unzip -n "%s"', ...
                       'unzip -nq "%s"', ...
                       @__parse_zip__, false};
  endif

  ## Unzip doesn't actually care about the extension
  if (strcmpi (filetype, "zip"))
    nodotext = "zip";
  else
    nodotext = ext(ext != '.');
  endif

  if (ispc && strcmp (nodotext, "tar"))
    ## Change file pathname into a mingw style acceptable for tar
    file = __w2mpth__ (file);
  endif

  ## Create the output directory if necessary.
  s = stat (dir);
  if (isempty (s))
    [status, msg] = mkdir (dir);
    if (! status)
      error ("unpack: mkdir failed to create %s: %s", dir, msg);
    endif
  elseif (! S_ISDIR (s.mode))
    error ("unpack: %s: not a directory", dir);
  endif

  if (isfield (commandlist, tolower (nodotext)))
    [commandv, commandq, parsefcn, move] = deal (commandlist.(nodotext){:});
    origdir = pwd ();
    if (move)
      startdir = fileparts (file);
    else
      startdir = origdir;
    endif
    cstartdir = make_absolute_filename (startdir);
    cenddir = make_absolute_filename (dir);
    if (cenddir(end) == filesep)
      cenddir(end) = [];
    endif
    needmove = move && ! is_same_file (cstartdir, cenddir);
    if (nargout > 0 || needmove)
      command = commandv;
    else
      command = commandq;
    endif
  else
    warning ("unpack: unrecognized FILETYPE <%s>", nodotext);
    filelist = {};
    return;
  endif

  ## Save and restore the TAR_OPTIONS environment variable used by GNU tar.
  tar_options_env = getenv ("TAR_OPTIONS");
  unwind_protect
    unsetenv ("TAR_OPTIONS");
    cd (dir);
    if (ispc ())
      ## Escape backslashes (necessary for UNC paths).
      file = strrep (file, '\', '\\');
    endif
    [status, output] = system (sprintf ([command " 2>&1"], file));
  unwind_protect_cleanup
    cd (origdir);
    if (! isempty (tar_options_env))
      setenv ("TAR_OPTIONS", tar_options_env);
    endif
  end_unwind_protect

  if (status)
    error ("unpack: unarchiving program exited with status: %d\n%s",
           status, output);
  endif

  if (nargout > 0 || needmove)
    ## Trim the last CR or NL if needed.
    files = parsefcn (ostrsplit (output, "\r\n", true))';

    ## Move files if necessary.
    if (needmove)
      [st, msg] = movefile (files, cenddir);
      if (! st)
        error ('unpack: unable to move files to "%s": %s', dir, msg);
      endif

      ## Fix the names of the files since they were moved.
      files = strrep (files, cstartdir, cenddir);
    endif

    ## Return output if requested.
    if (nargout > 0)
      filelist = files;
    endif
  endif

endfunction

function files = __parse_zip__ (output)
  ## Parse the output from zip and unzip.

  ## Skip first line which is Archive header.
  files = char (output(2:end));
  ## Trim constant width prefix and return cell array.
  files = cellstr (files(:,14:end));

endfunction

function output = __parse_tar__ (output)
  ## BSD tar emits file actions in the first 2 columns

  if (tar_is_bsd ())
    output = cellfun (@(x) x(3:end), output, 'UniformOutput', false);
  endif
endfunction

function files = __parse_gzip__ (output)
  ## Parse the output from gzip and gunzip returning the files
  ## compressed (or decompressed).

  files = regexprep (output, '^.+ -- (?:created|replaced with) (.*)$', '$1');
endfunction

function files = __parse_bzip2__ (output)
  ## Parse the output from bzip2 and bunzip2 returning the files
  ## compressed (or decompressed).

  ## Strip leading blanks and .bz2 extension from filename
  files = regexprep (output, '^\s+(.*)\.bz2: .*', '$1');
endfunction


%!testif HAVE_ZLIB
%! envvar = {"TMPDIR", "TMP"};
%! envdir = cellfun (@(x) getenv (x), envvar, "uniformoutput", false);
%! unwind_protect
%!   cellfun (@(x) unsetenv (x), envvar);
%!   ## Create temporary directory and file for packing and unpacking
%!   dirname = tempname ();
%!   assert (mkdir (dirname));
%!   filename = tempname ();
%!   fid = fopen (filename, "wt");
%!   assert (fid >= 0);
%!   fprintf (fid, "Hello World\n");
%!   fprintf (fid, "123 456 789\n");
%!   fclose (fid);
%!
%!   unwind_protect
%!     copyfile (filename, [filename ".orig"]);
%!     gzip (filename, dirname);
%!     [~, f] = fileparts (filename);
%!     filelist = unpack (fullfile (dirname, [f ".gz"]), tempdir);
%!     assert (filelist{1}, filename);
%!     fid = fopen ([filename ".orig"], "rb");
%!     assert (fid >= 0);
%!     orig_data = fread (fid);
%!     fclose (fid);
%!     fid = fopen (filename, "rb");
%!     assert (fid >= 0);
%!     new_data = fread (fid);
%!     fclose (fid);
%!     if (orig_data != new_data)
%!       error ("unpack: Unpacked file does not equal original");
%!     endif
%!   unwind_protect_cleanup
%!     unlink (filename);
%!     unlink ([filename ".orig"]);
%!     confirm_recursive_rmdir (false, "local");
%!     sts = rmdir (dirname, "s");
%!   end_unwind_protect
%! unwind_protect_cleanup
%!   ## Restore environment variables TMPDIR, TMP
%!   for i = 1:numel (envvar)
%!     if (isempty (envdir{i}))
%!       unsetenv (envvar{i});
%!     else
%!       setenv (envvar{i}, envdir{i});
%!     endif
%!   endfor
%! end_unwind_protect

## Test input validation
%!error <Invalid call> unpack ()
%!error <FILE must be a string or cell array of strings> unpack (1)
%!error <FILE "_%NOT_A_FILENAME%_" not found> unpack ("_%NOT_A_FILENAME%_")
%!error <FILE "_%NOT_A_FILENAME%_" not found> unpack ({"_%NOT_A_FILENAME%_"})
%!error <FILE "_%NOT_A_FILENAME%_" not found> unpack ({"_%NOT_A_FILENAME%_", "2nd_filename"})
%!error <FILETYPE must be a string>
%! if (isunix || ismac)
%!   unpack ("/", [], 1)
%! else
%!   unpack ('C:\', [], 1)
%! endif
%!error <FILETYPE must be given for a directory>
%! if (isunix || ismac)
%!   unpack ("/");
%! else
%!   unpack ('C:\');
%! endif
%!error <FILETYPE must be "gz" for a directory>
%! if (isunix || ismac)
%!   unpack ("/", [], "foobar");
%! else
%!   unpack ('C:\', [], "foobar");
%! endif
