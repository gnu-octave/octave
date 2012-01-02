## Copyright (C) 2006-2012 Bill Denney
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
## @deftypefn  {Function File} {@var{files} =} unpack (@var{file})
## @deftypefnx {Function File} {@var{files} =} unpack (@var{file}, @var{dir})
## @deftypefnx {Function File} {@var{files} =} unpack (@var{file}, @var{dir}, @var{filetype})
## Unpack the archive @var{file} based on its extension to the directory
## @var{dir}.  If @var{file} is a list of strings, then each file is
## unpacked individually.  If @var{dir} is not specified, it defaults to
## the current directory.  If a directory is in the file list, then the
## @var{filetype} must also be specified.
##
## The optional return value is a list of @var{files} unpacked.
## @seealso{bzip2, gzip, zip, tar}
## @end deftypefn

## Author: Bill Denney <denney@seas.upenn.edu>

function filelist = unpack (file, dir = ".", filetype = "")

  if (nargin < 1 || nargin > 3)
    print_usage ();
  endif

  if (! ischar (file) && ! iscellstr (file))
    error ("unpack: invalid input file class, %s", class(file));
  endif

  ## character arrays of more than one string must be treated as cell strings
  if (ischar (file) && ! isvector (file))
    file = cellstr (file);
  endif

  ## Recursively unpack cellstr arrays one file at a time
  if (iscellstr (file))
    files = {};
    for i = 1:numel (file)
      tmpfiles = unpack (file{i}, dir);
      files = {files{:} tmpfiles{:}};
    endfor

    ## Return output if requested.
    if (nargout > 0)
      filelist = files;
    endif

    return;
  endif

  if (isdir (file))
    if (isempty (filetype))
      error ("unpack: FILETYPE must be given for a directory");
    elseif (! any (strcmpi (filetype, "gunzip")))
      error ("unpack: FILETYPE must be gunzip for a directory");
    endif
    ext = ".gz";
  else
    [pathstr, name, ext] = fileparts (file);

    ## Check to see if it's .tar.gz, .tar.Z, etc.
    if (any (strcmpi ({".gz" ".Z" ".bz2" ".bz"}, ext)))
      [tmppathstr, tmpname, tmpext] = fileparts (name);
      if (strcmpi (tmpext, ".tar"))
        name = tmpname;
        ext = cstrcat (tmpext, ext);
      endif
    endif

    ## If the file is a URL, download it and then work with that file.
    if (! isempty (strfind (file, "://")))
      ## FIXME -- the above is not a perfect test for a URL
      urlfile = file;
      ## FIXME -- should we name the file that we download with the
      ## same file name as the URL requests?
      tmpfile = cstrcat (tmpnam (), ext);
      [file, success, msg] = urlwrite (urlfile, tmpfile);
      if (! success)
        error ("unpack: could not get \"%s\": %s", urlfile, msg);
      endif
    endif

  endif

  ## canonicalize_file_name returns empty if the file isn't found, so
  ## use that to check for existence.
  cfile = canonicalize_file_name (file);

  if (isempty (cfile))
    error ("unpack: file \"%s\" not found", file);
  else
    file = cfile;
  endif

  ## Instructions on what to do for any extension.
  ##
  ## The field names are the file extension without periods.
  ## The first cell is what is executed to unpack an archive verbosely.
  ## The second cell is what is executed to unpack an archive quietly.
  ## The third cell is the function to execute on output to get the
  ##   files list.
  ## The fourth cell indicates if the files may need to be manually moved
  ##   (i.e. tar and unzip decompress into the current directory while
  ##   bzip2 and gzip decompress the file at its location).
  persistent commandlist;
  if (isempty (commandlist))
    commandlist.gz = {"gzip -d -v -r \"%s\"", ...
                      "gzip -d -r \"%s\"", ...
                      @__parse_gzip__, true};
    commandlist.z = commandlist.gz;
    commandlist.bz2 = {"bzip2 -d -v \"%s\"", ...
                       "bzip2 -d \"%s\"", ...
                       @__parse_bzip2__, true};
    commandlist.bz = commandlist.bz2;
    commandlist.tar = {"tar xvf \"%s\"", ...
                       "tar xf \"%s\"", ...
                       @__parse_tar__, false};
    commandlist.targz = {"gzip -d -c \"%s\" | tar xvf -", ...
                         "gzip -d -c \"%s\" | tar xf -", ...
                         @__parse_tar__, false};
    commandlist.tgz = commandlist.targz;
    commandlist.tarbz2 = {"bzip2 -d -c \"%s\" | tar xvf -", ...
                          "bzip2 -d -c \"%s\" | tar xf -", ...
                          @__parse_tar__, false};
    commandlist.tarbz = commandlist.tarbz2;
    commandlist.tbz2 = commandlist.tarbz2;
    commandlist.tbz = commandlist.tarbz2;
    commandlist.zip = {"unzip \"%s\"", ...
                       "unzip -q \"%s\"", ...
                       @__parse_zip__, false};
  endif

  nodotext = ext(! ismember (ext, "."));

  origdir = pwd ();

  if (isfield (commandlist, nodotext))
    [commandv, commandq, parser, move] = deal (commandlist.(nodotext){:});
    cstartdir = canonicalize_file_name (origdir);
    cenddir = canonicalize_file_name (dir);
    needmove = move && ! strcmp (cstartdir, cenddir);
    if (nargout > 0 || needmove)
      command = commandv;
    else
      command = commandq;
    endif
  else
    warning ("unpack:filetype", "unrecognized file type, %s", ext);
    files = file;
    return;
  endif

  ## Create the directory if necessary.
  s = stat (dir);
  if (isempty (s))
    [status, msg] = mkdir (dir);
    if (! status)
      error ("unpack: mkdir failed to create %s: %s", dir, msg);
    endif
  elseif (! S_ISDIR (s.mode))
    error ("unpack: %s: not a directory", dir);
  endif

  unwind_protect
    cd (dir);
    [status, output] = system (sprintf (cstrcat (command, " 2>&1"), file));
  unwind_protect_cleanup
    cd (origdir);
  end_unwind_protect

  if (status)
    error ("unpack: unarchiving program exited with status: %d\n%s",
           status, output);
  endif

  if (nargout > 0 || needmove)
    ## Trim the last cr if needed.
    ## FIXME -- will this need to change to a check for "\r\n" for windows?
    if (output(length (output)) == "\n")
      output(length (output)) = [];
    endif
    files = parser (strsplit (output, "\n"))';

    ## Move files if necessary
    if (needmove)
      [st, msg, msgid] = movefile (files, dir);
      if (! st)
        error ("unpack: unable to move files to \"%s\": %s",
               dir, msg);
      endif

      ## Fix the names for the files since they were moved.
      for i = 1:numel (files)
        files{i} = strrep (files{i}, cstartdir, cenddir);
      endfor
    endif

    ## Return output if requested.
    if (nargout > 0)
      filelist = files;
    endif
  endif

endfunction

function files = __parse_zip__ (output)
  ## Parse the output from zip and unzip.

  ## Skip first line which is Archive header
  output(1) = [];
  for i = 1:length (output)
    files{i} = output{i}(14:length(output{i}));
  endfor
endfunction

function output = __parse_tar__ (output)
  ## This is a noop, but it makes things simpler for other cases.
endfunction

function files = __parse_gzip__ (output)
  ## Parse the output from gzip and gunzip returning the files
  ## commpressed (or decompressed).

  files = {};
  ## The middle ": " should indicate a good place to start looking for
  ## the filename.
  for i = 1:length (output)
    colons = strfind (output{i}, ":");
    if (isempty (colons))
      warning ("unpack:parsing",
               "Unable to parse line (gzip missing colon):\n%s", output{i});
    else
      midcolon = colons(ceil (length (colons)/2));
      thisstr = output{i}(midcolon+2:length(output{i}));
      idx = index (thisstr, "with") + 5;
      if (isempty (idx))
        warning ("unpack:parsing",
                 "Unable to parse line (gzip missing with):\n%s", output{i});
      else
        files{i} = thisstr(idx:length (thisstr));
      endif
    endif
  endfor
endfunction

function files = __parse_bzip2__ (output)
  ## Parse the output from bzip2 and bunzip2 returning the files
  ## commpressed (or decompressed).

  files = {};
  for i = 1:length (output)
    ## the -5 is to remove the ".bz2:"
    endoffilename = rindex (output{i}, ": ") - 5;
    if (isempty (endoffilename))
      warning ("unpack:parsing", "Unable to parse line:\n%s", output{i});
    else
      files{i} = output{i}(3:endoffilename);
    endif
  endfor
endfunction
