########################################################################
##
## Copyright (C) 2005-2023 The Octave Project Developers
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
## @deftypefn  {} {} fileattrib
## @deftypefnx {} {} fileattrib @var{file}
## @deftypefnx {} {} fileattrib (@var{file})
## @deftypefnx {} {[@var{status}, @var{attrib}] =} fileattrib (@dots{})
## @deftypefnx {} {[@var{status}, @var{msg}, @var{msgid}] =} fileattrib (@dots{})
## Report attribute information about @var{file}.
##
## If no file or directory is specified, report information about the present
## working directory.
##
## If successful, the output is a structure with the following fields:
##
## @table @code
## @item Name
## Full name of @var{file}.
##
## @item archive
## True if @var{file} is an archive (Windows).
##
## @item system
## True if @var{file} is a system file (Windows).
##
## @item hidden
## True if @var{file} is a hidden file (Windows).
##
## @item directory
## True if @var{file} is a directory.
##
## @item  UserRead
## @itemx GroupRead
## @itemx OtherRead
## True if the user (group; other users) has read permission for @var{file}.
##
## @item  UserWrite
## @itemx GroupWrite
## @itemx OtherWrite
## True if the user (group; other users) has write permission for @var{file}.
##
## @item  UserExecute
## @itemx GroupExecute
## @itemx OtherExecute
## True if the user (group; other users) has execute permission for @var{file}.
## @end table
##
## If an attribute does not apply (e.g., archive on a Unix system) then the
## field is set to NaN.
##
## If @var{file} contains globbing characters, information about all matching
## files is returned in a structure array.
##
## If outputs are requested, the first is @var{status} which takes the value 1
## when the operation was successful, and 0 otherwise.  The second output
## contains the structure described above (@var{attrib}) if the operation was
## successful; otherwise, the second output is a system-dependent error message
## (@var{msg}).  The third output is an empty string ("") when the operation
## was successful, or a unique message identifier (@var{msgid}) in the case of
## failure.
## @seealso{stat, glob}
## @end deftypefn

function [status, msg, msgid] = fileattrib (file = ".")

  if (! ischar (file))
    error ("fileattrib: FILE must be a string");
  endif

  sts = 1;
  msg = "";
  msgid = "";

  if (ispc ())
    files = __wglob__ (file);
  else
    files = glob (file);
  endif
  if (isempty (files))
    files = {file};
  endif
  nfiles = numel (files);

  for i = [nfiles, 1:nfiles-1]  # first time in loop extends the struct array
    [info, err, msg] = stat (files{i});
    if (! err)
      r(i).Name = canonicalize_file_name (files{i});

      if (isunix ())
        r(i).archive = NaN;
        r(i).system = NaN;
        r(i).hidden = NaN;
      else
        [~, attrib] = dos (sprintf ('attrib "%s"', r(i).Name));
        ## DOS never returns error status so have to check it indirectly
        if (! isempty (strfind (attrib, " -")))
          sts = 0;
          break;
        endif
        attrib = regexprep (attrib, '\S+:.*', "");
        r(i).archive = any (attrib == "A");
        r(i).system = any (attrib == "S");
        r(i).hidden = any (attrib == "H");
      endif

      r(i).directory = S_ISDIR (info.mode);

      modestr = info.modestr;
      r(i).UserRead = (modestr(2) == "r");
      r(i).UserWrite = (modestr(3) == "w");
      r(i).UserExecute = (modestr(4) == "x");
      if (isunix ())
        r(i).GroupRead = (modestr(5) == "r");
        r(i).GroupWrite = (modestr(6) == "w");
        r(i).GroupExecute = (modestr(7) == "x");
        r(i).OtherRead = (modestr(8) == "r");
        r(i).OtherWrite = (modestr(9) == "w");
        r(i).OtherExecute = (modestr(10) == "x");
      else
        r(i).GroupRead = NaN;
        r(i).GroupWrite = NaN;
        r(i).GroupExecute = NaN;
        r(i).OtherRead = NaN;
        r(i).OtherWrite = NaN;
        r(i).OtherExecute = NaN;
      endif
    else
      sts = 0;
      break;
    endif
  endfor

  if (nargout == 0)
    if (! sts)
      error ("fileattrib: operation failed");
    endif
    status = r;
  else
    status = sts;
    if (! sts)
      if (isempty (msg))
        msg = "operation failed";
      endif
      msgid = "fileattrib";
    else
      msg = r;
    endif
  endif

endfunction


%!test
%! def_tmpdir = canonicalize_file_name (P_tmpdir ());
%! while (length (def_tmpdir) > 2 && strfind (filesep ("all"), def_tmpdir(end)))
%!   def_tmpdir(end) = [];
%! endwhile
%! [status, attr] = fileattrib (P_tmpdir ());
%! assert (status);
%! assert (isstruct (attr));
%! assert (numfields (attr), 14);
%! assert (attr.Name, def_tmpdir);
%! assert (attr.directory);
%! if (ispc ())
%!   assert (! isnan (attr.archive));
%! else
%!   assert (isnan (attr.archive));
%! endif
%! assert (attr.UserRead);
%! if (ispc ())
%!   assert (isnan (attr.GroupRead));
%! else
%!   assert (! isnan (attr.GroupRead));
%! endif

%!error fileattrib (1, 2)
%!error <FILE must be a string> fileattrib (1)
