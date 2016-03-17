## Copyright (C) 2005-2015 John W. Eaton
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
## @deftypefn  {} {} fileattrib (@var{file})
## @deftypefnx {} {} fileattrib ()
## @deftypefnx {} {[@var{status}, @var{msg}, @var{msgid}] =} fileattrib (@dots{})
## Return information about @var{file}.
##
## If successful, @var{status} is 1 and @var{msg} is a structure with the
## following fields:
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
## If an attribute does not apply (i.e., archive on a Unix system) then the
## field is set to NaN.
##
## If @code{attrib} fails, @var{msg} is a non-empty string containing an
## error message and @var{msg_id} is the non-empty string @qcode{"fileattrib"}.
##
## With no input arguments, return information about the current directory.
##
## If @var{file} contains globbing characters, return information about all
## the matching files.
## @seealso{glob}
## @end deftypefn

function [status, msg, msgid] = fileattrib (file = ".")

  if (nargin > 1)
    print_usage ();
  endif

  if (! ischar (file))
    error ("fileattrib: FILE must be a string");
  endif

  status = true;
  msg = "";
  msgid = "";

  files = glob (file);
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
        ## dos never returns error status so have to check it indirectly
        if (! isempty (strfind (attrib, " -")))
          status = false;
          msgid = "fileattrib";
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
        r(i).OtherRead  = (modestr(8) == "r");
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
      status = false;
      msgid = "fileattrib";
      break;
    endif
  endfor

  if (status)
    if (nargout == 0)
      status = r;
    else
      msg = r;
    endif
  endif

endfunction


%!test
%! def_tmpdir = P_tmpdir ();
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

