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
## @deftypefn  {} {} copyfile @var{f1} @var{f2}
## @deftypefnx {} {} copyfile @var{f1} @var{f2} f
## @deftypefnx {} {} copyfile (@var{f1}, @var{f2})
## @deftypefnx {} {} copyfile (@var{f1}, @var{f2}, 'f')
## @deftypefnx {} {[@var{status}, @var{msg}, @var{msgid}] =} copyfile (@dots{})
## Copy the source file(s) or directory @var{f1} to the destination @var{f2}.
##
## The name @var{f1} may contain globbing patterns, or may be a cell array of
## strings.  If @var{f1} expands to multiple filenames, @var{f2} must be a
## directory.
##
## When the force flag @qcode{'f'} is given any existing files will be
## overwritten without prompting.
##
## If successful, @var{status} is logical 1, and @var{msg}, @var{msgid} are
## empty character strings ("").  Otherwise, @var{status} is logical 0,
## @var{msg} contains a system-dependent error message, and @var{msgid}
## contains a unique message identifier.  Note that the status code is exactly
## opposite that of the @code{system} command.
## @seealso{movefile, rename, unlink, delete, glob}
## @end deftypefn

function [status, msg, msgid] = copyfile (f1, f2, force)

  if (nargin < 2)
    print_usage ();
  endif

  max_cmd_line = 1024;
  sts = true;
  msg = "";
  msgid = "";

  ## FIXME: Maybe use the same method as in ls to allow users control
  ##        over the command that is executed.

  if (ispc () && ! isunix ()
      && isempty (file_in_path (getenv ("PATH"), "cp.exe")))
    ## Windows.
    cmd = "cmd /C xcopy /E";
    cmd_force_flag = "/Y";
  else
    cmd = "cp -r";
    cmd_force_flag = "-f";
  endif

  ## Input type check.
  if (ischar (f1))
    f1 = cellstr (f1);
  elseif (! iscellstr (f1))
    error ("copyfile: F1 must be a string or a cell array of strings");
  endif
  if (! ischar (f2))
    error ("copyfile: F2 must be a string");
  endif

  if (nargin == 3 && strcmp (force, "f"))
    cmd = [cmd " " cmd_force_flag];
  endif

  ## If f1 has more than 1 element then f2 must be a directory
  isdir = isfolder (f2);
  if (numel (f1) > 1 && ! isdir)
    if (nargout == 0)
      error ("copyfile: when copying multiple files, F2 must be a directory");
    else
      status = false;
      msg = "when copying multiple files, F2 must be a directory";
      msgid = "copyfile";
      return;
    endif
  endif

  ## Protect the filename(s).
  if (ispc ())
    f1 = __wglob__ (f1);
  else
    f1 = glob (f1);
  endif
  if (isempty (f1))
    if (nargout == 0)
      error ("copyfile: no files to move");
    else
      status = false;
      msg = "no files to move";
      msgid = "copyfile";
      return;
    endif
  endif
  p1 = sprintf ('"%s" ', f1{:});
  p2 = tilde_expand (f2);

  if (isdir && length (p1) > max_cmd_line)
    l2 = length (p2) + length (cmd) + 6;
    while (! isempty (f1))
      p1 = sprintf ('"%s" ', f1{1});
      f1(1) = [];
      while (! isempty (f1)
             && (length (p1) + length (f1{1}) + l2 < max_cmd_line))
        p1 = sprintf ('%s"%s" ', p1, f1{1});
        f1(1) = [];
      endwhile

      if (ispc () && ! isunix ()
          && ! isempty (file_in_path (getenv ("PATH"), "cp.exe")))
        p1 = strrep (p1, '\', '/');
        p2 = strrep (p2, '\', '/');
      endif

      ## Copy the files.
      [err, msg] = system (sprintf ('%s %s"%s"', cmd, p1, p2));
      if (err != 0)
        sts = false;
        msgid = "copyfile";
        break;
      endif
    endwhile
  else
    if (ispc () && ! isunix ()
        && ! isempty (file_in_path (getenv ("PATH"), "cp.exe")))
      p1 = strrep (p1, '\', '/');
      p2 = strrep (p2, '\', '/');
    endif

    ## Copy the files.
    [err, msg] = system (sprintf ('%s %s"%s"', cmd, p1, p2));
    if (err != 0)
      sts = false;
      msgid = "copyfile";
    endif
  endif

  if (nargout == 0)
    if (! sts)
      error ("copyfile: operation failed: %s", msg);
    endif
  else
    status = sts;
  endif

endfunction


%!test
%! unwind_protect
%!   f1 = tempname ();
%!   tmp_var = pi;
%!   save (f1, "tmp_var");
%!   f2 = tempname ();
%!   assert (copyfile (f1, f2));
%!   assert (exist (f2, "file"));
%!   fid = fopen (f1, "rb");
%!   assert (fid >= 0);
%!   orig_data = fread (fid);
%!   fclose (fid);
%!   fid = fopen (f2, "rb");
%!   assert (fid >= 0);
%!   new_data = fread (fid);
%!   fclose (fid);
%!   if (orig_data != new_data)
%!     error ("copied file not equal to original file!");
%!   endif
%! unwind_protect_cleanup
%!   delete (f1);
%!   delete (f2);
%! end_unwind_protect

## Test input validation
%!error <Invalid call> copyfile ()
%!error <Invalid call> copyfile (1)
%!error <F1 must be a string> copyfile (1, "foobar")
%!error <F2 must be a string> copyfile ("foobar", 1)
%!error <F2 must be a directory> copyfile ({"a", "b"}, "%_NOT_A_DIR_%")
%!error <no files to move> copyfile ("%_NOT_A_FILENAME1_%", "%_NOT_A_FILENAME2_%")
