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
## @deftypefn  {} {} movefile @var{f1}
## @deftypefnx {} {} movefile @var{f1} @var{f2}
## @deftypefnx {} {} movefile @var{f1} @var{f2} f
## @deftypefnx {} {} movefile (@var{f1})
## @deftypefnx {} {} movefile (@var{f1}, @var{f2})
## @deftypefnx {} {} movefile (@var{f1}, @var{f2}, 'f')
## @deftypefnx {} {[@var{status}] =} movefile (@dots{})
## @deftypefnx {} {[@var{status}, @var{msg}] =} movefile (@dots{})
## @deftypefnx {} {[@var{status}, @var{msg}, @var{msgid}] =} movefile (@dots{})
## Move the source file or directory @var{f1} to the destination @var{f2}.
##
## The name @var{f1} may contain globbing patterns, or may be a cell array of
## strings.  If @var{f1} expands to multiple filenames, @var{f2} must be a
## directory.
##
## If no destination @var{f2} is specified then the destination is the present
## working directory.  If @var{f2} is a filename then @var{f1} is renamed to
## @var{f2}.
##
## When the force flag @qcode{'f'} is given any existing files will be
## overwritten without prompting.
##
## If successful, @var{status} is logical 1, and @var{msg}, @var{msgid} are
## empty character strings ("").  Otherwise, @var{status} is logical 0,
## @var{msg} contains a system-dependent error message, and @var{msgid}
## contains a unique message identifier.  Note that the status code is exactly
## opposite that of the @code{system} command.
## @seealso{rename, copyfile, unlink, delete, glob}
## @end deftypefn

function [status, msg, msgid] = movefile (f1, f2, force)

  if (nargin < 1)
    print_usage ();
  endif

  max_cmd_line = 1024;
  sts = true;
  msg = "";
  msgid = "";

  ## FIXME: maybe use the same method as in ls to allow users control
  ##        over the command that is executed.

  if (ispc () && ! isunix ()
      && isempty (file_in_path (getenv ("PATH"), "mv.exe")))
    ## Windows.
    cmd = "cmd /C move";
    cmd_force_flag = "/Y";
  else
    cmd = "mv";
    cmd_force_flag = "-f";
  endif

  ## Input type check.
  if (ischar (f1))
    f1 = cellstr (f1);
  elseif (! iscellstr (f1))
    error ("copyfile: F1 must be a string or a cell array of strings");
  endif

  if (nargin == 1)
    f2 = pwd ();
  elseif (! ischar (f2))
    error ("movefile: F2 must be a string");
  endif

  if (nargin == 3 && strcmp (force, "f"))
    cmd = [cmd " " cmd_force_flag];
  endif

  ## If f1 has more than 1 element f2 must be a directory
  isdir = isfolder (f2);
  if (numel (f1) > 1 && ! isdir)
    if (nargout == 0)
      error ("movefile: when copying multiple files, F2 must be a directory");
    else
      status = false;
      msg = "when copying multiple files, F2 must be a directory";
      msgid = "movefile";
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
      error ("movefile: no files to move");
    else
      status = false;
      msg = "no files to move";
      msgid = "movefile";
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

      ## Close old file(s) in editor
      __event_manager_file_remove__ (p1, p2);
      ## Move the file(s).
      [err, msg] = system (sprintf ('%s %s "%s"', cmd, p1, p2));
      if (err != 0)
        sts = false;
        msgid = "movefile";
      endif
      ## Load new file(s) in editor
      __event_manager_file_renamed__ (sts);
    endwhile
  else
    if (ispc () && ! isunix ()
        && ! isempty (file_in_path (getenv ("PATH"), "cp.exe")))
      p1 = strrep (p1, '\', '/');
      p2 = strrep (p2, '\', '/');
    endif

    ## Close old file(s) in editor
    __event_manager_file_remove__ (p1, p2);
    ## Move the file(s).
    [err, msg] = system (sprintf ('%s %s "%s"', cmd, p1, p2));
    if (err != 0)
      sts = false;
      msgid = "movefile";
    endif
    ## Load new file(s) in editor
    __event_manager_file_renamed__ (sts);
  endif

  if (nargout == 0)
    if (! sts)
      error ("movefile: operation failed: %s", msg);
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
%!   fid = fopen (f1, "rb");
%!   assert (fid >= 0);
%!   orig_data = fread (fid);
%!   fclose (fid);
%!   f2 = tempname ();
%!   assert (movefile (f1, f2));
%!   assert (! exist (f1, "file"));
%!   assert (exist (f2, "file"));
%!   fid = fopen (f2, "rb");
%!   assert (fid >= 0);
%!   new_data = fread (fid);
%!   fclose (fid);
%!   if (orig_data != new_data)
%!     error ("moved file not equal to original file!");
%!   endif
%! unwind_protect_cleanup
%!   delete (f2);
%! end_unwind_protect

## Test input validation
%!error <Invalid call> movefile ()
%!error <F1 must be a string> movefile (1, "foobar")
%!error <F2 must be a string> movefile ("foobar", 1)
%!error <F2 must be a directory> movefile ({"a", "b"}, "%_NOT_A_DIR_%")
%!error <no files to move> movefile ("%_NOT_A_FILENAME1_%", "%_NOT_A_FILENAME2_%")
