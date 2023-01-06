########################################################################
##
## Copyright (C) 2012-2023 The Octave Project Developers
##
## See the file COPYRIGHT.md in the top-level directory of this
## distribution or <https://octave.org/copyright/>.
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
## @deftypefn  {} {} mkdir @var{dirname}
## @deftypefnx {} {} mkdir @var{parent} @var{dirname}
## @deftypefnx {} {} mkdir (@var{dirname})
## @deftypefnx {} {} mkdir (@var{parent}, @var{dirname})
## @deftypefnx {} {[@var{status}, @var{msg}, @var{msgid}] =} mkdir (@dots{})
## Create a directory named @var{dirname} in the directory @var{parent},
## creating any intermediate directories if necessary.
##
## If @var{dirname} is a relative path, and no @var{parent} directory is
## specified, then the present working directory is used.
##
## If successful, @var{status} is logical 1, and @var{msg}, @var{msgid} are
## empty character strings ("").  Otherwise, @var{status} is logical 0,
## @var{msg} contains a system-dependent error message, and @var{msgid}
## contains a unique message identifier.  Note that the status code is exactly
## opposite that of the @code{system} command.
##
## When creating a directory permissions will be set to
## @w{@code{0777 - UMASK}}.
##
## @seealso{rmdir, pwd, cd, umask}
## @end deftypefn

## There is/was a bug in gnulib's mkdir-p module under Windows.
## This file is a workaround until that is fixed and the fix incorporated
## into Octave.

function [status, msg, msgid] = mkdir (parent, dirname)

  if (nargin < 1)
    print_usage ();
  endif

  parent = tilde_expand (parent);

  if (nargin == 1)
    dirname = parent;
  else
    dirname = fullfile (parent, dirname);
  endif

  dirname = make_absolute_filename (dirname);

  ## Move leading directory names from dirname to parent
  [parent, dirname, ext] = fileparts (dirname);

  [sts, msg, msgid] = mkdir_recur (parent, [dirname, ext]);

  if (nargout == 0)
    if (! sts)
      error ("mkdir: operation failed: %s", msg);
    elseif (strcmp (msg, "directory exists"))
      warning ("mkdir: directory exists\n");
    endif
  else
    status = sts;
  endif

endfunction

## Recursively make directories until parent/dirname can be created.
function [status, msg, msgid] = mkdir_recur (parent, dirname)

  status = true;

  if (isempty (parent))
    error ("mkdir: invalid PARENT");
  endif

  if (! isfolder (parent))
    [grandparent, name, ext] = fileparts (parent);
    [status, msg, msgid] = mkdir_recur (grandparent, [name, ext]);
  endif

  if (status)
    [status, msg, msgid] = __mkdir__ (parent, dirname);
  endif

endfunction


%!test
%! dir1 = tempname ();
%! dir2 = "%_unlikely_name_%";
%! dir = fullfile (dir1, dir2);
%! unwind_protect
%!   status = mkdir (dir);
%!   assert (status);
%!   assert (isfolder (dir));
%! unwind_protect_cleanup
%!   confirm_recursive_rmdir (false, "local");
%!   sts = rmdir (dir1, "s");
%! end_unwind_protect

%!test <*53031>
%! HOME = getenv ("HOME");
%! tmp_dir = tempname ();
%! unwind_protect
%!   mkdir (tmp_dir);
%!   setenv ("HOME", tmp_dir);
%!   status = mkdir ("~/subdir");
%!   assert (status);
%!   assert (isfolder (fullfile (tmp_dir, "subdir")));
%! unwind_protect_cleanup
%!   sts = rmdir (fullfile (tmp_dir, "subdir"));
%!   sts = rmdir (tmp_dir);
%!   if (isempty (HOME))
%!     unsetenv ("HOME");
%!   else
%!     setenv ("HOME", HOME);
%!   endif
%! end_unwind_protect

## Test input validation
%!error <Invalid call> mkdir ()
