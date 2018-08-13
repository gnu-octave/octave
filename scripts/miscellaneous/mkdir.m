## Copyright (C) 2016-2018 Lachlan Andrew
## Copyright (C) 2012 Carnë Draug
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

## -*- texinfo -*-
## @deftypefn  {} {} mkdir @var{dirname}
## @deftypefnx {} {} mkdir @var{parent} @var{dirname}
## @deftypefnx {} {} mkdir (@var{dirname})
## @deftypefnx {} {} mkdir (@var{parent}, @var{dirname})
## @deftypefnx {} {[@var{status}, @var{msg}, @var{msgid}] =} mkdir (@dots{})
## Create a directory named @var{dirname} in the directory @var{parent},
## creating any intermediate directories if necessary.
##
## If @var{dir} is a relative path, and no @var{parent} directory is specified,
## then the present working directory is used.
##
## If successful, @var{status} is 1, and @var{msg} and @var{msgid} are empty
## strings ("").  Otherwise, @var{status} is 0, @var{msg} contains a
## system-dependent error message, and @var{msgid} contains a unique message
## identifier.
##
## When creating a directory permissions will be set to @code{0777 - UMASK}.
##
## @seealso{rmdir, pwd, cd, umask}
## @end deftypefn

## There is/was a bug in gnulib's mkdir-p module under Windows.
## This file is a workaround until that is fixed and the fix incorporated
## into Octave.

function [status, msg, msgid] = mkdir (parent, dirname)

  if (nargin < 1 || nargin > 2)
    print_usage ();
  endif

  if (nargin == 1)
    dirname = parent;

    if (is_absolute_filename (tilde_expand (dirname)))
      parent = "";
    else
      parent = [pwd(), filesep];
    endif
  else
    parent = [parent, filesep];
  endif

  ## Move leading directory names from dirname to parent
  [parent, dirname, ext] = fileparts ([parent, dirname]);

  [status, msg, msgid] = mkdir_recur (parent, [dirname, ext]);

endfunction

## Recursively make directories until parent/dirname can be created.
function [status, msg, msgid] = mkdir_recur (parent, dirname)

  status = 1;

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
%!   rmdir (dir1, "s");
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
%!   rmdir (fullfile (tmp_dir, "subdir"));
%!   rmdir (tmp_dir);
%!   if (isempty (HOME))
%!     unsetenv ("HOME");
%!   else
%!     setenv ("HOME", HOME);
%!   endif
%! end_unwind_protect

## Test input validation
%!error mkdir ()
%!error mkdir ("a", "b", "c")
