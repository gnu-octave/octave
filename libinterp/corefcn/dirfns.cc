////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1994-2023 The Octave Project Developers
//
// See the file COPYRIGHT.md in the top-level directory of this
// distribution or <https://octave.org/copyright/>.
//
// This file is part of Octave.
//
// Octave is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// Octave is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Octave; see the file COPYING.  If not, see
// <https://www.gnu.org/licenses/>.
//
////////////////////////////////////////////////////////////////////////

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <cerrno>
#include <cstdio>
#include <cstddef>
#include <cstdlib>
#include <cstring>

#include <sstream>
#include <string>

#include "file-ops.h"
#include "file-stat.h"
#include "glob-match.h"
#include "oct-env.h"
#include "oct-glob.h"
#include "pathsearch.h"
#include "str-vec.h"

#include "Cell.h"
#include "defun.h"
#include "dir-ops.h"
#include "error.h"
#include "errwarn.h"
#include "event-manager.h"
#include "input.h"
#include "load-path.h"
#include "octave.h"
#include "ovl.h"
#include "pager.h"
#include "procstream.h"
#include "sysdep.h"
#include "interpreter.h"
#include "unwind-prot.h"
#include "utils.h"
#include "variables.h"

OCTAVE_BEGIN_NAMESPACE(octave)

// TRUE means we ask for confirmation before recursively removing a
// directory tree.
static bool Vconfirm_recursive_rmdir = true;

DEFMETHOD (cd, interp, args, nargout,
           doc: /* -*- texinfo -*-
@deftypefn  {} {} cd @var{dir}
@deftypefnx {} {} cd
@deftypefnx {} {@var{old_dir} =} cd
@deftypefnx {} {@var{old_dir} =} cd (@var{dir})
@deftypefnx {} {} chdir @dots{}
Change the current working directory to @var{dir}.

If called with no input or output arguments, the current directory is
changed to the user's home directory (@qcode{"~"}).

For example,

@example
cd ~/octave
@end example

@noindent
changes the current working directory to @file{~/octave}.  If the
directory does not exist, an error message is printed and the working
directory is not changed.

Programming Note: @code{chdir} is an alias for @code{cd} and can be used with
all of the same calling formats.

Compatibility Note: When called with no arguments, @sc{matlab} prints the
present working directory rather than changing to the user's home directory.
@seealso{pwd, mkdir, rmdir, dir, ls}
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin > 1)
    print_usage ();

  octave_value_list retval;

  if (nargout > 0)
    retval = octave_value (sys::env::get_current_directory ());

  if (nargin == 1)
    {
      std::string dirname = args(0).xstring_value ("cd: DIR must be a string");

      if (! dirname.empty ())
        interp.chdir (dirname);
    }
  else if (nargout == 0)
    {
      std::string home_dir = sys::env::get_home_directory ();

      if (! home_dir.empty ())
        interp.chdir (home_dir);
    }

  return retval;
}

DEFALIAS (chdir, cd);

DEFUN (pwd, , ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{dir} =} pwd ()
Return the current working directory.
@seealso{cd, dir, ls, mkdir, rmdir}
@end deftypefn */)
{
  return ovl (sys::env::get_current_directory ());
}

DEFUN (readdir, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{files} =} readdir (@var{dir})
@deftypefnx {} {[@var{files}, @var{err}, @var{msg}] =} readdir (@var{dir})
Return the names of files in the directory @var{dir} as a cell array of
strings.

If an error occurs, return an empty cell array in @var{files}.
If successful, @var{err} is 0 and @var{msg} is an empty string.
Otherwise, @var{err} is nonzero and @var{msg} contains a system-dependent
error message.
@seealso{ls, dir, glob, what}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  std::string dirname = args(0).xstring_value ("readdir: DIR must be a string");

  octave_value_list retval = ovl (Cell (), -1.0, "");

  dirname = sys::file_ops::tilde_expand (dirname);

  string_vector dirlist;
  std::string msg;

  if (sys::get_dirlist (dirname, dirlist, msg))
    {
      retval(0) = Cell (dirlist.sort ());
      retval(1) = 0.0;
    }
  else
    retval(2) = msg;

  return retval;
}

// FIXME: should maybe also allow second arg to specify mode?
//        OTOH, that might cause trouble with compatibility later...

DEFUN (__mkdir__, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {} __mkdir__ (@var{dir})
@deftypefnx {} {} __mkdir__ (@var{parent}, @var{dir})
@deftypefnx {} {[@var{status}, @var{msg}, @var{msgid}] =} __mkdir__ (@dots{})
Internal function called by mkdir.m.
@seealso{mkdir, rmdir, pwd, cd, umask}
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin < 1 || nargin > 2)
    print_usage ("mkdir");

  std::string dirname;

  if (nargin == 2)
    {
      std::string parent = args(0).xstring_value ("mkdir: PARENT must be a string");
      std::string dir = args(1).xstring_value ("mkdir: DIR must be a string");

      dirname = sys::file_ops::concat (parent, dir);
    }
  else if (nargin == 1)
    dirname = args(0).xstring_value ("mkdir: DIR must be a string");

  dirname = sys::file_ops::tilde_expand (dirname);

  sys::file_stat fs (dirname);

  if (fs && fs.is_dir ())
    {
      // For Matlab compatibility, return true when directory already exists.
      return ovl (true, "directory exists", "mkdir");
    }
  else
    {
      std::string msg;

      int status = sys::mkdir (dirname, 0777, msg);

      if (status < 0)
        return ovl (false, msg, "mkdir");
      else
        return ovl (true, "", "");
    }
}

DEFMETHODX ("rmdir", Frmdir, interp, args, nargout,
            doc: /* -*- texinfo -*-
@deftypefn  {} {} rmdir @var{dir}
@deftypefnx {} {} rmdir (@var{dir}, "s")
@deftypefnx {} {[@var{status}, @var{msg}, @var{msgid}] =} rmdir (@dots{})
Remove the directory named @var{dir}.

If the optional second parameter is supplied with value @qcode{"s"},
recursively remove all subdirectories as well.

If successful, @var{status} is logical 1, and @var{msg}, @var{msgid} are empty
character strings ("").  Otherwise, @var{status} is logical 0, @var{msg}
contains a system-dependent error message, and @var{msgid} contains a unique
message identifier.

@seealso{mkdir, confirm_recursive_rmdir, pwd}
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin < 1 || nargin > 2)
    print_usage ();

  std::string dirname = args(0).xstring_value ("rmdir: DIR must be a string");

  std::string fulldir = sys::file_ops::tilde_expand (dirname);
  octave_value_list retval;
  int status = -1;
  std::string msg;

  event_manager& evmgr = interp.get_event_manager ();

  if (nargin == 2)
    {
      if (args(1).string_value () != "s")
        error (R"(rmdir: second argument must be "s" for recursive removal)");

      bool doit = true;

      if (interp.interactive ()
          && ! application::forced_interactive ()
          && Vconfirm_recursive_rmdir)
        {
          input_system& input_sys = interp.get_input_system ();

          std::string prompt = "remove entire contents of " + fulldir + "? ";

          doit = input_sys.yes_or_no (prompt);
        }

      if (doit)
        {
          evmgr.file_remove (fulldir, "");
          status = sys::recursive_rmdir (fulldir, msg);
        }
    }
  else
    {
      evmgr.file_remove (fulldir, "");
      status = sys::rmdir (fulldir, msg);
    }

  evmgr.file_renamed (status >= 0);

  if (nargout == 0)
    {
      if (status < 0)
        error ("rmdir: operation failed: %s", msg.c_str ());
    }
  else
    {
      if (status < 0)
        retval = ovl (false, msg, "rmdir");
      else
        retval = ovl (true, "", "");
    }

  return retval;
}

DEFUNX ("link", Flink, args, nargout,
        doc: /* -*- texinfo -*-
@deftypefn  {} {} link @var{old} @var{new}
@deftypefnx {} {[@var{status}, @var{msg}] =} link (@var{old}, @var{new})
Create a new link (also known as a hard link) to an existing file.

If successful, @var{status} is 0 and @var{msg} is an empty string.
Otherwise, @var{status} is -1 and @var{msg} contains a system-dependent
error message.
@seealso{symlink, unlink, readlink, lstat}
@end deftypefn */)
{
  if (args.length () != 2)
    print_usage ();

  std::string from = args(0).xstring_value ("link: OLD must be a string");
  std::string to = args(1).xstring_value ("link: NEW must be a string");

  from = sys::file_ops::tilde_expand (from);
  to = sys::file_ops::tilde_expand (to);

  octave_value_list retval;
  std::string msg;

  int status = sys::link (from, to, msg);

  if (nargout == 0)
    {
      if (status < 0)
        error ("link: operation failed: %s", msg.c_str ());
    }
  else
    {
      if (status < 0)
        retval = ovl (-1.0, msg);
      else
        retval = ovl (0.0, "");
    }

  return retval;
}

DEFUNX ("symlink", Fsymlink, args, nargout,
        doc: /* -*- texinfo -*-
@deftypefn  {} {} symlink @var{old} @var{new}
@deftypefnx {} {[@var{status}, @var{msg}] =} symlink (@var{old}, @var{new})
Create a symbolic link @var{new} which contains the string @var{old}.

If successful, @var{status} is 0 and @var{msg} is an empty string.
Otherwise, @var{status} is -1 and @var{msg} contains a system-dependent
error message.
@seealso{link, unlink, readlink, lstat}
@end deftypefn */)
{
  if (args.length () != 2)
    print_usage ();

  std::string from = args(0).xstring_value ("symlink: OLD must be a string");
  std::string to = args(1).xstring_value ("symlink: NEW must be a string");

  from = sys::file_ops::tilde_expand (from);
  to = sys::file_ops::tilde_expand (to);

  octave_value_list retval;
  std::string msg;

  int status = sys::symlink (from, to, msg);

  if (nargout == 0)
    {
      if (status < 0)
        error ("symlink: operation failed: %s", msg.c_str ());
    }
  else
    {
      if (status < 0)
        retval = ovl (-1.0, msg);
      else
        retval = ovl (0.0, "");
    }

  return retval;
}

DEFUNX ("readlink", Freadlink, args, ,
        doc: /* -*- texinfo -*-
@deftypefn  {} {@var{result} =} readlink @var{symlink}
@deftypefnx {} {[@var{result}, @var{err}, @var{msg}] =} readlink (@var{symlink})
Read the value of the symbolic link @var{symlink}.

If successful, @var{result} contains the contents of the symbolic link
@var{symlink}, @var{err} is 0, and @var{msg} is an empty string.
Otherwise, @var{err} is nonzero and @var{msg} contains a system-dependent
error message.
@seealso{lstat, symlink, link, unlink, delete}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  std::string symlink = args(0).xstring_value ("readlink: SYMLINK must be a string");

  symlink = sys::file_ops::tilde_expand (symlink);

  std::string result, msg;

  int status = sys::readlink (symlink, result, msg);

  if (status < 0)
    return ovl ("", -1.0, msg);
  else
    return ovl (result, status, "");
}

DEFMETHODX ("rename", Frename, interp, args, nargout,
            doc: /* -*- texinfo -*-
@deftypefn  {} {} rename @var{old} @var{new}
@deftypefnx {} {[@var{status}, @var{msg}] =} rename (@var{old}, @var{new})
Change the name of file @var{old} to @var{new}.

If successful, @var{status} is 0 and @var{msg} is an empty string.
Otherwise, @var{status} is -1 and @var{msg} contains a system-dependent
error message.
@seealso{movefile, copyfile, ls, dir}
@end deftypefn */)
{
  if (args.length () != 2)
    print_usage ();

  std::string from = args(0).xstring_value ("rename: OLD must be a string");
  std::string to = args(1).xstring_value ("rename: NEW must be a string");

  from = sys::file_ops::tilde_expand (from);
  to = sys::file_ops::tilde_expand (to);

  octave_value_list retval;
  std::string msg;

  event_manager& evmgr = interp.get_event_manager ();

  evmgr.file_remove (from, to);

  int status = sys::rename (from, to, msg);

  evmgr.file_renamed (status >= 0);

  if (nargout == 0)
    {
      if (status < 0)
        error ("rename: operation failed: %s", msg.c_str ());
    }
  else
    {
      if (status < 0)
        retval = ovl (-1.0, msg);
      else
        retval = ovl (0.0, "");
    }

  return retval;
}

DEFUN (glob, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{cstr} =} glob (@var{pattern})
Given an array of pattern strings (as a char array or a cell array) in
@var{pattern}, return a cell array of filenames that match any of
them, or an empty cell array if no patterns match.

The pattern strings are interpreted as filename globbing patterns (as they
are used by Unix shells).

Within a pattern

@table @code
@item *
matches any string, including the null string,

@item ?
matches any single character, and

@item [@dots{}]
matches any of the enclosed characters.
@end table

Tilde expansion is performed on each of the patterns before looking for
matching filenames.  For example:

@example
ls
   @result{}
      file1  file2  file3  myfile1 myfile1b
glob ("*file1")
   @result{}
      @{
        [1,1] = file1
        [2,1] = myfile1
      @}
glob ("myfile?")
   @result{}
      @{
        [1,1] = myfile1
      @}
glob ("file[12]")
   @result{}
      @{
        [1,1] = file1
        [2,1] = file2
      @}
@end example

Note: On Windows, patterns that contain non-ASCII characters are not
supported.

@seealso{ls, dir, readdir, what}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  string_vector pat
    = args(0).xstring_vector_value ("glob: PATTERN must be a string");

  glob_match pattern (sys::file_ops::tilde_expand (pat));

  return ovl (Cell (pattern.glob ()));
}

/*
%!test
%! tmpdir = tempname ();
%! filename = {"file1", "file2", "file3", "myfile1", "myfile1b"};
%! if (mkdir (tmpdir))
%!   cwd = pwd ();
%!   cd (tmpdir);
%!   if (strcmp (canonicalize_file_name (pwd), canonicalize_file_name (tmpdir)))
%!     a = 0;
%!     for n = 1:5
%!       save (filename{n}, "a");
%!     endfor
%!   else
%!     sts = rmdir (tmpdir);
%!     error ("Couldn't change to temporary directory");
%!   endif
%! else
%!   error ("Couldn't create temporary directory");
%! endif
%! result1 = glob ("*file1");
%! result2 = glob ("myfile?");
%! result3 = glob ("file[12]");
%! for n = 1:5
%!   delete (filename{n});
%! endfor
%! cd (cwd);
%! sts = rmdir (tmpdir);
%! assert (result1, {"file1"; "myfile1"});
%! assert (result2, {"myfile1"});
%! assert (result3, {"file1"; "file2"});

## Check backslash handling on Windows
%!testif ; ispc ()
%! win_dir = getenv ("WINDIR");
%! assert (glob (win_dir), {win_dir});
%! assert (glob ([win_dir, filesep]), {[win_dir, filesep]});
%! win_dir2 = strrep(win_dir, filesep, '/');
%! assert (glob (win_dir2), {win_dir});
%! assert (glob ([win_dir2, '/']), {[win_dir, filesep]});
*/

DEFUN (__wglob__, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{cstr} =} __wglob__ (@var{pattern})
Windows-like glob for dir.

Given an array of pattern strings (as a char array or a cell array) in
@var{pattern}, return a cell array of filenames that match any of
them, or an empty cell array if no patterns match.

The pattern strings are interpreted as filename globbing patterns
(roughly as they are used by Windows dir).

Within a pattern

@table @code
@item *
matches any string, including the null string,

@item ?
matches any single character, and

@item *.*
matches any string, even if no . is present.
@end table

Tilde expansion is performed on each of the patterns before looking for
matching filenames.  For example:

@example
ls
   @result{}
      file1  file2  file3  myfile1 myfile1b
glob ("*file1")
   @result{}
      @{
        [1,1] = file1
        [2,1] = myfile1
      @}
glob ("myfile?")
   @result{}
      @{
        [1,1] = myfile1
      @}
glob ("*.*")
   @result{}
      @{
        [1,1] = file1
        [2,1] = file2
        [3,1] = file3
        [4,1] = myfile1
        [5,1] = myfile1b
      @}
@end example
@seealso{glob, dir}
@end deftypefn */)
{
  if (args.length () == 0)
    return ovl ();

  string_vector pat = args(0).string_vector_value ();

  string_vector pattern (sys::file_ops::tilde_expand (pat));

  return ovl (Cell (sys::windows_glob (pattern)));
}

/*
%!test <*62414>
%! ## get name of current directory and one file in it
%! [~, curr_dir, ext] = fileparts (pwd ());
%! curr_dir = [curr_dir, ext];
%! files = dir ();
%! if (numel (files) < 3)
%!   return;
%! endif
%! ## check some patterns including "." and ".."
%! file_in_pwd = files(3).name;
%! assert (__wglob__ (file_in_pwd), {file_in_pwd});
%! glob_pattern = fullfile (".", file_in_pwd);
%! assert (__wglob__ (glob_pattern), {glob_pattern});
%! glob_pattern = fullfile ("..", curr_dir, file_in_pwd);
%! assert (__wglob__ (glob_pattern), {glob_pattern});
%! glob_pattern = fullfile ("..", curr_dir, "..", ".", curr_dir, ".", file_in_pwd);
%! assert (__wglob__ (glob_pattern), {glob_pattern});

%!test <*62414>
%! old_dir = cd (fileparts (which ("plot.m")));
%! unwind_protect
%!   assert (__wglob__ (fullfile (".", "*.m")), ...
%!           fullfile (".", __wglob__ ("*.m")));
%! unwind_protect_cleanup
%!   cd (old_dir);
%! end_unwind_protect

## retain trailing file separator
%!test <*62414>
%! old_dir = cd (fileparts (which ("plot.m")));
%! unwind_protect
%!   assert (__wglob__ ("private"), {"private"});
%!   assert (__wglob__ ("private/"), {["private", filesep()]});
%!   assert (__wglob__ ("private///"), {["private", filesep()]});
%!   assert (__wglob__ ("./private"), {fullfile(".", "private")});
%!   assert (__wglob__ ("./private/"), ...
%!           {[fullfile(".", "private"), filesep()]});
%!   assert (__wglob__ ("./private///"), ...
%!           {[fullfile(".", "private"), filesep()]});
%!   assert (__wglob__ (["./p*","/"]), ...
%!           {[fullfile(".", "private"), filesep()]});
%! unwind_protect_cleanup
%!   cd (old_dir);
%! end_unwind_protect
*/

DEFUN (__fnmatch__, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{TF} =} fnmatch (@var{pattern}, @var{string})
Return true or false for each element of @var{string} that matches any of
the elements of the string array @var{pattern}, using the rules of
filename pattern matching.

For example:

@example
@group
fnmatch ("a*b", @{"ab"; "axyzb"; "xyzab"@})
     @result{} [ 1; 1; 0 ]
@end group
@end example
@seealso{glob, regexp}
@end deftypefn */)
{
  if (args.length () != 2)
    print_usage ();

  string_vector pat = args(0).string_vector_value ();
  string_vector str = args(1).string_vector_value ();

  glob_match pattern (sys::file_ops::tilde_expand (pat));

  return ovl (pattern.match (str));
}

DEFUN (filesep, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{sep} =} filesep ()
@deftypefnx {} {} filesep ("all")
Return the system-dependent character used to separate directory names.

If @qcode{"all"} is given, the function returns all valid file separators
in the form of a string.  The list of file separators is system-dependent.
It is @samp{/} (forward slash) under UNIX or @w{Mac OS X}, @samp{/} and
@samp{\} (forward and backward slashes) under Windows.
@seealso{pathsep}
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin > 1)
    print_usage ();

  octave_value retval;

  if (nargin == 0)
    retval = sys::file_ops::dir_sep_str ();
  else
    {
      std::string s = args(0).xstring_value ("filesep: argument must be a string");
      if (s != "all")
        error (R"(filesep: argument must be "all")");

      retval = sys::file_ops::dir_sep_chars ();
    }

  return retval;
}

DEFUN (pathsep, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{val} =} pathsep ()
Query the character used to separate directories in a path.
@seealso{filesep}
@end deftypefn */)
{
  if (args.length () > 0)
    print_usage ();

  return ovl (directory_path::path_sep_str ());
}

DEFUN (confirm_recursive_rmdir, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{val} =} confirm_recursive_rmdir ()
@deftypefnx {} {@var{old_val} =} confirm_recursive_rmdir (@var{new_val})
@deftypefnx {} {@var{old_val} =} confirm_recursive_rmdir (@var{new_val}, "local")
Query or set the internal variable that controls whether Octave
will ask for confirmation before recursively removing a directory tree.

When called from inside a function with the @qcode{"local"} option, the
variable is changed locally for the function and any subroutines it calls.
The original variable value is restored when exiting the function.
@seealso{rmdir}
@end deftypefn */)
{
  return set_internal_variable (Vconfirm_recursive_rmdir, args, nargout,
                                "confirm_recursive_rmdir");
}

OCTAVE_END_NAMESPACE(octave)
