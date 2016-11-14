/*

Copyright (C) 1994-2016 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or
(at your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

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
#include "dirfns.h"
#include "error.h"
#include "errwarn.h"
#include "input.h"
#include "load-path.h"
#include "octave.h"
#include "octave-link.h"
#include "ovl.h"
#include "pager.h"
#include "procstream.h"
#include "sysdep.h"
#include "interpreter.h"
#include "unwind-prot.h"
#include "utils.h"
#include "variables.h"

// TRUE means we ask for confirmation before recursively removing a
// directory tree.
static bool Vconfirm_recursive_rmdir = true;

// The time we last time we changed directories.
octave::sys::time Vlast_chdir_time = 0.0;

static int
octave_change_to_directory (const std::string& newdir)
{
  std::string xdir = octave::sys::file_ops::tilde_expand (newdir);

  int cd_ok = octave::sys::env::chdir (xdir);

  if (! cd_ok)
    error ("%s: %s", newdir.c_str (), std::strerror (errno));

  Vlast_chdir_time.stamp ();

  // FIXME: should these actions be handled as a list of functions
  // to call so users can add their own chdir handlers?

  load_path::update ();

  octave_link::change_directory (octave::sys::env::get_current_directory ());

  return cd_ok;
}

DEFUN (cd, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {} cd @var{dir}
@deftypefnx {} {} cd
@deftypefnx {} {@var{old_dir} =} cd (@var{dir})
@deftypefnx {} {} chdir @dots{}
Change the current working directory to @var{dir}.

If @var{dir} is omitted, the current directory is changed to the user's home
directory (@qcode{"~"}).

For example,

@example
cd ~/octave
@end example

@noindent
changes the current working directory to @file{~/octave}.  If the
directory does not exist, an error message is printed and the working
directory is not changed.

@code{chdir} is an alias for @code{cd} and can be used in all of the same
calling formats.

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
    retval = octave_value (octave::sys::env::get_current_directory ());

  if (nargin == 1)
    {
      std::string dirname = args(0).xstring_value ("cd: DIR must be a string");

      if (! dirname.empty ())
        octave_change_to_directory (dirname);
    }
  else
    {
      std::string home_dir = octave::sys::env::get_home_directory ();

      if (! home_dir.empty ())
        octave_change_to_directory (home_dir);
    }

  return retval;
}

DEFALIAS (chdir, cd);

DEFUN (pwd, , ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {} pwd ()
@deftypefnx {} {@var{dir} =} pwd ()
Return the current working directory.
@seealso{cd, dir, ls, mkdir, rmdir}
@end deftypefn */)
{
  return ovl (octave::sys::env::get_current_directory ());
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

  dirname = octave::sys::file_ops::tilde_expand (dirname);

  octave::sys::dir_entry dir (dirname);

  if (dir)
    {
      string_vector dirlist = dir.read ();
      retval(0) = Cell (dirlist.sort ());
      retval(1) = 0.0;
    }
  else
    retval(2) = dir.error ();

  return retval;
}

// FIXME: should maybe also allow second arg to specify mode?
//        OTOH, that might cause trouble with compatibility later...

DEFUN (__mkdir__, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {} __mkdir__ (@var{parent}, @var{dir})
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

      dirname = octave::sys::file_ops::concat (parent, dir);
    }
  else if (nargin == 1)
    dirname = args(0).xstring_value ("mkdir: DIR must be a string");

  dirname = octave::sys::file_ops::tilde_expand (dirname);

  octave::sys::file_stat fs (dirname);

  if (fs && fs.is_dir ())
    {
      // For Matlab compatibility, return true when directory already exists.
      return ovl (true, "directory exists", "mkdir");
    }
  else
    {
      std::string msg;

      int status = octave::sys::mkdir (dirname, 0777, msg);

      if (status < 0)
        return ovl (false, msg, "mkdir");
      else
        return ovl (true, "", "");
    }
}

DEFUNX ("rmdir", Frmdir, args, ,
        doc: /* -*- texinfo -*-
@deftypefn  {} {} rmdir @var{dir}
@deftypefnx {} {} rmdir (@var{dir}, "s")
@deftypefnx {} {[@var{status}, @var{msg}, @var{msgid}] =} rmdir (@dots{})
Remove the directory named @var{dir}.

If the optional second parameter is supplied with value @qcode{"s"},
recursively remove all subdirectories as well.

If successful, @var{status} is 1, and @var{msg}, @var{msgid} are empty
character strings ("").  Otherwise, @var{status} is 0, @var{msg} contains a
system-dependent error message, and @var{msgid} contains a unique message
identifier.

@seealso{mkdir, confirm_recursive_rmdir, pwd}
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin < 1 || nargin > 2)
    print_usage ();

  std::string dirname = args(0).xstring_value ("rmdir: DIR must be a string");

  std::string fulldir = octave::sys::file_ops::tilde_expand (dirname);
  int status = -1;
  std::string msg;

  if (nargin == 2)
    {
      if (args(1).string_value () != "s")
        error ("rmdir: second argument must be \"s\" for recursive removal");

      bool doit = true;

      if (octave::application::interactive ()
          && ! octave::application::forced_interactive ()
          && Vconfirm_recursive_rmdir)
        {
          std::string prompt = "remove entire contents of " + fulldir + "? ";

          doit = octave_yes_or_no (prompt);
        }

      if (doit)
        status = octave::sys::recursive_rmdir (fulldir, msg);
    }
  else
    status = octave::sys::rmdir (fulldir, msg);

  if (status < 0)
    return ovl (false, msg, "rmdir");
  else
    return ovl (true, "", "");
}

DEFUNX ("link", Flink, args, ,
        doc: /* -*- texinfo -*-
@deftypefn  {} {} link @var{old} @var{new}
@deftypefnx {} {[@var{err}, @var{msg}] =} link (@var{old}, @var{new})
Create a new link (also known as a hard link) to an existing file.

If successful, @var{err} is 0 and @var{msg} is an empty string.
Otherwise, @var{err} is nonzero and @var{msg} contains a system-dependent
error message.
@seealso{symlink, unlink, readlink, lstat}
@end deftypefn */)
{
  if (args.length () != 2)
    print_usage ();

  std::string from = args(0).xstring_value ("link: OLD must be a string");
  std::string to = args(1).xstring_value ("link: NEW must be a string");

  from = octave::sys::file_ops::tilde_expand (from);
  to = octave::sys::file_ops::tilde_expand (to);

  std::string msg;

  int status = octave::sys::link (from, to, msg);

  if (status < 0)
    return ovl (-1.0, msg);
  else
    return ovl (status, "");
}

DEFUNX ("symlink", Fsymlink, args, ,
        doc: /* -*- texinfo -*-
@deftypefn  {} {} symlink @var{old} @var{new}
@deftypefnx {} {[@var{err}, @var{msg}] =} symlink (@var{old}, @var{new})
Create a symbolic link @var{new} which contains the string @var{old}.

If successful, @var{err} is 0 and @var{msg} is an empty string.
Otherwise, @var{err} is nonzero and @var{msg} contains a system-dependent
error message.
@seealso{link, unlink, readlink, lstat}
@end deftypefn */)
{
  if (args.length () != 2)
    print_usage ();

  std::string from = args(0).xstring_value ("symlink: OLD must be a string");
  std::string to = args(1).xstring_value ("symlink: NEW must be a string");

  from = octave::sys::file_ops::tilde_expand (from);
  to = octave::sys::file_ops::tilde_expand (to);

  std::string msg;

  int status = octave::sys::symlink (from, to, msg);

  if (status < 0)
    return ovl (-1.0, msg);
  else
    return ovl (status, "");
}

DEFUNX ("readlink", Freadlink, args, ,
        doc: /* -*- texinfo -*-
@deftypefn  {} {} readlink @var{symlink}
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

  symlink = octave::sys::file_ops::tilde_expand (symlink);

  std::string result, msg;

  int status = octave::sys::readlink (symlink, result, msg);

  if (status < 0)
    return ovl ("", -1.0, msg);
  else
    return ovl (result, status, "");
}

DEFUNX ("rename", Frename, args, ,
        doc: /* -*- texinfo -*-
@deftypefn  {} {} rename @var{old} @var{new}
@deftypefnx {} {[@var{err}, @var{msg}] =} rename (@var{old}, @var{new})
Change the name of file @var{old} to @var{new}.

If successful, @var{err} is 0 and @var{msg} is an empty string.
Otherwise, @var{err} is nonzero and @var{msg} contains a system-dependent
error message.
@seealso{movefile, copyfile, ls, dir}
@end deftypefn */)
{
  if (args.length () != 2)
    print_usage ();

  std::string from = args(0).xstring_value ("rename: OLD must be a string");
  std::string to = args(1).xstring_value ("rename: NEW must be a string");

  from = octave::sys::file_ops::tilde_expand (from);
  to = octave::sys::file_ops::tilde_expand (to);

  std::string msg;

  int status = octave::sys::rename (from, to, msg);

  if (status < 0)
    return ovl (-1.0, msg);
  else
    return ovl (status, "");
}

DEFUN (glob, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {} glob (@var{pattern})
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
@seealso{ls, dir, readdir, what}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  string_vector pat = args(0).xstring_vector_value ("glob: PATTERN must be a string");

  glob_match pattern (octave::sys::file_ops::tilde_expand (pat));

  return ovl (Cell (pattern.glob ()));
}


DEFUN (__wglob__, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {} __wglob__ (@var{pattern})
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

  string_vector pattern (octave::sys::file_ops::tilde_expand (pat));

  return ovl (Cell (octave::sys::windows_glob (pattern)));
}

/*
%!test
%! tmpdir = tempname;
%! filename = {"file1", "file2", "file3", "myfile1", "myfile1b"};
%! if (mkdir (tmpdir))
%!   cwd = pwd;
%!   cd (tmpdir);
%!   if (strcmp (canonicalize_file_name (pwd), canonicalize_file_name (tmpdir)))
%!     a = 0;
%!     for n = 1:5
%!       save (filename{n}, "a");
%!     endfor
%!   else
%!     rmdir (tmpdir);
%!     error ("Couldn't change to temporary dir");
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
%! rmdir (tmpdir);
%! assert (result1, {"file1"; "myfile1"});
%! assert (result2, {"myfile1"});
%! assert (result3, {"file1"; "file2"});
*/

DEFUN (__fnmatch__, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {} fnmatch (@var{pattern}, @var{string})
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

  glob_match pattern (octave::sys::file_ops::tilde_expand (pat));

  return ovl (pattern.match (str));
}

DEFUN (filesep, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {} filesep ()
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
    retval = octave::sys::file_ops::dir_sep_str ();
  else
    {
      std::string s = args(0).xstring_value ("filesep: argument must be a string");
      if (s != "all")
        error ("filesep: argument must be \"all\"");

      retval = octave::sys::file_ops::dir_sep_chars ();
    }

  return retval;
}

DEFUN (pathsep, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{val} =} pathsep ()
@deftypefnx {} {@var{old_val} =} pathsep (@var{new_val})
Query or set the character used to separate directories in a path.
@seealso{filesep}
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin > 1)
    print_usage ();

  octave_value retval;

  if (nargout > 0 || nargin == 0)
    retval = octave::directory_path::path_sep_str ();

  if (nargin == 1)
    {
      std::string sval = args(0).xstring_value ("pathsep: argument must be a single character");

      switch (sval.length ())
        {
        case 1:
          octave::directory_path::path_sep_char (sval[0]);
          break;

        case 0:
          octave::directory_path::path_sep_char ('\0');
          break;

        default:
          error ("pathsep: argument must be a single character");
          break;
        }
    }

  return retval;
}

DEFUN (confirm_recursive_rmdir, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{val} =} confirm_recursive_rmdir ()
@deftypefnx {} {@var{old_val} =} confirm_recursive_rmdir (@var{new_val})
@deftypefnx {} {} confirm_recursive_rmdir (@var{new_val}, "local")
Query or set the internal variable that controls whether Octave
will ask for confirmation before recursively removing a directory tree.

When called from inside a function with the @qcode{"local"} option, the
variable is changed locally for the function and any subroutines it calls.
The original variable value is restored when exiting the function.
@seealso{rmdir}
@end deftypefn */)
{
  return SET_INTERNAL_VARIABLE (confirm_recursive_rmdir);
}

