/*

Copyright (C) 1996, 1997 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, write to the Free
Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cerrno>
#include <cstdio>
#include <cstddef>
#include <cstdlib>
#include <cstring>

#include <sstream>
#include <string>

#ifdef HAVE_UNISTD_H
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#include <unistd.h>
#endif

#include "file-ops.h"
#include "file-stat.h"
#include "glob-match.h"
#include "oct-env.h"
#include "pathsearch.h"
#include "str-vec.h"

#include "Cell.h"
#include "defun.h"
#include "dir-ops.h"
#include "dirfns.h"
#include "error.h"
#include "gripes.h"
#include "input.h"
#include "oct-obj.h"
#include "pager.h"
#include "procstream.h"
#include "sysdep.h"
#include "toplev.h"
#include "unwind-prot.h"
#include "utils.h"
#include "variables.h"

// TRUE means we ask for confirmation before recursively removing a
// directory tree.
static bool Vconfirm_recursive_rmdir = true;

// FIXME -- changing the plotter directory should be handled
// by registering a function for octave_env::chdir to call so that
// this function can be eliminated.

static int
octave_change_to_directory (const std::string& newdir)
{
  int cd_ok = octave_env::chdir (newdir);

  if (cd_ok)
    // FIXME -- this should be handled as a list of functions
    // to call so users can add their own chdir handlers.
    /* do_external_plotter_cd (newdir) */;
  else
    {
      using namespace std;

      error ("%s: %s", newdir.c_str (), strerror (errno));
    }

  return cd_ok;
}

DEFCMD (cd, args, ,
  "-*- texinfo -*-\n\
@deffn {Command} cd dir\n\
@deffnx {Command} chdir dir\n\
Change the current working directory to @var{dir}.  If @var{dir} is\n\
omitted, the current directory is changed to the users home\n\
directory.  For example,\n\
\n\
@example\n\
cd ~/octave\n\
@end example\n\
\n\
@noindent\n\
Changes the current working directory to @file{~/octave}.  If the\n\
directory does not exist, an error message is printed and the working\n\
directory is not changed.\n\
@seealso{mkdir, rmdir, dir}\n\
@end deffn")
{
  octave_value_list retval;

  int argc = args.length () + 1;

  string_vector argv = args.make_argv ("cd");

  if (error_state)
    return retval;

  if (argc > 1)
    {
      std::string dirname = file_ops::tilde_expand (argv[1]);

      if (dirname.length () > 0
	  && ! octave_change_to_directory (dirname))
	{
	  return retval;
	}
    }
  else
    {
      std::string home_dir = octave_env::get_home_directory ();

      if (home_dir.empty () || ! octave_change_to_directory (home_dir))
	return retval;
    }

  return retval;
}

DEFALIAS (chdir, cd);

// Get a directory listing.

static void
cleanup_iprocstream (void *p)
{
  delete static_cast <iprocstream *> (p);
}

DEFCMD (ls, args, nargout,
  "-*- texinfo -*-\n\
@deffn {Command} ls options\n\
List directory contents.  For example,\n\
\n\
@example\n\
ls -l\n\
     @print{} total 12\n\
     @print{} -rw-r--r--   1 jwe  users  4488 Aug 19 04:02 foo.m\n\
     @print{} -rw-r--r--   1 jwe  users  1315 Aug 17 23:14 bar.m\n\
@end example\n\
\n\
The @code{dir} and @code{ls} commands are implemented by calling your\n\
system's directory listing command, so the available options may vary\n\
from system to system.\n\
@seealso{dir, stat, readdir, glob, filesep}\n\
@end deffn")
{
  octave_value retval;

  int argc = args.length () + 1;

  string_vector argv = args.make_argv ("ls");

  if (error_state)
    return retval;

  std::ostringstream ls_buf;

  ls_buf << "ls -C ";
  for (int i = 1; i < argc; i++)
    ls_buf << file_ops::tilde_expand (argv[i]) << " ";

  iprocstream *cmd = new iprocstream (ls_buf.str ());

  unwind_protect::add (cleanup_iprocstream, cmd);

  if (cmd && *cmd)
    {
      char ch;

      std::ostringstream output_buf;

      for (;;)
	{
	  if (cmd->get (ch))
	    output_buf << ch;
	  else
	    break;
	}

      std::string output = output_buf.str ();

      if (nargout > 0)
	retval = output;
      else
	octave_stdout << output;
    }
  else
    error ("couldn't start process for ls!");

  unwind_protect::run ();

  return retval;
}

DEFUN (pwd, , nargout,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} pwd ()\n\
Return the current working directory.\n\
@seealso{dir, ls}\n\
@end deftypefn")
{
  octave_value retval;

  std::string directory = octave_env::getcwd ();

  if (directory.empty ())
    warning ("pwd: can't find working directory!");
  else
    {
      if (nargout == 0)
	octave_stdout << directory << "\n";
      else
	retval = directory;
    }

  return retval;
}

DEFUN (readdir, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {[@var{files}, @var{err}, @var{msg}] =} readdir (@var{dir})\n\
Return names of the files in the directory @var{dir} as a cell array of\n\
strings.  If an error occurs, return an empty cell array in @var{files}.\n\
\n\
If successful, @var{err} is 0 and @var{msg} is an empty string.\n\
Otherwise, @var{err} is nonzero and @var{msg} contains a\n\
system-dependent error message.\n\
@seealso{dir, glob}\n\
@end deftypefn")
{
  octave_value_list retval;

  retval(2) = std::string ();
  retval(1) = -1.0;
  retval(0) = Cell ();

  if (args.length () == 1)
    {
      std::string dirname = args(0).string_value ();

      if (error_state)
	gripe_wrong_type_arg ("readdir", args(0));
      else
	{
	  dir_entry dir (file_ops::tilde_expand (dirname));

	  if (dir)
	    {
	      string_vector dirlist = dir.read ();
	      retval(0) = Cell (dirlist.qsort ());
	      retval(1) = 0.0;
	    }
	  else
	    {
	      retval(2) = dir.error ();
	    }
	}
    }
  else
    print_usage ();

  return retval;
}

// FIXME -- should maybe also allow second arg to specify
// mode?  OTOH, that might cause trouble with compatibility later...

DEFCMD (mkdir, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {[@var{status}, @var{msg}, @var{msgid}] =} mkdir (@var{dir})\n\
Create a directory named @var{dir}.\n\
\n\
If successful, @var{status} is 1, with @var{msg} and @var{msgid} empty\n\
character strings.  Otherwise, @var{status} is 0, @var{msg} contains a\n\
system-dependent error message, and @var{msgid} contains a unique\n\
message identifier.\n\
@seealso{rmdir}\n\
@end deftypefn")
{
  octave_value_list retval;

  retval(2) = std::string ();
  retval(1) = std::string ();
  retval(0) = false;

  if (args.length () == 1)
    {
      std::string dirname = args(0).string_value ();

      if (error_state)
	gripe_wrong_type_arg ("mkdir", args(0));
      else
	{
	  std::string msg;

	  int status = file_ops::mkdir (file_ops::tilde_expand (dirname),
					0777, msg);

	  if (status < 0)
	    {
	      retval(2) = "mkdir";
	      retval(1) = msg;
	    }
	  else
	    retval(0) = true;
	}
    }
  else
    print_usage ();

  return retval;
}

DEFCMD (rmdir, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {[@var{status}, @var{msg}, @var{msgid}] =} rmdir (@var{dir})\n\
@deftypefnx {Built-in Function} {[@var{status}, @var{msg}, @var{msgid}] =} rmdir (@var{dir}, @code{\"s\"})\n\
Remove the directory named @var{dir}.\n\
\n\
If successful, @var{status} is 1, with @var{msg} and @var{msgid} empty\n\
character strings.  Otherwise, @var{status} is 0, @var{msg} contains a\n\
system-dependent error message, and @var{msgid} contains a unique\n\
message identifier.\n\
\n\
If the optional second parameter is suplied, recursively remove all\n\
subdirectories as well.\n\
@seealso{mkdir, confirm_recursive_rmdir}\n\
@end deftypefn")
{
  octave_value_list retval;

  retval(2) = std::string ();
  retval(1) = std::string ();
  retval(0) = false;

  int nargin = args.length ();

  if (nargin == 1 || nargin == 2)
    {
      std::string dirname = args(0).string_value ();

      if (error_state)
	gripe_wrong_type_arg ("rmdir", args(0));
      else
	{
	  std::string fulldir = file_ops::tilde_expand (dirname);
	  int status = -1;
	  std::string msg;

	  if (nargin == 2)
	    {
	      if (args(1).string_value () == "s")
		{
		  bool doit = true;

		  if (interactive && Vconfirm_recursive_rmdir)
		    {
		      std::string prompt
			= "remove entire contents of " + fulldir + "? ";

		      doit = octave_yes_or_no (prompt);
		    }

		  if (doit)
		    status = file_ops::recursive_rmdir (fulldir, msg);
		}
	      else
		error ("rmdir: expecting second argument to be \"s\"");
	    }
	  else
	    status = file_ops::rmdir (fulldir, msg);

	  if (status < 0)
	    {
	      retval(2) = "rmdir";
	      retval(1) = msg;
	    }
	  else
	    retval(0) = true;
	}
    }
  else
    print_usage ();

  return retval;
}

DEFUN (link, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {[@var{err}, @var{msg}] =} link (@var{old}, @var{new})\n\
Create a new link (also known as a hard link) to an existing file.\n\
\n\
If successful, @var{err} is 0 and @var{msg} is an empty string.\n\
Otherwise, @var{err} is nonzero and @var{msg} contains a\n\
system-dependent error message.\n\
@seealso{symlink}\n\
@end deftypefn")
{
  octave_value_list retval;

  retval(1) = std::string ();
  retval(0) = -1.0;

  if (args.length () == 2)
    {
      std::string from = args(0).string_value ();

      if (error_state)
	gripe_wrong_type_arg ("link", args(0));
      else
	{
	  std::string to = args(1).string_value ();

	  if (error_state)
	    gripe_wrong_type_arg ("link", args(1));
	  else
	    {
	      std::string msg;

	      int status = file_ops::link (from, to, msg);

	      retval(0) = status;

	      if (status < 0)
		retval(1) = msg;
	    }
	}
    }
  else
    print_usage ();

  return retval;
}

DEFUN (symlink, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {[@var{err}, @var{msg}] =} symlink (@var{old}, @var{new})\n\
Create a symbolic link @var{new} which contains the string @var{old}.\n\
\n\
If successful, @var{err} is 0 and @var{msg} is an empty string.\n\
Otherwise, @var{err} is nonzero and @var{msg} contains a\n\
system-dependent error message.\n\
@seealso{link, readlink}\n\
@end deftypefn")
{
  octave_value_list retval;

  retval(1) = std::string ();
  retval(0) = -1.0;

  if (args.length () == 2)
    {
      std::string from = args(0).string_value ();

      if (error_state)
	gripe_wrong_type_arg ("symlink", args(0));
      else
	{
	  std::string to = args(1).string_value ();

	  if (error_state)
	    gripe_wrong_type_arg ("symlink", args(1));
	  else
	    {
	      std::string msg;

	      int status = file_ops::symlink (from, to, msg);

	      retval(0) = status;

	      if (status < 0)
		retval(1) = msg;
	    }
	}
    }
  else
    print_usage ();

  return retval;
}

DEFUN (readlink, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {[@var{result}, @var{err}, @var{msg}] =} readlink (@var{symlink})\n\
Read the value of the symbolic link @var{symlink}.\n\
\n\
If successful, @var{result} contains the contents of the symbolic link\n\
@var{symlink}, @var{err} is 0 and @var{msg} is an empty string.\n\
Otherwise, @var{err} is nonzero and @var{msg} contains a\n\
system-dependent error message.\n\
@seealso{link, symlink}\n\
@end deftypefn")
{
  octave_value_list retval;

  retval(2) = std::string ();
  retval(1) = -1.0;
  retval(0) = std::string ();

  if (args.length () == 1)
    {
      std::string symlink = args(0).string_value ();

      if (error_state)
	gripe_wrong_type_arg ("readlink", args(0));
      else
	{
	  std::string result;
	  std::string msg;

	  int status = file_ops::readlink (symlink, result, msg);

	  retval(0) = result;

	  retval(1) = status;

	  if (status < 0)
	    retval(2) = msg;
	}
    }
  else
    print_usage ();

  return retval;
}

DEFUN (rename, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {[@var{err}, @var{msg}] =} rename (@var{old}, @var{new})\n\
Change the name of file @var{old} to @var{new}.\n\
\n\
If successful, @var{err} is 0 and @var{msg} is an empty string.\n\
Otherwise, @var{err} is nonzero and @var{msg} contains a\n\
system-dependent error message.\n\
@seealso{ls, dir}\n\
@end deftypefn")
{
  octave_value_list retval;

  retval(1) = std::string ();
  retval(0) = -1.0;

  if (args.length () == 2)
    {
      std::string from = args(0).string_value ();

      if (error_state)
	gripe_wrong_type_arg ("rename", args(0));
      else
	{
	  std::string to = args(1).string_value ();

	  if (error_state)
	    gripe_wrong_type_arg ("rename", args(1));
	  else
	    {
	      std::string msg;

	      int status = file_ops::rename (from, to, msg);

	      retval(0) = status;

	      if (status < 0)
		retval(1) = msg;
	    }
	}
    }
  else
    print_usage ();

  return retval;
}

DEFUN (glob, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} glob (@var{pattern})\n\
Given an array of strings (as a char array or a cell array) in\n\
@var{pattern}, return a cell array of file names that match any of\n\
them, or an empty cell array if no patterns match.  Tilde expansion\n\
is performed on each of the patterns before looking for matching file\n\
names.  For example,\n\
\n\
@example\n\
@group\n\
glob (\"/vm*\")\n\
     @result{} \"/vmlinuz\"\n\
@end group\n\
@end example\n\
@seealso{dir, ls, stat, readdir}\n\
@end deftypefn")
{
  octave_value retval;

  if (args.length () == 1)
    {
      string_vector pat = args(0).all_strings ();

      if (error_state)
	gripe_wrong_type_arg ("glob", args(0));
      else
	{
	  glob_match pattern (file_ops::tilde_expand (pat));

	  retval = Cell (pattern.glob ());
	}
    }
  else
    print_usage ();

  return retval;
}

DEFUN (fnmatch, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} fnmatch (@var{pattern}, @var{string})\n\
Return 1 or zero for each element of @var{string} that matches any of\n\
the elements of the string array @var{pattern}, using the rules of\n\
filename pattern matching.  For example,\n\
\n\
@example\n\
@group\n\
fnmatch (\"a*b\", [\"ab\"; \"axyzb\"; \"xyzab\"])\n\
     @result{} [ 1; 1; 0 ]\n\
@end group\n\
@end example\n\
@end deftypefn")
{
  octave_value retval;

  if (args.length () == 2)
    {
      string_vector pat = args(0).all_strings ();
      string_vector str = args(1).all_strings ();

      if (error_state)
	gripe_wrong_type_arg ("fnmatch", args(0));
      else
	{
	  glob_match pattern (file_ops::tilde_expand (pat));

	  Array<bool> tmp = pattern.match (str);

	  octave_idx_type n = tmp.length ();

	  ColumnVector result (n);

	  for (octave_idx_type i = 0; i < n; i++)
	    result(i) = tmp(i);

	  retval = result;
	}
    }
  else
    print_usage ();

  return retval;
}

DEFUN (filesep, args, ,
    "-*- texinfo -*-\n\
@detypefn {Built-in Function} {} filesep ()\n\
Return the system-dependent character used to separate directory names.\n\
@seealso{pathsep, dir, ls}\n\
@end deftypefn")
{
  octave_value retval;

  if (args.length () == 0)
    retval = file_ops::dir_sep_str;
  else
    print_usage ();

  return retval;
}

DEFUN (pathsep, args, ,
    "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} pathsep ()\n\
Return the system-dependent character used to separate directories in\n\
a path.\n\
@seealso{filesep, dir, ls}\n\
@end deftypefn")
{
  octave_value retval;

  if (args.length () == 0)
    retval = dir_path::path_sep_str;
  else
    print_usage ();

  return retval;
}

DEFUN (confirm_recursive_rmdir, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {@var{val} =} confirm_recursive_rmdir ()\n\
@deftypefnx {Built-in Function} {@var{old_val} =} confirm_recursive_rmdir (@var{new_val})\n\
Query or set the internal variable that controls whether Octave\n\
will ask for confirmation before recursively removing a directory tree.\n\
@end deftypefn")
{
  return SET_INTERNAL_VARIABLE (confirm_recursive_rmdir);
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
