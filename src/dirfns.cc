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
Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cerrno>
#include <cstdio>
#include <cstddef>
#include <cstdlib>
#include <cstring>

#include <string>

#include <strstream.h>

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
#include "str-vec.h"

#include "defun.h"
#include "dir-ops.h"
#include "dirfns.h"
#include "error.h"
#include "gripes.h"
#include "oct-obj.h"
#include "pager.h"
#include "procstream.h"
#include "pt-plot.h"
#include "sysdep.h"
#include "toplev.h"
#include "unwind-prot.h"
#include "utils.h"
#include "variables.h"

// XXX FIXME XXX -- changing the plotter directory should be handled
// by registering a function for octave_env::chdir to call so that
// this function can be eliminated.

static int
octave_change_to_directory (const string& newdir)
{
  int cd_ok = octave_env::chdir (newdir);

  if (cd_ok)
    do_external_plotter_cd (newdir);
  else
    error ("%s: %s", newdir.c_str (), strerror (errno));

  return cd_ok;
}

DEFUN_TEXT (cd, args, ,
  "cd [dir]\n\
\n\
change current working directory\n\
if no arguments are given, the current directory is changed to the\n\
users home directory")
{
  octave_value_list retval;

  int argc = args.length () + 1;

  string_vector argv = args.make_argv ("cd");

  if (error_state)
    return retval;

  if (argc > 1)
    {
      string dirname = file_ops::tilde_expand (argv[1]);

      if (dirname.length () > 0
	  && ! octave_change_to_directory (dirname))
	{
	  return retval;
	}
    }
  else
    {
      string home_dir = octave_env::get_home_directory ();

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

DEFUN_TEXT (ls, args, ,
  "ls [options]\n\
\n\
print a directory listing")
{
  octave_value_list retval;

  int argc = args.length () + 1;

  string_vector argv = args.make_argv ("ls");

  if (error_state)
    return retval;

  ostrstream ls_buf;

  ls_buf << "ls -C ";
  for (int i = 1; i < argc; i++)
    ls_buf << file_ops::tilde_expand (argv[i]) << " ";

  ls_buf << ends;
  char *ls_command = ls_buf.str ();

  iprocstream *cmd = new iprocstream (ls_command);

  delete [] ls_command;

  unwind_protect::add (cleanup_iprocstream, cmd);

  if (cmd && *cmd)
    {
      int ch;
      while ((ch = cmd->get ()) != EOF)
	octave_stdout << (char) ch;
    }
  else
    error ("couldn't start process for ls!");

  unwind_protect::run ();

  return retval;
}

DEFALIAS (dir, ls);

DEFUN (pwd, , nargout,
  "pwd (): print current working directory")
{
  octave_value_list retval;

  string directory = octave_env::getcwd ();

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
  "[FILES, STATUS, MSG] = readdir (NAME)\n\
\n\
Return an array of strings containing the list of all files in the\n\
named directory in FILES, or an empty matrix if an error occurs\n\
\n\
If successful, STATUS is 0 and MSG is an empty string.  Otherwise,\n\
STATUS is nonzero and MSG contains a system-dependent error message.")
{
  octave_value_list retval;

  retval(2) = string ();
  retval(1) = -1.0;
  retval(0) = Matrix ();

  if (args.length () == 1)
    {
      string dirname = args(0).string_value ();

      if (error_state)
	gripe_wrong_type_arg ("readdir", args(0));
      else
	{
	  dir_entry dir (file_ops::tilde_expand (dirname));

	  if (dir)
	    {
	      string_vector dirlist = dir.read ();
	      retval(0) = dirlist.qsort ();
	      retval(1) = 0.0;
	    }
	  else
	    {
	      retval(2) = dir.error ();
	    }
	}
    }
  else
    print_usage ("readdir");

  return retval;
}

// XXX FIXME XXX -- should probably also allow second arg to specify
// mode.

DEFUN (mkdir, args, ,
  "[STATUS, MSG] = mkdir (NAME)\n\
\n\
Create the directory named by NAME.\n\
\n\
If successful, STATUS is 0 and MSG is an empty string.  Otherwise,\n\
STATUS is nonzero and MSG contains a system-dependent error message.")
{
  octave_value_list retval;

  retval(1) = string ();
  retval(0) = -1.0;

  if (args.length () == 1)
    {
      string dirname = args(0).string_value ();

      if (error_state)
	gripe_wrong_type_arg ("mkdir", args(0));
      else
	{
	  string msg;

	  int status = file_ops::mkdir (file_ops::tilde_expand (dirname),
					0777, msg);

	  retval(0) = static_cast<double> (status);

	  if (status < 0)
	    retval(1) = msg;
	}
    }
  else
    print_usage ("mkdir");

  return retval;
}

DEFUN (rmdir, args, ,
  "[STATUS, MSG] = rmdir (NAME)\n\
\n\
Remove the directory named by NAME.\n\
\n\
If successful, STATUS is 0 and MSG is an empty string.  Otherwise,\n\
STATUS is nonzero and MSG contains a system-dependent error message.")
{
  octave_value_list retval;

  retval(1) = string ();
  retval(0) = -1.0;

  if (args.length () == 1)
    {
      string dirname = args(0).string_value ();

      if (error_state)
	gripe_wrong_type_arg ("rmdir", args(0));
      else
	{
	  string msg;

	  int status = file_ops::rmdir (file_ops::tilde_expand (dirname), msg);

	  retval(0) = static_cast<double> (status);

	  if (status < 0)
	    retval(1) = msg;
	}
    }
  else
    print_usage ("rmdir");

  return retval;
}

DEFUN (rename, args, ,
  "[STATUS, MSG] = rename (FROM, TO)\n\
\n\
Rename a file.\n\
\n\
If successful, STATUS is 0 and MSG is an empty string.  Otherwise,\n\
STATUS is nonzero and MSG contains a system-dependent error message.")
{
  octave_value_list retval;

  retval(1) = string ();
  retval(0) = -1.0;

  if (args.length () == 2)
    {
      string from = args(0).string_value ();

      if (error_state)
	gripe_wrong_type_arg ("rename", args(0));
      else
	{
	  string to = args(1).string_value ();

	  if (error_state)
	    gripe_wrong_type_arg ("rename", args(1));
	  else
	    {
	      string msg;

	      int status = file_ops::rename (from, to, msg);

	      retval(0) = static_cast<double> (status);

	      if (status < 0)
		retval(1) = msg;
	    }
	}
    }
  else
    print_usage ("rename");

  return retval;
}

DEFUN (glob, args, ,
  "glob (PATTERN)\n\
\n\
Given an array of strings in PATTERN, return the list of file names\n\
that any of them, or an empty string if no patterns match.  Tilde\n\
expansion is performed on each of the patterns before looking for\n\
matching file names.")
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

	  string_vector list = pattern.glob ();

	  if (list.empty ())
	    retval = "";
	  else
	    retval = list;
	}
    }
  else
    print_usage ("glob");

  return retval;
}

DEFUN (fnmatch, args, ,
  "fnmatch (PATTERN, STRING)\n\
\n\
Return 1 or zero for each element of STRING that matches any of the\n\
elements of the string array PATTERN, using the rules of filename\n\
pattern matching.")
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

	  int n = tmp.length ();

	  ColumnVector result (n);

	  for (int i = 0; i < n; i++)
	    result(i) = tmp(i);

	  retval = octave_value (result, true);
	}
    }
  else
    print_usage ("fnmatch");

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
