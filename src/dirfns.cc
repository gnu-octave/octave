// dirfns.cc                                           -*- C++ -*-
/*

Copyright (C) 1992, 1993, 1994, 1995 John W. Eaton

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

/*

The functions listed below were adapted from a similar functions
from GNU Bash, the Bourne Again SHell, copyright (C) 1987, 1989, 1991
Free Software Foundation, Inc.

  polite_directory_format  absolute_pathname
  absolute_program         base_pathname
  make_absolute            pathname_backup
  change_to_directory      get_working_directory

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

#include "str-vec.h"

#include "defun.h"
#include "dirfns.h"
#include "error.h"
#include "gripes.h"
#include "help.h"
#include "oct-obj.h"
#include "pager.h"
#include "pathlen.h"
#include "procstream.h"
#include "pt-plot.h"
#include "statdefs.h"
#include "sysdep.h"
#include "sysdir.h"
#include "toplev.h"
#include "unwind-prot.h"
#include "utils.h"
#include "variables.h"

// Non-zero means follow symbolic links that point to directories just
// as if they are real directories.
static int follow_symbolic_links = 1;

// Non-zero means that pwd always give verbatim directory, regardless
// of symbolic link following.
static int verbatim_pwd = 1;

// Remove the last N directories from PATH.  Do not PATH blank.
// PATH must contain enough space for MAXPATHLEN characters.

static void
pathname_backup (string& path, int n)
{
  if (path.empty ())
    return;

  size_t i = path.length () - 1;

  while (n--)
    {
      while (path[i] == '/' && i > 0)
	i--;

      while (path[i] != '/' && i > 0)
	i--;

      i++;
    }

  path.resize (i);
}

// Return a pretty pathname.  If the first part of the pathname is the
// same as $HOME, then replace that with `~'.

string
polite_directory_format (const string& name)
{
  string retval;

  size_t len = home_directory.length ();

  if (len > 1 && home_directory.compare (name, 0, len) == 0
      && (name.length () == len || name[len] == '/'))
    {
      retval = "~";
      retval.append (name.substr (len));
    }
  else
    retval = name;

  return retval;
}

// Return 1 if STRING contains an absolute pathname, else 0.

int
absolute_pathname (const string& s)
{
  if (s.empty ())
    return 0;

  if (s[0] == '/')
    return 1;

  if (s[0] == '.')
    {
      if (s[1] == '\0' || s[1] == '/')
	return 1;

      if (s[1] == '.')
	if (s[2] == '\0' || s[2] == '/')
	  return 1;
    }

  return 0;
}

// Return 1 if STRING is an absolute program name; it is absolute if
// it contains any slashes.  This is used to decide whether or not to
// look up through $PATH.

int
absolute_program (const string& s)
{
  return (s.find ('/') != NPOS);
}

// Return the `basename' of the pathname in STRING (the stuff after
// the last '/').  If STRING is not a full pathname, simply return it.

string
base_pathname (const string& s)
{
  if (! absolute_pathname (s))
    return s;

  size_t pos = s.rfind ('/');

  if (pos == NPOS)
    return s;
  else
    return s.substr (pos+1);
}

// Turn STRING (a pathname) into an absolute pathname, assuming that
// DOT_PATH contains the symbolic location of '.'.

string
make_absolute (const string& s, const string& dot_path)
{
  if (dot_path.empty () || s[0] == '/')
    return s;

  string current_path = dot_path;

  if (current_path.empty ())
    current_path = "./";

  size_t pos = current_path.length () - 1;

  if (current_path[pos] != '/')
    current_path.append ("/");

  size_t i = 0;
  size_t slen = s.length ();

  while (i < slen)
    {
      if (s[i] == '.')
	{
	  if (i + 1 == slen)
	    return current_path;

	  if (s[i+1] == '/')
	    {
	      i += 2;
	      continue;
	    }

	  if (s[i+1] == '.' && (i + 2 == slen || s[i+2] == '/'))
	    {
	      i += 2;

	      if (i != slen)
		i++;

	      pathname_backup (current_path, 1);

	      continue;
	    }
	}

      size_t tmp = s.find ('/', i);

      if (tmp == NPOS)
	{
	  current_path.append (s, i, tmp-i);
	  break;
	}
      else
	{
	  current_path.append (s, i, tmp-i+1);
	  i = tmp + 1;
	}
    }

  return current_path;
}

// Return a consed string which is the current working directory.
// FOR_WHOM is the name of the caller for error printing.

string
get_working_directory (const string& for_whom)
{
  if (! follow_symbolic_links)
    the_current_working_directory = "";

  if (the_current_working_directory.empty ())
    {
      the_current_working_directory = octave_getcwd ();

      if (the_current_working_directory.empty ())
	warning ("%s: can't find current directory!", for_whom.c_str ());
    }

  return the_current_working_directory;
}

// Do the work of changing to the directory NEWDIR.  Handle symbolic
// link following, etc.

static int
change_to_directory (const string& newdir)
{
  string tmp;

  if (follow_symbolic_links)
    {
      if (the_current_working_directory.empty ())
	get_working_directory ("cd_links");

      if (the_current_working_directory.empty ())
	tmp = newdir;
      else
	tmp = make_absolute (newdir, the_current_working_directory);

      // Get rid of trailing `/'.

      size_t len = tmp.length ();

      if (len > 1)
	{
	  if (tmp[--len] == '/')
	    tmp.resize (len);
	}

      if (octave_chdir (tmp) < 0)
	return 0;
      else
	{
	  the_current_working_directory = tmp;
	  return 1;
	}
    }
  else
    return (octave_chdir (newdir) < 0) ? 0 : 1;
}

static int
octave_change_to_directory (const string& newdir)
{
  int cd_ok = change_to_directory (newdir);

  if (cd_ok)
    do_external_plotter_cd (newdir);
  else
    error ("%s: %s", newdir.c_str (), strerror (errno));

  return cd_ok;
}

DEFUN_TEXT ("cd", Fcd, Scd, 10,
  "cd [dir]\n\
\n\
change current working directory\n\
if no arguments are given, the current directory is changed to the\n\
users home directory")
{
  Octave_object retval;

  int argc = args.length () + 1;

  string_vector argv = make_argv (args, "cd");

  if (error_state)
    return retval;

  if (argc > 1)
    {
      string dirname = oct_tilde_expand (argv[1]);

      if (dirname.length () > 0
	  && ! octave_change_to_directory (dirname))
	{
	  return retval;
	}
    }
  else
    {
      if (home_directory.empty ()
	  || ! octave_change_to_directory (home_directory))
	{
	  return retval;
	}
    }

  string directory = get_working_directory ("cd");
  tree_constant *dir = new tree_constant (directory);
  bind_builtin_variable ("PWD", dir, 1);

  return retval;
}

DEFALIAS (chdir, cd);

// Get a directory listing.

DEFUN_TEXT ("ls", Fls, Sls, 10,
  "ls [options]\n\
\n\
print a directory listing")
{
  Octave_object retval;

  int argc = args.length () + 1;

  string_vector argv = make_argv (args, "ls");

  if (error_state)
    return retval;

  ostrstream ls_buf;

  ls_buf << "ls -C ";
  for (int i = 1; i < argc; i++)
    ls_buf << oct_tilde_expand (argv[i]) << " ";

  ls_buf << ends;
  char *ls_command = ls_buf.str ();

  iprocstream *cmd = new iprocstream (ls_command);

  delete [] ls_command;

  add_unwind_protect (cleanup_iprocstream, cmd);

  if (cmd && *cmd)
    {
      int ch;
      ostrstream output_buf;
      while ((ch = cmd->get ()) != EOF)
	output_buf << (char) ch;
      output_buf << ends;

      maybe_page_output (output_buf);
    }
  else
    error ("couldn't start process for ls!");

  run_unwind_protect ();

  return retval;
}

DEFALIAS (dir, ls);

DEFUN ("pwd", Fpwd, Spwd, 01,
  "pwd (): print current working directory")
{
  Octave_object retval;
  string directory;

  if (verbatim_pwd)
    {
      directory = octave_getcwd ();

      if (directory.empty ())
	warning ("pwd: can't find working directory!");
    }
  else
    directory = get_working_directory ("pwd");

  if (! directory.empty ())
    {
      if (nargout == 0)
	{
	  ostrstream output_buf;
	  output_buf << directory << "\n" << ends;
	  maybe_page_output (output_buf);
	}
      else
	retval = directory;
    }

  return retval;
}

DEFUN ("readdir", Freaddir, Sreaddir, 10,
  "readdir (NAME)\n\
\n\
Return an array of strings containing the list of all files in the
named directory.  If sucessful, returns 0; otherwise an error message
is printed.")
{
  Octave_object retval;
  charMatrix dirlist;
  int status = 0;

  if (args.length () == 1)
    {
      string dirname = args(0).string_value ();

      if (error_state)
	{
	  status = -1;
	  gripe_wrong_type_arg ("readdir", args(0));
	}
      else
	{
	  string tmp = oct_tilde_expand (dirname);

	  DIR *dir = opendir (tmp.c_str ());

	  if (dir)
	    {
	      int count = 0;
	      int max_len = 0;

	      struct dirent *dir_entry;

	      while ((dir_entry = readdir (dir)))
		{
		  count++;
		  int len = strlen (dir_entry->d_name);
		  if (len > max_len)
		    max_len = len;
		}

	      rewinddir (dir);

	      dirlist.resize (count, max_len, 0);

	      while ((dir_entry = readdir (dir)))
		{
		  if (--count < 0)
		    break;

		  dirlist.insert (dir_entry->d_name, count, 0);
		}

#if defined (CLOSEDIR_VOID)
	      closedir (dir);
#else
	      if (closedir (dir) < 0)
		{
		  status = -1;
		  error ("%s", strerror (errno));
		}
#endif

	      if (count != 0)
		{
		  status = -1;
		  error ("readdir: failed reading directory");
		}
	    }
	  else
	    {
	      status = -1;
	      error ("%s", strerror (errno));
	    }
	}
    }
  else
    print_usage ("readdir");

  if (status == 0)
    retval(0) = tree_constant (dirlist, 1);

  return retval;
}

// XXX FIXME XXX -- should probably also allow second arg to specify
// mode.

DEFUN ("mkdir", Fmkdir, Smkdir, 10,
  "mkdir (NAME)\n\
\n\
Create the directory named by NAME.  If successful, returns 0;\n\
otherwise prints an error message.")
{
  Octave_object retval;

  int status = 0;

  if (args.length () == 1)
    {
      string dirname = args(0).string_value ();

      if (error_state)
	gripe_wrong_type_arg ("mkdir", args(0));
      else
	{
	  string tmp = oct_tilde_expand (dirname);

	  int mkdir_retval = mkdir (tmp.c_str (), 0777);

	  if (mkdir_retval < 0)
	    {
	      status = -1;
	      error ("%s", strerror (errno));
	    }
	}
    }
  else
    print_usage ("mkdir");

  if (status == 0)
    retval (0) = (double) status;

  return retval;
}

DEFUN ("rmdir", Frmdir, Srmdir, 10,
  "rmdir (NAME)\n\
\n\
Remove the directory named by NAME.  If successful, returns 0;\n\
otherwise prints an error message.")
{
  Octave_object retval;

  int status = 0;

  if (args.length () == 1)
    {
      string dirname = args(0).string_value ();

      if (error_state)
	gripe_wrong_type_arg ("rmdir", args(0));
      else
	{
	  string tmp = oct_tilde_expand (dirname);

	  int rmdir_retval = rmdir (tmp.c_str ());

	  if (rmdir_retval < 0)
	    {
	      status = -1;
	      error ("%s", strerror (errno));
	    }
	}
    }
  else
    print_usage ("rmdir");

  if (status == 0)
    retval (0) = (double) status;

  return retval;
}

DEFUN ("rename", Frename, Srename, 10,
  "rename (FROM, TO)\n\
\n\
Rename a file.  If successful, returns 0;\n\
otherwise prints an error message and returns -1.")
{
  Octave_object retval;

  int status = 0;

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
	  else if (rename (from.c_str (), to.c_str ()) < 0)
	    {
	      status = -1;
	      error ("%s", strerror (errno));
	    }
	}
    }
  else
    print_usage ("rename");

  if (status == 0)
    retval (0) = (double) status;

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
