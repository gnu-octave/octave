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

#include <strstream.h>

#include <readline/tilde.h>

#include "defun.h"
#include "dirfns.h"
#include "error.h"
#include "gripes.h"
#include "help.h"
#include "oct-obj.h"
#include "oct-str.h"
#include "octave.h"
#include "pager.h"
#include "pathlen.h"
#include "procstream.h"
#include "statdefs.h"
#include "sysdep.h"
#include "sysdir.h"
#include "tree-const.h"
#include "tree-plot.h"
#include "unwind-prot.h"
#include "utils.h"

// Temp storage for a path.
static char tdir[MAXPATHLEN];

// Non-zero means follow symbolic links that point to directories just
// as if they are real directories.
static int follow_symbolic_links = 1;

// Non-zero means that pwd always give verbatim directory, regardless
// of symbolic link following.
static int verbatim_pwd = 1;

// Remove the last N directories from PATH.  Do not PATH blank.
// PATH must contain enough space for MAXPATHLEN characters.

void
pathname_backup (char *path, int n)
{
  register char *p;

  if (! *path)
    return;

  p = path + (strlen (path) - 1);

  while (n--)
    {
      while (*p == '/' && p != path)
	p--;

      while (*p != '/' && p != path)
	p--;

      *++p = '\0';
    }
}

// Return a pretty pathname.  If the first part of the pathname is the
// same as $HOME, then replace that with `~'.

char *
polite_directory_format (char *name)
{
  int l = home_directory ? strlen (home_directory) : 0;

  if (l > 1 && strncmp (home_directory, name, l) == 0
      && (! name[l] || name[l] == '/'))
    {
      strcpy (tdir + 1, name + l);
      tdir[0] = '~';
      return (tdir);
    }
  else
    return name;
}

// Return 1 if STRING contains an absolute pathname, else 0.

int
absolute_pathname (const char *string)
{
  if (! string || ! *string)
    return 0;

  if (*string == '/')
    return 1;

  if (*string++ == '.')
    {
      if ((! *string) || *string == '/')
	return 1;

      if (*string++ == '.')
	if (! *string || *string == '/')
	  return 1;
    }
  return 0;
}

// Return 1 if STRING is an absolute program name; it is absolute if
// it contains any slashes.  This is used to decide whether or not to
// look up through $PATH.

int
absolute_program (const char *string)
{
  return (strchr (string, '/') != 0);
}

// Return the `basename' of the pathname in STRING (the stuff after
// the last '/').  If STRING is not a full pathname, simply return it.

char *
base_pathname (char *string)
{
  char *p = strrchr (string, '/');

  if (! absolute_pathname (string))
    return (string);

  if (p)
    return (++p);
  else
    return (string);
}

// Turn STRING (a pathname) into an absolute pathname, assuming that
// DOT_PATH contains the symbolic location of '.'.  This always
// returns a new string, even if STRING was an absolute pathname to
// begin with.

char *
make_absolute (const char *string, const char *dot_path)
{
  static char current_path[MAXPATHLEN];
  register char *cp;

  if (! dot_path || *string == '/')
    return strsave (string);

  strcpy (current_path, dot_path);

  if (! current_path[0])
    strcpy (current_path, "./");

  cp = current_path + (strlen (current_path) - 1);

  if (*cp++ != '/')
    *cp++ = '/';

  *cp = '\0';

  while (*string)
    {
      if (*string == '.')
	{
	  if (! string[1])
	    return strsave (current_path);

	  if (string[1] == '/')
	    {
	      string += 2;
	      continue;
	    }

	  if (string[1] == '.' && (string[2] == '/' || ! string[2]))
	    {
	      string += 2;

	      if (*string)
		string++;

	      pathname_backup (current_path, 1);
	      cp = current_path + strlen (current_path);
	      continue;
	    }
	}

      while (*string && *string != '/')
	*cp++ = *string++;

      if (*string)
	*cp++ = *string++;

      *cp = '\0';
    }
  return strsave (current_path);
}

// Has file `A' been modified after time `T'?
//
// case:
//
//   a newer than t         returns    1
//   a older than t         returns    0
//   stat on a fails        returns   -1

int
is_newer (const char *fa, time_t t)
{
  struct stat fa_sb;
  register int fa_stat;
  register int status = 0;

  fa_stat = stat (fa, &fa_sb);
  if (fa_stat != 0)
    status = -1;

  if (status != 0)
    return status;

  return (fa_sb.st_mtime > t);
}

// Return a consed string which is the current working directory.
// FOR_WHOM is the name of the caller for error printing.

char *
get_working_directory (const char *for_whom)
{
  if (! follow_symbolic_links)
    {
      if (the_current_working_directory)
	delete [] the_current_working_directory;

      the_current_working_directory = 0;
    }

  if (! the_current_working_directory)
    {
      char *directory;

      the_current_working_directory = new char [MAXPATHLEN];
      directory = octave_getcwd (the_current_working_directory, MAXPATHLEN);
      if (! directory)
	{
	  message (for_whom, the_current_working_directory);
	  delete [] the_current_working_directory;
	  the_current_working_directory = 0;
	  return 0;
	}
    }

  return the_current_working_directory;
}

// Do the work of changing to the directory NEWDIR.  Handle symbolic
// link following, etc.

static int
change_to_directory (const char *newdir)
{
  char *t;

  if (follow_symbolic_links)
    {
      if (! the_current_working_directory)
	get_working_directory ("cd_links");

      if (the_current_working_directory)
	t = make_absolute (newdir, the_current_working_directory);
      else
	t = strsave (newdir);

      // Get rid of trailing `/'.

      {
	register int len_t = strlen (t);
	if (len_t > 1)
	  {
	    --len_t;
	    if (t[len_t] == '/')
	      t[len_t] = '\0';
	  }
      }

      if (octave_chdir (t) < 0)
	{
	  delete [] t;
	  return 0;
	}

      if (the_current_working_directory)
	strcpy (the_current_working_directory, t);

      delete [] t;
      return 1;
    }
  else
    {
      if (octave_chdir (newdir) < 0)
	return 0;
      else
	return 1;
    }
}

static int
octave_change_to_directory (const char *newdir)
{
  int cd_ok = change_to_directory (newdir);

  if (cd_ok)
    do_external_plotter_cd (newdir);
  else
    error ("%s: %s", newdir, strerror (errno));

  return cd_ok;
}

DEFUN_TEXT ("cd", Fcd, Scd, 2, 1,
  "cd [dir]\n\
\n\
change current working directory\n\
if no arguments are given, the current directory is changed to the\n\
users home directory")
{
  Octave_object retval;

  DEFINE_ARGV("cd");

  if (argc > 1)
    {
      static char *dirname = 0;

      if (dirname)
	free (dirname);

      dirname = tilde_expand (argv[1]);

      if (dirname && ! octave_change_to_directory (dirname))
	{
	  DELETE_ARGV;
	  return retval;
	}
    }
  else
    {
      if (! home_directory || ! octave_change_to_directory (home_directory))
	{
	  DELETE_ARGV;
	  return retval;
	}
    }

  char *directory = get_working_directory ("cd");
  tree_constant *dir = new tree_constant (directory);
  bind_builtin_variable ("PWD", dir, 1);

  DELETE_ARGV;

  return retval;
}

DEFALIAS (chdir, cd);

// Get a directory listing.

DEFUN_TEXT ("ls", Fls, Sls, -1, 1,
  "ls [options]\n\
\n\
print a directory listing")
{
  Octave_object retval;

  DEFINE_ARGV("ls");

  ostrstream ls_buf;

  ls_buf << "ls -C ";
  for (int i = 1; i < argc; i++)
    {
      char *tmp = tilde_expand (argv[i]);
      ls_buf << tmp << " ";
      free (tmp);
    }

  ls_buf << ends;
  char *ls_command = ls_buf.str ();
  delete [] ls_command;

  iprocstream *cmd = new iprocstream (ls_command);

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

  DELETE_ARGV;

  return retval;
}

DEFALIAS (dir, ls);

DEFUN ("pwd", Fpwd, Spwd, 1, 0,
  "pwd (): print current working directory")
{
  Octave_object retval;
  char *directory;

  if (verbatim_pwd)
    {
      char *buffer = new char [MAXPATHLEN];
      directory = octave_getcwd (buffer, MAXPATHLEN);

      if (!directory)
	{
	  warning ("pwd: can't find working directory!");
	  delete buffer;
	}
    }
  else
    {
      directory = get_working_directory ("pwd");
    }

  if (directory)
    {
      char *s = strconcat (directory, "\n");
      retval = s;
      delete [] s;
    }
  return retval;
}

DEFUN ("readdir", Freaddir, Sreaddir, 1, 0,
  "readdir (NAME)\n\
\n\
Return an array of strings containing the list of all files in the
named directory.  If sucessful, returns 0; otherwise an error message
is printed.")
{
  Octave_object retval;
  Octave_str_obj dirlist;
  int status = 0;

  if (args.length () == 1)
    {
      const char *dirname = args(0).string_value ();

      if (error_state)
	{
	  status = -1;
	  gripe_wrong_type_arg ("readdir", args(0));
	}
      else
	{
	  DIR *dir = opendir (dirname);

	  if (dir)
	    {
	      int count = 0;
	      while (readdir (dir))
		count++;

	      rewinddir (dir);

	      dirlist.resize (count);

	      struct dirent *dir_entry;
	      while ((dir_entry = readdir (dir)))
		{
		  if (--count < 0)
		    break;

		  dirlist (count) = dir_entry->d_name;
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
    retval(0) = dirlist;

  return retval;
}

// XXX FIXME XXX -- should probably also allow second arg to specify
// mode.

DEFUN ("mkdir", Fmkdir, Smkdir, 1, 0,
  "mkdir (NAME)\n\
\n\
Create the directory named by NAME.  If successful, returns 0;\n\
otherwise prints an error message.")
{
  Octave_object retval;

  int status = 0;

  if (args.length () == 1)
    {
      const char *dirname = args(0).string_value ();

      if (error_state)
	gripe_wrong_type_arg ("mkdir", args(0));
      else if (mkdir (dirname, 0777) < 0)
	{
	  status = -1;
	  error ("%s", strerror (errno));
	}
    }
  else
    print_usage ("mkdir");

  if (status == 0)
    retval (0) = (double) status;

  return retval;
}

DEFUN ("rmdir", Frmdir, Srmdir, 1, 0,
  "rmdir (NAME)\n\
\n\
Remove the directory named by NAME.  If successful, returns 0;\n\
otherwise prints an error message.")
{
  Octave_object retval;

  int status = 0;

  if (args.length () == 1)
    {
      const char *dirname = args(0).string_value ();

      if (error_state)
	gripe_wrong_type_arg ("rmdir", args(0));
      else if (rmdir (dirname) < 0)
	{
	  status = -1;
	  error ("%s", strerror (errno));
	}
    }
  else
    print_usage ("rmdir");

  if (status == 0)
    retval (0) = (double) status;

  return retval;
}

DEFUN ("rename", Frename, Srename, 1, 0,
  "rename (FROM, TO)\n\
\n\
Rename a file.  If successful, returns 0;\n\
otherwise prints an error message and returns -1.")
{
  Octave_object retval;

  int status = 0;

  if (args.length () == 2)
    {
      const char *from = args(0).string_value ();
      if (error_state)
	gripe_wrong_type_arg ("rename", args(0));
      else
	{
	  const char *to = args(1).string_value ();
	  if (error_state)
	    gripe_wrong_type_arg ("rename", args(1));
	  else if (rename (from, to) < 0)
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
