// dirfns.cc                                           -*- C++ -*-
/*

Copyright (C) 1992, 1993, 1994 John W. Eaton

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
Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

*/

/*

The functions listed below were adapted from similar functions from
the GNU C library, copyright (C) 1991, 1992, 1993, Free Software
Foundation, Inc.

  dir_access    exists    gen_tempname    tempnam

The functions listed below were adapted from a similar functions
from GNU Bash, the Bourne Again SHell, copyright (C) 1987, 1989, 1991
Free Software Foundation, Inc.

  polite_directory_format  absolute_pathname
  absolute_program         base_pathname
  make_absolute            pathname_backup
  change_to_directory      get_working_directory

*/ 

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <sys/types.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strstream.h>
#include <sys/param.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>

// This mess suggested by the autoconf manual.
// unistd.h defines _POSIX_VERSION on POSIX.1 systems.
#if defined(DIRENT) || defined(_POSIX_VERSION)
#include <dirent.h>
#define NLENGTH(dirent) (strlen((dirent)->d_name))
#else /* not (DIRENT or _POSIX_VERSION) */
#define dirent direct
#define NLENGTH(dirent) ((dirent)->d_namlen)
#ifdef SYSNDIR
#include <sys/ndir.h>
#endif /* SYSNDIR */
#ifdef SYSDIR
#include <sys/dir.h>
#endif /* SYSDIR */
#ifdef NDIR
#include <ndir.h>
#endif /* NDIR */
#endif /* not (DIRENT or _POSIX_VERSION) */

#include "statdefs.h"
#include "procstream.h"
#include "tree-const.h"
#include "oct-obj.h"
#include "octave.h"
#include "dirfns.h"
#include "pager.h"
#include "error.h"
#include "utils.h"
#include "defun.h"

extern "C"
{
#include <readline/tilde.h>
extern char *strerror (int);
}

#ifndef MAXPATHLEN
#define MAXPATHLEN 1024
#endif

// Temp storage for a path.
static char tdir[MAXPATHLEN];

// Non-zero means follow symbolic links that point to directories just
// as if they are real directories.
static int follow_symbolic_links = 1;

// Non-zero means that pwd always give verbatim directory, regardless
// of symbolic link following.
static int verbatim_pwd = 1;

#ifndef HAVE_TEMPNAM

#ifndef P_tmpdir
#define P_tmpdir "/usr/tmp/"
#endif

// Return nonzero if DIR is an existent directory.

static int
diraccess (const char *dir)
{
  struct stat buf;
  return stat (dir, &buf) == 0 && S_ISDIR (buf.st_mode);
}

// Return nonzero if FILE exists.

static int
exists (const char *file)
{
// We can stat the file even if we can't read its data.
  struct stat st;
  int save = errno;
  if (stat (file, &st) == 0)
    return 1;
  else
    {
// We report that the file exists if stat failed for a reason other
// than nonexistence.  In this case, it may or may not exist, and we
// don't know; but reporting that it does exist will never cause any
// trouble, while reporting that it doesn't exist when it does would
// violate the interface of gen_tempname.
      int exists = errno != ENOENT;
      errno = save;
      return exists;
    }
}

// These are the characters used in temporary filenames.

static const char letters[] =
  "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789";

// Generate a temporary filename and return it (in a static buffer).
// If DIR_SEARCH is nonzero, DIR and PFX are used as described for
// tempnam.  If not, a temporary filename in P_tmpdir with no special
// prefix is generated.  This goes through a cyclic pattern of all
// possible filenames consisting of five decimal digits of the current
// pid and three of the characters in `letters'.  Data for tempnam and
// tmpnam is kept separate, but when tempnam is using P_tmpdir and no
// prefix (i.e, it is identical to tmpnam), the same data is used.
// Each potential filename is tested for an already-existing file of
// the same name, and no name of an existing file will be returned.
// When the cycle reaches its end (12345ZZZ), NULL is returned.

static char *
gen_tempname (const char *dir, const char *pfx, int dir_search,
	      size_t *lenptr)
{
  int saverrno = errno;
  static const char tmpdir[] = P_tmpdir;
  static size_t indices[2];
  size_t *idx;
  static char buf[MAXPATHLEN];
  static pid_t oldpid = (pid_t) 0;
  pid_t pid = getpid ();
  register size_t len, plen, dlen;

  if (dir_search)
    {
      register const char *d = getenv ("TMPDIR");
      if (d != NULL && !diraccess (d))
	d = NULL;
      if (d == NULL && dir != NULL && diraccess (dir))
	d = dir;
      if (d == NULL && diraccess (tmpdir))
	d = tmpdir;
      if (d == NULL && diraccess ("/tmp"))
	d = "/tmp";
      if (d == NULL)
	{
	  errno = ENOENT;
	  return NULL;
	}
      dir = d;
    }
  else
    dir = tmpdir;

  dlen = strlen (dir);

// Remove trailing slashes from the directory name.
  while (dlen > 1 && dir[dlen - 1] == '/')
    --dlen;

  if (pfx != NULL && *pfx != '\0')
    {
      plen = strlen (pfx);
      if (plen > 5)
	plen = 5;
    }
  else
    plen = 0;

  if (dir != tmpdir && !strcmp (dir, tmpdir))
    dir = tmpdir;
  idx = &indices[(plen == 0 && dir == tmpdir) ? 1 : 0];

  if (pid != oldpid)
    {
      oldpid = pid;
      indices[0] = indices[1] = 0;
    }

  len = dlen + 1 + plen + 5 + 3;
  for (; *idx < ((sizeof (letters) - 1) * (sizeof (letters) - 1) *
		 (sizeof (letters) - 1));
       ++*idx)
    {
// Construct a file name and see if it already exists.
//
// We use a single counter in *IDX to cycle each of three character
// positions through each of 62 possible letters.

      if (sizeof (buf) < len)
	return NULL;

      sprintf (buf, "%.*s/%.*s%.5d%c%c%c",
	       (int) dlen, dir, (int) plen, pfx, pid % 100000,
	       letters[*idx % (sizeof (letters) - 1)],
	       letters[(*idx / (sizeof (letters) - 1))
		       % (sizeof (letters) - 1)], 
	       letters[(*idx / ((sizeof (letters) - 1)
				* (sizeof (letters) - 1)))
		       % (sizeof (letters) - 1)]);

      if (strlen (buf) != (int) len)
	return NULL;

      if (exists (buf))
	continue;

// If the file already existed we have continued the loop above, so we
// only get here when we have a winning name to return.

      errno = saverrno;

      if (lenptr != NULL)
	*lenptr = len + 1;

      return buf;
    }

// We got out of the loop because we ran out of combinations to try.
  errno = EEXIST;		// ???
  return NULL;
}

// Generate a unique temporary filename using up to five characters of
// PFX if it is not NULL.  The directory to put this file in is
// searched for as follows: First the environment variable "TMPDIR" is
// checked.  If it contains the name of a writable directory, that
// directory is used.  If not and if DIR is not NULL, that value is
// checked.  If that fails, P_tmpdir is tried and finally "/tmp".  The
// storage for the filename is allocated by `malloc'.

char *
tempnam (const char *dir, const char *pfx)
{
  size_t len;
  register char *s;
  register char *t = gen_tempname (dir, pfx, 1, &len);

  if (t == NULL)
    return NULL;

  s = (char *) malloc (len);
  if (s == NULL)
    return NULL;

  (void) memcpy (s, t, len);
  return s;
}

#endif

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
      directory = getcwd (the_current_working_directory, MAXPATHLEN);
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

      if (chdir (t) < 0)
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
      if (chdir (newdir) < 0)
	return 0;
      else
	return 1;
    }
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

      if (dirname && ! change_to_directory (dirname))
	{
	  error ("%s: %s", dirname, strerror (errno));
	  DELETE_ARGV;
	  return retval;
	}
    }
  else
    {
      if (!home_directory)
	{
	  DELETE_ARGV;
	  return retval;
	}

      if (!change_to_directory (home_directory))
	{
          error ("%s: %s", home_directory, strerror (errno));
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
    ls_buf << tilde_expand (argv[i]) << " ";

  ls_buf << ends;

  char *ls_command = ls_buf.str ();

  iprocstream cmd (ls_command);

  char ch;
  ostrstream output_buf;
  while (cmd.get (ch))
    output_buf.put (ch);

  output_buf << ends;

  maybe_page_output (output_buf);
  
  delete [] ls_command;

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
      directory = getcwd (buffer, MAXPATHLEN);

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

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
