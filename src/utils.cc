// utils.cc                                              -*- C++ -*-
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

The 12 functions listed below were adapted from a similar functions
from GNU Bash, the Bourne Again SHell, copyright (C) 1987, 1989, 1991
Free Software Foundation, Inc.

  polite_directory_format  absolute_pathname
  absolute_program         base_pathname
  read_octal               sub_append_string
  decode_prompt_string     pathname_backup
  make_absolute            get_working_directory
  change_to_directory      gethostname

*/

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <sys/types.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <sys/param.h>
#include <setjmp.h>
#include <string.h>
#include <limits.h>
#include <iostream.h>
#include <strstream.h>
#include <fstream.h>

#include <Complex.h>

#ifndef HAVE_STRNCASECMP
extern "C"
{
extern int strncasecmp (const char*, const char*, size_t);
}
#endif

extern "C"
{
#if defined (HAVE_TERMIOS_H)
#include <termios.h>
#elif defined (HAVE_TERMIO_H)
#include <termio.h>
#elif defined (HAVE_SGTTY_H)
#include <sgtty.h>
#else
LOSE! LOSE!
#endif

#include <readline/tilde.h>
}

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

#include "SLStack.h"

#include "procstream.h"
#include "user-prefs.h"
#include "variables.h"
#include "dirfns.h"
#include "error.h"
#include "pager.h"
#include "utils.h"
#include "input.h"
#include "octave.h"
#include "oct-obj.h"
#include "mappers.h"
#include "version.h"
#include "tree-const.h"
#include "unwind-prot.h"
#include "octave-hist.h"

// Top level context (?)
extern jmp_buf toplevel;

// Save a string.

char *
strsave (const char *s)
{
  if (! s)
    return 0;

  int len = strlen (s);
  char *tmp = new char [len+1];
  tmp = strcpy (tmp, s);
  return tmp;
}

// Concatenate two strings.

char *
strconcat (const char *s, const char *t)
{
  int len = strlen (s) + strlen (t);
  char *tmp = new char [len+1];
  strcpy (tmp, s);
  return strcat (tmp, t);
}

// Throw away input until a given character is read.

void
discard_until (istream& stream, char character)
{
  int c;
  for (;;)
    {
      stream >> c;
      if (c == EOF || c == character)
	break;
    }
  if (c != EOF)
    stream.putback ((char) c);
} 

char **
pathstring_to_vector (char *pathstring)
{
  static char **path = 0;

  if (pathstring)
    {
      int nelem = 0;
      char *tmp_path = strsave (pathstring);
      if (*tmp_path != '\0')
	{
	  nelem++;
	  char *ptr = tmp_path;
	  while (*ptr != '\0')
	    {
	      if (*ptr == ':')
		nelem++;
	      ptr++;
	    }
	}

      char **foo = path;
      while (foo && *foo)
	delete [] *foo++;
      delete [] path;

      path = new char * [nelem+1];
      path[nelem] = 0;

      int i = 0;
      char *ptr = tmp_path;
      while (i < nelem)
	{
	  char *end = strchr (ptr, ':');
	  if (end)
	    *end = '\0';
	  char *result = tilde_expand (ptr);
	  path[i] = strsave (result);
	  free (result);
	  ptr = end + 1;
	  i++;
	}

      delete [] tmp_path;
    }

  return path;
}

// Return to the main command loop in octave.cc.

void
jump_to_top_level (void)
{
  run_all_unwind_protects ();

  longjmp (toplevel, 1);
}

int
almost_match (const char *std, const char *s, int min_match_len,
	      int case_sens)
{
  int stdlen = strlen (std);
  int slen = strlen (s);

  return (slen <= stdlen
	  && slen >= min_match_len
	  && (case_sens
	      ? (strncmp (std, s, slen) == 0)
	      : (strncasecmp (std, s, slen) == 0)));
}

// Ugh.

int
keyword_almost_match (const char **std, int *min_len, const char *s,
		      int min_toks_to_match, int max_toks)
{
  int status = 0;
  int tok_count = 0;
  int toks_matched = 0;

  if (! s || *s == '\0' || max_toks < 1)
    return status;

  char *kw = strsave (s);

  char *t = kw;
  while (*t != '\0')
    {
      if (*t == '\t')
	*t = ' ';
      t++;
    }

  char *beg = kw;
  while (*beg == ' ')
    beg++;

  if (*beg == '\0')
    return status;


  char **to_match = new char * [max_toks + 1];
  const char **s1 = std;
  char **s2 = to_match;

  if (! s1 || ! s2)
    goto done;

  s2[tok_count] = beg;
  char *end;
  while ((end = strchr (beg, ' ')) != 0)
    {
      *end = '\0';
      beg = end + 1;

      while (*beg == ' ')
	beg++;

      if (*beg == '\0')
	break;

      tok_count++;
      if (tok_count >= max_toks)
	goto done;

      s2[tok_count] = beg;
    }
  s2[tok_count+1] = 0;

  s2 = to_match;

  for (;;)
    {
      if (! almost_match (*s1, *s2, min_len[toks_matched], 0))
	goto done;

      toks_matched++;

      s1++;
      s2++;

      if (! *s2)
	{
	  status = (toks_matched >= min_toks_to_match);
	  goto done;
	}

      if (! *s1)
	goto done;
    }

 done:

  delete [] kw;
  delete [] to_match;

  return status;
}

char **
get_fcn_file_names (int& num, const char *dir, int no_suffix)
{
  static int num_max = 256;
  char **retval = new char * [num_max];
  int i = 0;

  DIR *dirp = opendir (dir);
  if (dirp)
    {
      struct dirent *entry;
      while ((entry = readdir (dirp)) != 0)
	{
	  int len = NLENGTH (entry);
	  if (len > 2
	      && entry->d_name[len-2] == '.'
	      && entry->d_name[len-1] == 'm')
	    {
	      retval[i] = strsave (entry->d_name);
	      if (no_suffix)
		retval[i][len-2] = '\0';

	      i++;

	      if (i == num_max - 1)
		{
// Reallocate the array.  Only copy pointers, not the strings they
// point to, then only delete the original array of pointers, and not
// the strings they point to.

		  num_max += 256;
		  char **tmp = new char * [num_max];
		  for (int j = 0; j < i; j++)
		    tmp[j] = retval[j];

		  delete [] retval;

		  retval = tmp;
		}
	    }
	}
      closedir (dirp);
    }

  retval[i] = 0;
  num = i;

  return retval;
}

char **
get_fcn_file_names (int& num, int no_suffix)
{
  static int num_max = 1024;
  char **retval = new char * [num_max];
  int i = 0;

  char **path = pathstring_to_vector (user_pref.loadpath);

  char **ptr = path;
  if (ptr)
    {
      while (*ptr)
	{
	  int tmp_num;
	  char **names = get_fcn_file_names (tmp_num, *ptr, no_suffix);

	  if (i + tmp_num >= num_max - 1)
	    {
// Reallocate the array.  Only copy pointers, not the strings they
// point to, then only delete the original array of pointers, and not
// the strings they point to.

	      num_max += 1024;
	      char **tmp = new char * [num_max];
	      for (int j = 0; j < i; j++)
		tmp[j] = retval[j];

	      delete [] retval;

	      retval = tmp;
	    }

	  int k = 0;
	  while (k < tmp_num)
	    retval[i++] = names[k++];

	  ptr++;
	}
    }

  retval[i] = 0;
  num = i;

  return retval;
}

int
NINT (double x)
{
  if (x > INT_MAX)
    return INT_MAX;
  else if (x < INT_MIN)
    return INT_MIN;
  else
    return (x > 0) ? ((int) (x + 0.5)) : ((int) (x - 0.5));
}

double
D_NINT (double x)
{
  if (xisinf (x) || xisnan (x))
    return x;
  else
    return floor (x + 0.5);
}

// XXX FIXME XXX --  put these in some file, and make them extern.

static int
all_strings (const Octave_object& args)
{
  int n = args.length ();
  for (int i = 1; i < n; i++)
    if (! args(i).is_string_type ())
      return 0;
  return 1;
}

char **
make_argv (const Octave_object& args, const char *fcn_name)
{
  char **argv = 0;
  if (all_strings (args))
    {
      int n = args.length ();
      argv = new char * [n + 1];
      argv[0] = strsave (fcn_name);
      for (int i = 1; i < n; i++)
	argv[i] = strsave (args(i).string_value ());
    }
  else
    error ("%s: expecting all arguments to be strings", fcn_name);

  return argv;
}

// Format a list in neat columns.  Mostly stolen from GNU ls.  This
// should maybe be in utils.cc.

ostrstream&
list_in_columns (ostrstream& os, char **list)
{
// Compute the maximum name length.

  int max_name_length = 0;
  int total_names = 0;
  for (char **names = list; *names; names++)
    {
      total_names++;
      int name_length = strlen (*names);
      if (name_length > max_name_length)
	max_name_length = name_length;
    }

// Allow at least two spaces between names.

  max_name_length += 2;

// Calculate the maximum number of columns that will fit.

  int line_length = terminal_columns ();
  int cols = line_length / max_name_length;
  if (cols == 0)
    cols = 1;

// Calculate the number of rows that will be in each column except
// possibly  for a short column on the right.

  int rows = total_names / cols + (total_names % cols != 0);

// Recalculate columns based on rows.

  cols = total_names / rows + (total_names % rows != 0);

  names = list;
  int count;
  for (int row = 0; row < rows; row++)
    {
      count = row;
      int pos = 0;

// Print the next row.

      while (1)
	{
	  os << *(names + count);
	  int name_length = strlen (*(names + count));

	  count += rows;
	  if (count >= total_names)
	    break;

	  int spaces_to_pad = max_name_length - name_length;
	  for (int i = 0; i < spaces_to_pad; i++)
	    os << " ";
	  pos += max_name_length;
	}
      os << "\n";
    }

  return os;
}

// See if the given file is in the path.

char *
file_in_path (const char *name, const char *suffix)
{
  char *retval = 0;

  char *nm = strconcat ("/", name);
  if (suffix)
    {
      char *tmp = nm;
      nm = strconcat (tmp, suffix);
      delete [] tmp;
    }

  if (! the_current_working_directory)
    get_working_directory ("file_in_path");

  char **path = pathstring_to_vector (user_pref.loadpath);

  char **ptr = path;
  if (ptr)
    {
      while (*ptr)
	{
	  char *tmp = strconcat (*ptr, nm);
	  char *p = make_absolute (tmp, the_current_working_directory);
	  delete [] tmp;
	  ifstream in_file (p);
	  if (in_file)
	    {
	      in_file.close ();
	      retval = p;
	      goto done;
	    }
	  delete [] p;
	  ptr++;
	}
    }

 done:
  delete [] nm;
  return retval;
}

// See if there is an function file in the path.  If so, return the
// full path to the file.

char *
fcn_file_in_path (const char *name)
{
  return file_in_path (name, ".m");
}

// See if there is an octave file in the path.  If so, return the
// full path to the file.

char *
oct_file_in_path (const char *name)
{
  return file_in_path (name, ".oct");
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
