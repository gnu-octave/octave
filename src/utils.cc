// utils.cc                                              -*- C++ -*-
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
Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

*/

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <sys/types.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <sys/param.h>
#include <string.h>
#include <limits.h>
#include <iostream.h>
#include <strstream.h>
#include <fstream.h>

#include <Complex.h>

extern "C"
{
#include <setjmp.h>

#ifndef HAVE_STRNCASECMP
extern int strncasecmp (const char*, const char*, size_t);
#endif

#define boolean kpathsea_boolean
#define false kpathsea_false
#define true kpathsea_true
#include <kpathsea/pathsearch.h>

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
#if defined (HAVE_DIRENT_H) || defined (_POSIX_VERSION)
#include <dirent.h>
#define NLENGTH(dirent) (strlen((dirent)->d_name))
#else
#define dirent direct
#define NLENGTH(dirent) ((dirent)->d_namlen)
#if defined (HAVE_SYS_NDIR_H)
#include <sys/ndir.h>
#endif
#if defined (HAVE_SYS_DIR_H)
#include <sys/dir.h>
#endif
#if defined (HAVE_NDIR_H)
#include <ndir.h>
#endif
#endif

#include "SLStack.h"

#include "procstream.h"
#include "user-prefs.h"
#include "variables.h"
#include "dirfns.h"
#include "defun.h"
#include "error.h"
#include "help.h"
#include "gripes.h"
#include "pager.h"
#include "utils.h"
#include "input.h"
#include "octave.h"
#include "oct-obj.h"
#include "mappers.h"
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

#if 0

// XXX UNTESTED XXX

// Read input until a given character is read.  Returns characters
// read in a new string that must be freed by the caller.

char *
read_until (istream& stream, char character)
{
  int grow_size = 8;
  int limit = grow_size;
  char *buf = new char [limit];
  char *bp = buf;

 get_more:
  is.getline (bp, limit, character);

  if (is.gcount () == 0)
    {
      delete [] buf;
      return 0;
    }

  if (is.gcount () == limit && buf[limit-1] != '\0')
    {
      char *tmp = new char [limit + grow_size];
      strcpy (tmp, buf);
      delete [] buf;
      buf = tmp;
      bp = tmp + limit - 1;
      limit += grow_size;
      grow_size *= 2;
      goto get_more;
    }

  return buf;
}
#endif

// Get a temporary file name.

char *
octave_tmp_file_name (void)
{
  static char *retval = 0;

  if (retval)
    free (retval);

  retval = tempnam (0, "oct-");

  if (! retval)
    error ("can't open temporary file!");

  return retval;
}

DEFUN ("octave_tmp_file_name", Foctave_tmp_file_name,
       Soctave_tmp_file_name, 0, 1,
 "octave_tmp_file_name ()")
{
  tree_constant retval;

  if (args.length () == 0)
    retval = octave_tmp_file_name ();
  else
    print_usage ("octave_tmp_file_name");

  return retval;
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
#ifdef WITH_DLD
	  if ((len > 2
	       && entry->d_name[len-2] == '.'
	       && entry->d_name[len-1] == 'm')
	      || (len > 4
		  && entry->d_name[len-4] == '.'
		  && entry->d_name[len-3] == 'o'
		  && entry->d_name[len-2] == 'c'
		  && entry->d_name[len-1] == 't'))
#else
	  if (len > 2
	      && entry->d_name[len-2] == '.'
	      && entry->d_name[len-1] == 'm')
#endif
	    {
	      retval[i] = strsave (entry->d_name);
	      if (no_suffix)
		{
		  if (retval[i][len-1] == 'm')
		    retval[i][len-2] = '\0';
		  else
		    retval[i][len-4] = '\0';
		}

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

  char *path_elt = kpse_path_element (user_pref.loadpath);

  while (path_elt)
    {
      str_llist_type *elt_dirs = kpse_element_dirs (path_elt);

      str_llist_elt_type *dir;
      for (dir = *elt_dirs; dir; dir = STR_LLIST_NEXT (*dir))
	{
	  char *elt_dir = STR_LLIST (*dir);

	  if (elt_dir)
	    {
	      int tmp_num;
	      char **names = get_fcn_file_names (tmp_num, elt_dir, no_suffix);

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
	    }
	}

      path_elt = kpse_path_element (0);
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
  for (int i = 0; i < n; i++)
    if (! args(i).is_string ())
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
      for (int i = 0; i < n; i++)
	argv[i+1] = strsave (args(i).string_value ());
    }
  else
    error ("%s: expecting all arguments to be strings", fcn_name);

  return argv;
}

// Return non-zero if either NR or NC is zero.  Return -1 if this
// should be considered fatal; return 1 if this is ok.

int
empty_arg (const char *name, int nr, int nc)
{
  int is_empty = 0;

  if (nr == 0 || nc == 0)
    {
      int flag = user_pref.propagate_empty_matrices;

      if (flag < 0)
	{
	  gripe_empty_arg (name, 0);
	  is_empty = 1;
	}
      else if (flag == 0)
	{
	  gripe_empty_arg (name, 1);
	  is_empty = -1;
	}
      else
	is_empty = 1;
    }

  return is_empty;
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
search_path_for_file (const char *path, const char *name)
{
  char *retval = 0;

  char *tmp = kpse_path_search (path, name, kpathsea_true);

  if (tmp)
    {
      retval = make_absolute (tmp, the_current_working_directory);
      free (tmp);
    }

  return retval;
}

DEFUN ("file_in_path", Ffile_in_path, Sfile_in_path, 3, 1,
  "file_in_path (PATH, NAME)")
{
  Octave_object retval;

  DEFINE_ARGV("file_in_path");

  if (argc == 3)
    {
      char *fname = search_path_for_file (argv[1], argv[2]);

      if (fname)
	retval = fname;
      else
	retval = Matrix ();
    }
  else
    print_usage ("file_in_path");

  DELETE_ARGV;

  return retval;
}


char *
file_in_path (const char *name, const char *suffix)
{
  char *retval = 0;

  char *nm = strsave (name);

  if (suffix)
    {
      char *tmp = nm;
      nm = strconcat (tmp, suffix);
      delete [] tmp;
    }

  if (! the_current_working_directory)
    get_working_directory ("file_in_path");

  retval = search_path_for_file (user_pref.loadpath, nm);

  delete [] nm;

  return retval;
}

// See if there is an function file in the path.  If so, return the
// full path to the file.

char *
fcn_file_in_path (const char *name)
{
  if (name)
    {
      int len = strlen (name);

      if (name [len - 2] == '.' && name [len - 1] == 'm')
	return file_in_path (name, "");
      else
	return file_in_path (name, ".m");
    }
  else
    return 0;
}

// See if there is an octave file in the path.  If so, return the
// full path to the file.

char *
oct_file_in_path (const char *name)
{
  if (name)
    {
      int len = strlen (name);

      if (name [len - 4] == '.' && name [len - 3] == 'o'
	  && name [len - 2] == 'c' && name [len - 1] == 't')
	return file_in_path (name, "");
      else
	return file_in_path (name, ".oct");
    }
  else
    return 0;
}

char *
undo_string_escape (char c)
{
  static char retval[2];
  retval[1] = '\0';

  if (! c)
    return 0;

  switch (c)
    {
    case '\a':
      return "\\a";

    case '\b': // backspace
      return "\\b";

    case '\f': // formfeed
      return "\\f";

    case '\n': // newline
      return "\\n";

    case '\r': // carriage return
      return "\\r";

    case '\t': // horizontal tab
      return "\\t";

    case '\v': // vertical tab
      return "\\v";

    case '\\': // backslash
      return "\\\\";

    case '"': // double quote
      return "\\\"";

    default:
      retval[0] = c;
      return retval;
    }
}

char *
undo_string_escapes (char *s)
{
  ostrstream buf;

  char *t;
  while (t = undo_string_escape (*s++))
    buf << t;
  buf << ends;

  return buf.str ();
}

DEFUN ("undo_string_escapes", Fundo_string_escapes,
       Sundo_string_escapes, 1, 1,
  "undo_string_escapes (STRING)")
{
  tree_constant retval;

  int nargin = args.length ();

  if (nargin == 1 && args(0).is_string ())
    {
      char *str = undo_string_escapes (args(0).string_value ());
      retval = str;
      delete [] str;
    }
  else
    print_usage ("undo_string_escapaes");

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
