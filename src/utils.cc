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
Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <climits>
#include <csetjmp>
#include <cstring>

#include <string>

#include <fstream.h>
#include <iostream.h>
#include <strstream.h>

#ifdef HAVE_UNISTD_H
#include <sys/types.h>
#include <unistd.h>
#endif

#if defined (HAVE_TERMIOS_H)
#include <termios.h>
#elif defined (HAVE_TERMIO_H)
#include <termio.h>
#elif defined (HAVE_SGTTY_H)
#include <sgtty.h>
#else
LOSE! LOSE!
#endif

#ifndef HAVE_STRNCASECMP
extern "C" int strncasecmp (const char*, const char*, size_t);
#endif

#include "SLStack.h"

#include "oct-cmplx.h"
#include "str-vec.h"

#include "defun.h"
#include "dirfns.h"
#include "error.h"
#include "gripes.h"
#include "help.h"
#include "input.h"
#include "mappers.h"
#include "oct-hist.h"
#include "oct-obj.h"
#include "pager.h"
#include "pathsearch.h"
#include "sysdep.h"
#include "sysdir.h"
#include "toplev.h"
#include "unwind-prot.h"
#include "user-prefs.h"
#include "utils.h"
#include "variables.h"

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

#if 0

// Concatenate two strings.

char *
strconcat (const char *s, const char *t)
{
  int len = strlen (s) + strlen (t);
  char *tmp = new char [len+1];
  strcpy (tmp, s);
  strcat (tmp, t);
  return tmp;
}
#endif

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

string
octave_tmp_file_name (void)
{
  string retval;

  char *tmp = tempnam (0, "oct-");

  if (tmp)
    {
      retval = tmp;

      free (tmp);
    }
  else
    error ("can't open temporary file!");

  return retval;
}

DEFUN ("octave_tmp_file_name", Foctave_tmp_file_name,
       Soctave_tmp_file_name, 10,
 "octave_tmp_file_name ()")
{
  tree_constant retval;

  if (args.length () == 0)
    retval = octave_tmp_file_name ();
  else
    print_usage ("octave_tmp_file_name");

  return retval;
}

// Return to the main command loop in octave.cc.

extern "C" void
jump_to_top_level (void)
{
  run_all_unwind_protects ();

  longjmp (toplevel, 1);
}

int
almost_match (const string& std, const string& s, int min_match_len,
	      int case_sens)
{
  int stdlen = std.length ();
  int slen = s.length ();

  return (slen <= stdlen
	  && slen >= min_match_len
	  && (case_sens
	      ? (strncmp (std.c_str (), s.c_str (), slen) == 0)
	      : (strncasecmp (std.c_str (), s.c_str (), slen) == 0)));
}

// Ugh.

int
keyword_almost_match (const char **std, int *min_len, const string& s,
		      int min_toks_to_match, int max_toks)
{
  int status = 0;
  int tok_count = 0;
  int toks_matched = 0;

  if (s.empty () || max_toks < 1)
    return status;

  char *kw = strsave (s.c_str ());

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

string_vector
get_fcn_file_names (int& num, const char *dir, int no_suffix)
{
  static int num_max = 256;
  string_vector retval (num_max);
  int i = 0;

  DIR *dirp = opendir (dir);
  if (dirp)
    {
      struct dirent *entry;
      while ((entry = readdir (dirp)) != 0)
	{
	  int len = NLENGTH (entry);
#if defined (WITH_DYNAMIC_LINKING)
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
	      retval[i] = entry->d_name;
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
		  num_max += 256;
		  retval.resize (num_max);
		}
	    }
	}
      closedir (dirp);
    }

  num = i;
  retval.resize (num);

  return retval;
}

string_vector
get_fcn_file_names (int& num, int no_suffix)
{
  static int num_max = 1024;
  string_vector retval (num_max);
  int i = 0;

  char *path_elt = kpse_path_element (user_pref.loadpath.c_str ());

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
	      string_vector names
		= get_fcn_file_names (tmp_num, elt_dir, no_suffix);

	      if (i + tmp_num >= num_max - 1)
		{
		  num_max += 1024;
		  retval.resize (num_max);
		}

	      int k = 0;
	      while (k < tmp_num)
		retval[i++] = names[k++];
	    }
	}

      path_elt = kpse_path_element (0);
    }

  num = i;
  retval.resize (num);

  return retval;
}

// Convert X to the nearest integer value.  Should not pass NaN to
// this function.

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

string_vector
make_argv (const Octave_object& args, const string& fcn_name)
{
  string_vector argv;

  if (all_strings (args))
    {
      int n = args.length ();
      argv.resize (n+1);
      argv[0] = fcn_name;

      for (int i = 0; i < n; i++)
	argv[i+1] = args(i).string_value ();
    }
  else
    error ("%s: expecting all arguments to be strings", fcn_name.c_str ());

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

// Format a list in neat columns.  Mostly stolen from GNU ls.

ostrstream&
list_in_columns (ostrstream& os, const string_vector& list)
{
  // Compute the maximum name length.

  int max_name_length = 0;
  int total_names = list.length ();

  for (int i = 0; i < total_names; i++)
    {
      int name_length = list[i].length ();
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

  int count;
  for (int row = 0; row < rows; row++)
    {
      count = row;
      int pos = 0;

      // Print the next row.

      while (1)
	{
	  string nm = list[count];

	  os << nm;
	  int name_length = nm.length ();

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

string
search_path_for_file (const string& path, const string& name)
{
  string retval;

  char *tmp = kpse_path_search (path.c_str (), name.c_str (),
				kpathsea_true);

  if (tmp)
    {
      retval = make_absolute (tmp, the_current_working_directory);
      free (tmp);
    }

  return retval;
}

DEFUN ("file_in_path", Ffile_in_path, Sfile_in_path, 10,
  "file_in_path (PATH, NAME)")
{
  Octave_object retval;

  int argc = args.length () + 1;

  string_vector argv = make_argv (args, "file_in_path");

  if (error_state)
    return retval;

  if (argc == 3)
    {
      string fname = search_path_for_file (argv[1], argv[2]);

      if (fname.empty ())
	retval = Matrix ();
      else
	retval = fname;
    }
  else
    print_usage ("file_in_path");

  return retval;
}

string
file_in_path (const string& name, const string& suffix)
{
  string nm = name;

  if (! suffix.empty ())
    nm.append (suffix);

  if (the_current_working_directory.empty ())
    get_working_directory ("file_in_path");

  return search_path_for_file (user_pref.loadpath, nm);
}

// See if there is an function file in the path.  If so, return the
// full path to the file.

string
fcn_file_in_path (const string& name)
{
  string retval;

  int len = name.length ();
  
  if (len > 0)
    {
      if (len > 2 && name [len - 2] == '.' && name [len - 1] == 'm')
	retval = file_in_path (name, "");
      else
	retval = file_in_path (name, ".m");
    }

  return retval;
}

// See if there is an octave file in the path.  If so, return the
// full path to the file.

string
oct_file_in_path (const string& name)
{
  string retval;

  int len = name.length ();
  
  if (len > 0)
    {
      if (len > 2 && name [len - 4] == '.' && name [len - 3] == 'o'
	  && name [len - 2] == 'c' && name [len - 1] == 't')
	retval = file_in_path (name, "");
      else
	retval = file_in_path (name, ".oct");
    }

  return retval;
}

const char *
undo_string_escape (char c)
{
  if (! c)
    return "";

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
      {
	static char retval[2];
	retval[0] = c;
	retval[1] = '\0';
	return retval;
      }
    }
}

string
undo_string_escapes (const string& s)
{
  string retval;

  for (size_t i = 0; i < s.length (); i++)
    retval.append (undo_string_escape (s[i]));

  return retval;
}

DEFUN ("undo_string_escapes", Fundo_string_escapes,
       Sundo_string_escapes, 10,
  "undo_string_escapes (STRING)")
{
  tree_constant retval;

  int nargin = args.length ();

  if (nargin == 1 && args(0).is_string ())
    retval = undo_string_escapes (args(0).string_value ());
  else
    print_usage ("undo_string_escapes");

  return retval;
}

// This function was adapted from xputenv from Karl Berry's kpathsearch
// library.

void
oct_putenv (const char *var_name, const char *value)
{
  static const char **saved_env_items = 0;
  static unsigned saved_len;
  char *old_item = 0;

  int new_len = strlen (var_name) + strlen (value) + 2;

  char *new_item = new char [new_len];

  sprintf (new_item, "%s=%s", var_name, value);

#ifndef SMART_PUTENV

  // Check if we have saved anything yet.

  if (! saved_env_items)
    {
      saved_env_items = new const char * [1];
      saved_env_items[0] = var_name;
      saved_len = 1;
    }
  else
    {
      // Check if we've assigned VAR_NAME before.

      unsigned len = strlen (var_name);

      for (unsigned i = 0; i < saved_len && ! old_item; i++)
        {
          if (strcmp (saved_env_items[i], var_name) == 0)
            {
              old_item = getenv (var_name);

              assert (old_item);

              // Back up to the `NAME=' in the environment before the
	      // value that getenv returns.

              old_item -= (len + 1);
            }
        }
      
      if (! old_item)
        {
          // If we haven't seen VAR_NAME before, save it.  Assume it
	  // is in safe storage.

          saved_len++;

	  const char **tmp = new const char * [saved_len];

	  for (unsigned i = 0; i < saved_len - 1; i++)
	    tmp[i] = saved_env_items[i];

	  tmp[saved_len - 1] = var_name;

          delete [] saved_env_items;

	  saved_env_items = tmp;
        }
    }

#endif

  // As far as I can see there's no way to distinguish between the
  // various errors; putenv doesn't have errno values.

  if (putenv (new_item) < 0)
    error ("putenv (%s) failed", new_item);
  
#ifndef SMART_PUTENV

  // Can't free `new_item' because its contained value is now in
  // `environ', but we can free `old_item', since it's been replaced.

  delete [] old_item;

#endif
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
