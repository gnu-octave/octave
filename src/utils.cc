// utils.cc                                              -*- C++ -*-
/*

Copyright (C) 1996 John W. Eaton

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
#include "dir-ops.h"
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
get_fcn_file_names (const string& name, int no_suffix)
{
  string_vector retval;

  dir_entry dir (name);

  if (dir)
    {
      string_vector tmp = dir.read ();

      int max_len = tmp.length ();

      retval.resize (max_len);

      int k = 0;
      int i;
      for (i = 0; i < max_len; i++)
	{
	  string entry = tmp[i];

	  int len = entry.length ();

#if defined (WITH_DYNAMIC_LINKING)
	  if ((len > 2
	       && entry[len-2] == '.' && entry[len-1] == 'm')
	      || (len > 4
		  && entry[len-4] == '.' && entry[len-3] == 'o'
		  && entry[len-2] == 'c' && entry[len-1] == 't'))
#else
	  if (len > 2
	      && entry[len-2] == '.' && entry[len-1] == 'm')
#endif
	    {
	      if (no_suffix)
		{
		  if (entry[len-1] == 'm')
		    entry.resize (len-2);
		  else
		    entry.resize (len-4);
		}

	      retval[k++] = entry;
	    }
	}

      retval.resize (i);
    }

  return retval;
}

string_vector
get_fcn_file_names (int no_suffix)
{
  static int num_max = 1024;

  string_vector retval (num_max);

  dir_path p (user_pref.loadpath);

  string_vector dirs = p.all_directories ();

  int len = dirs.length ();

  int k = 0;

  for (int i = 0; i < len; i++)
    {
      string_vector names = get_fcn_file_names (dirs[i], no_suffix);

      int tmp_num = names.length ();

      if (k + tmp_num > num_max)
	{
	  num_max += tmp_num;
	  retval.resize (num_max);
	}

      for (int j = 0; j < tmp_num; j++)
	retval[k++] = names[j++];
    }

  retval.resize (k);

  return retval;
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

// See if the given file is in the path.

string
search_path_for_file (const string& path, const string& name)
{
  dir_path p (path);

  return make_absolute (p.find (name), the_current_working_directory);
}

DEFUN (file_in_path, args, ,
  "file_in_path (PATH, NAME)")
{
  Octave_object retval;

  int argc = args.length () + 1;

  string_vector argv = args.make_argv ("file_in_path");

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

DEFUN (undo_string_escapes, args, ,
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
