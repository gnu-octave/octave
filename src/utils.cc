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

#include <climits>
#include <csetjmp>
#include <cstring>

#include <string>

#include <fstream.h>
#include <iostream.h>
#include <strstream.h>

#ifdef HAVE_UNISTD_H
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
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

#include "dir-ops.h"
#include "file-ops.h"
#include "file-stat.h"
#include "oct-cmplx.h"
#include "oct-env.h"
#include "pathsearch.h"
#include "str-vec.h"

#include <defaults.h>
#include "defun.h"
#include "dirfns.h"
#include "error.h"
#include "gripes.h"
#include "input.h"
#include "oct-hist.h"
#include "oct-obj.h"
#include "pager.h"
#include "sysdep.h"
#include "toplev.h"
#include "unwind-prot.h"
#include "utils.h"
#include "variables.h"

// Top level context (?)
extern jmp_buf toplevel;

// Return to the main command loop in octave.cc.

extern "C" void
jump_to_top_level (void)
{
  unwind_protect::run_all ();

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

// Return non-zero if either NR or NC is zero.  Return -1 if this
// should be considered fatal; return 1 if this is ok.

int
empty_arg (const char *name, int nr, int nc)
{
  int is_empty = 0;

  if (nr == 0 || nc == 0)
    {
      int flag = Vpropagate_empty_matrices;

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
search_path_for_file (const string& path, const string& name,
		      bool do_tilde_expansion)
{
  string tmp_path = do_tilde_expansion ? file_ops::tilde_expand (path) : path;

  dir_path p (tmp_path);

  return octave_env::make_absolute (p.find (name), octave_env::getcwd ());
}

DEFUN (file_in_path, args, ,
  "file_in_path (PATH, NAME)")
{
  octave_value_list retval;

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

  return search_path_for_file (Vload_path, nm, false);
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
  octave_value retval;

  int nargin = args.length ();

  if (nargin == 1 && args(0).is_string ())
    retval = undo_string_escapes (args(0).string_value ());
  else
    print_usage ("undo_string_escapes");

  return retval;
}

static void
warn_old_style_preference (bool val, const string& sval)
{
  warning
    ("preference of \"%s\" is obsolete -- use numeric value of %d instead",
     sval.c_str (), (val ? 1 : 0));
}

// Check the value of a string variable to see if it it's ok to do
// something.
//
//   return of  1 => always ok.
//   return of  0 => never ok.
//   return of -1 => ok, but give me warning (default).

int
check_preference (const string& var)
{
  int pref = -1;

  string val = builtin_string_variable (var);

  if (val.empty ())
    {
      double dval = 0;
      if (builtin_real_scalar_variable (var, dval))
	pref = NINT (dval);
    }
  else
    {
      if (val.compare ("yes", 0, 3) == 0
	  || val.compare ("true", 0, 4) == 0)
	{
	  warn_old_style_preference (true, val);
	  pref = 1;
	}
      else if (val.compare ("never", 0, 5) == 0
	       || val.compare ("no", 0, 2) == 0
	       || val.compare ("false", 0, 5) == 0)
	{
	  warn_old_style_preference (false, val);
	  pref = 0;
	}
    }

  return pref;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
