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
#include <cstdlib>
#include <cstring>

#include <iostream.h>

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "file-ops.h"
#include "oct-env.h"
#include "oct-passwd.h"
#include "statdefs.h"
#include "str-vec.h"

#define NOT_SUPPORTED(nm) \
  nm ## ": not supported on this system"

// We provide a replacement for mkdir().

int
file_ops::mkdir (const string& name, mode_t mode)
{
  string msg;
  return mkdir (name, mode, msg);
}

int
file_ops::mkdir (const string& name, mode_t mode, string& msg)
{
  msg = string ();

  int status = -1;

#if defined (HAVE_MKDIR)
  status = ::mkdir (name.c_str (), mode);

  if (status < 0)
    msg = ::strerror (errno);
#else
  msg = NOT_SUPPORTED ("mkdir");
#endif

  return status;
}

// I don't know how to emulate this on systems that don't provide it.

int
file_ops::mkfifo (const string& name, mode_t mode)
{
  string msg;
  return mkfifo (name, mode, msg);
}

int
file_ops::mkfifo (const string& name, mode_t mode, string& msg)
{
  msg = string ();

  int status = -1;

#if defined (HAVE_MKFIFO)
  status = ::mkfifo (name.c_str (), mode);

  if (status < 0)
    msg = ::strerror (errno);
#else
  msg = NOT_SUPPORTED ("mkfifo");
#endif

  return status;
}

// We provide a replacement for rename().

int
file_ops::rename (const string& from, const string& to)
{
  string msg;
  return rename (from, to, msg);
}

int
file_ops::rename (const string& from, const string& to, string& msg)
{
  int status = -1;

  msg = string ();

#if defined (HAVE_RENAME)
  status = ::rename (from.c_str (), to.c_str ());

  if (status < 0)
    msg = ::strerror (errno);
#else
  msg = NOT_SUPPORTED ("rename");
#endif

  return status;
}

// We provide a replacement for rmdir().

int
file_ops::rmdir (const string& name)
{
  string msg;
  return rmdir (name, msg);
}

int
file_ops::rmdir (const string& name, string& msg)
{
  msg = string ();

  int status = -1;

#if defined (HAVE_RMDIR)
  status = ::rmdir (name.c_str ());

  if (status < 0)
    msg = ::strerror (errno);
#else
  msg = NOT_SUPPORTED ("rmdir");
#endif

  return status;
}

// We provide a replacement for tempnam().

string
file_ops::tempnam (const string& dir, const string& pfx)
{
  string msg;
  return tempnam (dir, pfx, msg);
}

string
file_ops::tempnam (const string& dir, const string& pfx, string& msg)
{
  msg = string ();

  string retval;
  
  const char *pdir = dir.empty () ? 0 : dir.c_str ();

  const char *ppfx = pfx.empty () ? 0 : pfx.c_str ();

  char *tmp = ::tempnam (pdir, ppfx);

  if (tmp)
    {
      retval = tmp;

      ::free (tmp);
    }
  else
    msg = ::strerror (errno);

  return retval;
}

// The following tilde-expansion code was stolen and adapted from
// readline.

// XXX FIXME XXX
#define DIR_SEP_CHAR '/'

// The default value of tilde_additional_prefixes.  This is set to
// whitespace preceding a tilde so that simple programs which do not
// perform any word separation get desired behaviour.
static const char *default_prefixes[] = { " ~", "\t~", ":~", 0 };

// The default value of tilde_additional_suffixes.  This is set to
// whitespace or newline so that simple programs which do not perform
// any word separation get desired behaviour.
static const char *default_suffixes[] = { " ", "\n", ":", 0 };

// If non-null, this contains the address of a function that the
// application wants called before trying the standard tilde
// expansions.  The function is called with the text sans tilde, and
// returns a malloc()'ed string which is the expansion, or a NULL
// pointer if the expansion fails.
file_ops::tilde_expansion_hook file_ops::tilde_expansion_preexpansion_hook = 0;

// If non-null, this contains the address of a function to call if the
// standard meaning for expanding a tilde fails.  The function is
// called with the text (sans tilde, as in "foo"), and returns a
// malloc()'ed string which is the expansion, or a NULL pointer if
// there is no expansion.
file_ops::tilde_expansion_hook file_ops::tilde_expansion_failure_hook = 0;

// When non-null, this is a NULL terminated array of strings which are
// duplicates for a tilde prefix.  Bash uses this to expand `=~' and
// `:~'.
string_vector file_ops::tilde_additional_prefixes = default_prefixes;

// When non-null, this is a NULL terminated array of strings which
// match the end of a username, instead of just "/".  Bash sets this
// to `:' and `=~'.
string_vector file_ops::tilde_additional_suffixes = default_suffixes;

// Find the start of a tilde expansion in S, and return the index
// of the tilde which starts the expansion.  Place the length of the
// text which identified this tilde starter in LEN, excluding the
// tilde itself.

static size_t
tilde_find_prefix (const string& s, size_t& len)
{
  len = 0;

  size_t s_len = s.length ();

  if (s_len == 0 || s[0] == '~')
    return 0;

  string_vector prefixes = file_ops::tilde_additional_prefixes;

  if (! prefixes.empty ())
    {
      for (size_t i = 0; i < s_len; i++)
	{
	  for (int j = 0; j < prefixes.length (); j++)
	    {
	      size_t pfx_len = prefixes[j].length ();

	      if (prefixes[j].compare (s.substr (i, pfx_len)) == 0)
		{
		  len = pfx_len - 1;
		  return i + len;
		}
	    }
	}
    }

  return s_len;
}

// Find the end of a tilde expansion in S, and return the index
// of the character which ends the tilde definition.

static size_t
tilde_find_suffix (const string& s)
{
  size_t s_len = s.length ();

  string_vector suffixes = file_ops::tilde_additional_suffixes;

  size_t i = 0;

  for ( ; i < s_len; i++)
    {
      if (s[i] == DIR_SEP_CHAR)
	break;

      if (! suffixes.empty ())
	{
	  for (int j = 0; j < suffixes.length (); j++)
	    {
	      size_t sfx_len = suffixes[j].length ();

	      if (suffixes[j].compare (s.substr (i, sfx_len)) == 0)
		return i;
	    }
	}
    }

  return i;
}

// Take FNAME and return the tilde prefix we want expanded.

static string
isolate_tilde_prefix (const string& fname)
{
  size_t f_len = fname.length ();

  size_t len = 1;

  while (len < f_len && fname[len] != DIR_SEP_CHAR)
    len++;

  return fname.substr (1, len);
}

// Do the work of tilde expansion on FILENAME.  FILENAME starts with a
// tilde.

static string
tilde_expand_word (const string& filename)
{
  size_t f_len = filename.length ();

  if (f_len == 0 || filename[0] != '~')
    return filename;

  // A leading `~/' or a bare `~' is *always* translated to the value
  // of $HOME or the home directory of the current user, regardless of
  // any preexpansion hook.

  if (f_len == 1 || filename[1] == DIR_SEP_CHAR)
    return octave_env::get_home_directory () + filename.substr (1);

  string username = isolate_tilde_prefix (filename);

  size_t user_len = username.length ();

  string dirname;

  if (file_ops::tilde_expansion_preexpansion_hook)
    {
      string expansion
	= file_ops::tilde_expansion_preexpansion_hook (username);

      if (! expansion.empty ())
	return expansion + filename.substr (user_len+1);
    }

  // No preexpansion hook, or the preexpansion hook failed.  Look in the
  // password database.

  octave_passwd pw = octave_passwd::getpwnam (username);

  if (! pw)
    {
      // If the calling program has a special syntax for expanding tildes,
      // and we couldn't find a standard expansion, then let them try.

      if (file_ops::tilde_expansion_failure_hook)
	{
	  string expansion
	    = file_ops::tilde_expansion_failure_hook (username);

	  if (! expansion.empty ())
	    dirname = expansion + filename.substr (user_len+1);
	}

      // If we don't have a failure hook, or if the failure hook did not
      // expand the tilde, return a copy of what we were passed.

      if (dirname.length () == 0)
	dirname = filename;
    }
  else
    dirname = pw.dir () + filename.substr (user_len+1);

  return dirname;
}

// If NAME has a leading ~ or ~user, Unix-style, expand it to the
// user's home directory.  If no ~, or no <pwd.h>, just return NAME.

string
file_ops::tilde_expand (const string& name)
{
  string result;

  size_t name_len = name.length ();

  // Scan through S expanding tildes as we come to them.

  size_t pos = 0;

  while (1)
    {
      if (pos > name_len)
	break;

      size_t len;

      // Make START point to the tilde which starts the expansion.

      size_t start = tilde_find_prefix (name.substr (pos), len);

      result.append (name.substr (pos, start));

      // Advance STRING to the starting tilde.

      pos += start;

      // Make FINI be the index of one after the last character of the
      // username.

      size_t fini = tilde_find_suffix (name.substr (pos));

      // If both START and FINI are zero, we are all done.

      if (! (start || fini))
	break;

      // Expand the entire tilde word, and copy it into RESULT.

      string tilde_word = name.substr (pos, fini);

      pos += fini;

      string expansion = tilde_expand_word (tilde_word);

      result.append (expansion);
    }

  return result;
}

// A vector version of the above.

string_vector
file_ops::tilde_expand (const string_vector& names)
{
  string_vector retval;

  int n = names.length ();

  retval.resize (n);

  for (int i = 0; i < n; i++)
    retval[i] = file_ops::tilde_expand (names[i]);

  return retval;
}

int
file_ops::umask (mode_t mode)
{
#if defined (HAVE_UMASK)
  return ::umask (mode);
#else
  return 0;
#endif
}

int
file_ops::unlink (const string& name)
{
  string msg;
  return unlink (name, msg);
}

int
file_ops::unlink (const string& name, string& msg)
{
  msg = string ();

  int status = -1;

#if defined (HAVE_UNLINK)
  status = ::unlink (name.c_str ());

  if (status < 0)
    msg = ::strerror (errno);
#else
  msg = NOT_SUPPORTED ("unlink");
#endif

  return status;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
