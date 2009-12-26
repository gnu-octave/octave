/*

Copyright (C) 1996, 1997, 2000, 2001, 2002, 2003, 2004, 2005, 2006,
              2007, 2008 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cerrno>
#include <cstdio>
#include <cstdlib>
#include <cstring>

#include <iostream>
#include <vector>

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "dir-ops.h"
#include "file-ops.h"
#include "file-stat.h"
#include "oct-env.h"
#include "oct-passwd.h"
#include "pathlen.h"
#include "quit.h"
#include "statdefs.h"
#include "str-vec.h"
#include "oct-locbuf.h"

file_ops::static_members *file_ops::static_members::instance = 0;

file_ops::static_members::static_members (void)
  :
#if (defined (OCTAVE_HAVE_WINDOWS_FILESYSTEM) && ! defined (OCTAVE_HAVE_POSIX_FILESYSTEM))
  xdir_sep_char ('\\'),
  xdir_sep_str ("\\"),
#else
  xdir_sep_char ('/'),
  xdir_sep_str ("/"), 
#endif
#if defined (OCTAVE_HAVE_WINDOWS_FILESYSTEM)
  xdir_sep_chars ("/\\")
#else
  xdir_sep_chars (xdir_sep_str)
#endif
{ }

bool
file_ops::static_members::instance_ok (void)
{
  bool retval = true;

  if (! instance)
    instance = new static_members ();

  if (! instance)
    {
      (*current_liboctave_error_handler)
	("unable to create file_ops::static_members object!");

      retval = false;
    }

  return retval;
}

#define NOT_SUPPORTED(nm) \
  nm ": not supported on this system"

// We provide a replacement for mkdir().

int
file_ops::mkdir (const std::string& name, mode_t mode)
{
  std::string msg;
  return mkdir (name, mode, msg);
}

int
file_ops::mkdir (const std::string& name, mode_t mode, std::string& msg)
{
  msg = std::string ();

  int status = -1;

  status = ::mkdir (name.c_str (), mode);

  if (status < 0)
    {
      using namespace std;
      msg = ::strerror (errno);
    }

  return status;
}

// I don't know how to emulate this on systems that don't provide it.

int
file_ops::mkfifo (const std::string& name, mode_t mode)
{
  std::string msg;
  return mkfifo (name, mode, msg);
}

int
file_ops::mkfifo (const std::string& name, mode_t mode, std::string& msg)
{
  msg = std::string ();

  int status = -1;

  // With gnulib we will always have mkfifo, but some systems like MinGW
  // don't have working mkfifo functions.  On those systems, mkfifo will
  // always return -1 and set errno.

  status = ::mkfifo (name.c_str (), mode);

  if (status < 0)
    {
      using namespace std;
      msg = ::strerror (errno);
    }

  return status;
}

// I don't know how to emulate this on systems that don't provide it.

int
file_ops::link (const std::string& old_name, const std::string& new_name)
{
  std::string msg;
  return link (old_name, new_name, msg);
}

int
file_ops::link (const std::string& old_name,
		const std::string& new_name, std::string& msg)
{
  msg = std::string ();

  int status = -1;

#if defined (HAVE_LINK)
  status = ::link (old_name.c_str (), new_name.c_str ());

  if (status < 0)
    {
      using namespace std;
      msg = ::strerror (errno);
    }
#else
  msg = NOT_SUPPORTED ("link");
#endif

  return status;
}

// I don't know how to emulate this on systems that don't provide it.

int
file_ops::symlink (const std::string& old_name, const std::string& new_name)
{
  std::string msg;
  return symlink (old_name, new_name, msg);
}

int
file_ops::symlink (const std::string& old_name,
		   const std::string& new_name, std::string& msg)
{
  msg = std::string ();

  int status = -1;

#if defined (HAVE_SYMLINK)

  status = ::symlink (old_name.c_str (), new_name.c_str ());

  if (status < 0)
    {
      using namespace std;
      msg = ::strerror (errno);
    }
#else
  msg = NOT_SUPPORTED ("symlink");
#endif

  return status;
}

// We provide a replacement for rename().

int
file_ops::readlink (const std::string& path, std::string& result)
{
  std::string msg;
  return readlink (path, result, msg);
}

int
file_ops::readlink (const std::string& path, std::string& result,
		    std::string& msg)
{
  int status = -1;

  msg = std::string ();

#if defined (HAVE_READLINK)
  char buf[MAXPATHLEN+1];

  status = ::readlink (path.c_str (), buf, MAXPATHLEN);

  if (status < 0)
    {
      using namespace std;
      msg = ::strerror (errno);
    }
  else
    {
      buf[status] = '\0';
      result = std::string (buf);
      status = 0;
    }
#else
  msg = NOT_SUPPORTED ("rename");
#endif

  return status;
}

// We provide a replacement for rename().

int
file_ops::rename (const std::string& from, const std::string& to)
{
  std::string msg;
  return rename (from, to, msg);
}

int
file_ops::rename (const std::string& from, const std::string& to,
		  std::string& msg)
{
  int status = -1;

  msg = std::string ();

#if defined (HAVE_RENAME)
  status = ::rename (from.c_str (), to.c_str ());

  if (status < 0)
    {
      using namespace std;
      msg = ::strerror (errno);
    }
#else
  msg = NOT_SUPPORTED ("rename");
#endif

  return status;
}

// We provide a replacement for rmdir().

int
file_ops::rmdir (const std::string& name)
{
  std::string msg;
  return rmdir (name, msg);
}

int
file_ops::rmdir (const std::string& name, std::string& msg)
{
  msg = std::string ();

  int status = -1;

#if defined (HAVE_RMDIR)
  status = ::rmdir (name.c_str ());

  if (status < 0)
    {
      using namespace std;
      msg = ::strerror (errno);
    }
#else
  msg = NOT_SUPPORTED ("rmdir");
#endif

  return status;
}

// And a version that works recursively.

int
file_ops::recursive_rmdir (const std::string& name)
{
  std::string msg;
  return recursive_rmdir (name, msg);
}

int
file_ops::recursive_rmdir (const std::string& name, std::string& msg)
{
  msg = std::string ();

  int status = 0;

  dir_entry dir (name);

  if (dir)
    {
      string_vector dirlist = dir.read ();

      for (octave_idx_type i = 0; i < dirlist.length (); i++)
	{
	  OCTAVE_QUIT;

	  std::string nm = dirlist[i];

	  // Skip current directory and parent.
	  if (nm == "." || nm == "..")
	    continue;

	  std::string fullnm = name + file_ops::dir_sep_str () + nm;

	  // Get info about the file.  Don't follow links.
	  file_stat fs (fullnm, false);

	  if (fs)
	    {
	      if (fs.is_dir ())
		{
		  status = recursive_rmdir (fullnm, msg);

		  if (status < 0)
		    break;
		}
	      else
		{
		  status = unlink (fullnm, msg);

		  if (status < 0)
		    break;
		}
	    }
	  else
	    {
	      msg = fs.error ();
	      break;
	    }
	}

      if (status >= 0)
	{
	  dir.close ();
	  status = file_ops::rmdir (name, msg);
	}
    }
  else
    {
      status = -1;

      msg = dir.error ();
    }

  return status;
}

std::string
file_ops::canonicalize_file_name (const std::string& name)
{
  std::string msg;
  return canonicalize_file_name (name, msg);
}

std::string
file_ops::canonicalize_file_name (const std::string& name, std::string& msg)
{
  msg = std::string ();

  std::string retval;

#if defined (HAVE_CANONICALIZE_FILE_NAME)

  char *tmp = ::canonicalize_file_name (name.c_str ());

  if (tmp)
    {
      retval = tmp;
      ::free (tmp);
    }

#elif defined (HAVE_RESOLVEPATH)

#if !defined (errno)
extern int errno;
#endif

#if !defined (__set_errno)
# define __set_errno(Val) errno = (Val)
#endif

  if (name.empty ())
    {
      __set_errno (ENOENT);
      return retval;
    }

  // All known hosts with resolvepath (e.g. Solaris 7) don't turn
  // relative names into absolute ones, so prepend the working
  // directory if the path is not absolute.

  std::string absolute_name
    = octave_env::make_absolute (name, octave_env::getcwd ());

  size_t resolved_size = absolute_name.length ();

  while (true)
    {
      resolved_size = 2 * resolved_size + 1;

      OCTAVE_LOCAL_BUFFER (char, resolved, resolved_size);

      int resolved_len
	= ::resolvepath (absolute_name.c_str (), resolved, resolved_size);

      if (resolved_len < 0)
	break;

      if (resolved_len < resolved_size)
	{
	  retval = resolved;
	  break;
	}
    }

#elif defined (__WIN32__)

  int n = 1024;

  std::string win_path (n, '\0');

  while (true)
    {
      int status = GetFullPathName (name.c_str (), n, &win_path[0], 0);

      if (status == 0)
        break;
      else if (status < n)
        {
          win_path.resize (status);
	  retval = win_path;
	  break;
        }
      else
        {
          n *= 2;
	  win_path.resize (n);
        }
    }

#elif defined (HAVE_REALPATH)

#if !defined (__set_errno)
# define __set_errno(Val) errno = (Val)
#endif

  if (name.empty ())
    {
      __set_errno (ENOENT);
      return retval;
    }

  OCTAVE_LOCAL_BUFFER (char, buf, PATH_MAX);

  if (::realpath (name.c_str (), buf))
    retval = buf;

#else

  // FIXME -- provide replacement here...
  retval = name;

#endif

  if (retval.empty ())
    {
      using namespace std;
      msg = ::strerror (errno);
    }

  return retval;
}

// We provide a replacement for tempnam().

std::string
file_ops::tempnam (const std::string& dir, const std::string& pfx)
{
  std::string msg;
  return tempnam (dir, pfx, msg);
}

std::string
file_ops::tempnam (const std::string& dir, const std::string& pfx,
		   std::string& msg)
{
  msg = std::string ();

  std::string retval;
  
  const char *pdir = dir.empty () ? 0 : dir.c_str ();

  const char *ppfx = pfx.empty () ? 0 : pfx.c_str ();

  char *tmp = ::tempnam (pdir, ppfx);

  if (tmp)
    {
      retval = tmp;

      ::free (tmp);
    }
  else
    {
      using namespace std;
      msg = ::strerror (errno);
    }

  return retval;
}

// The following tilde-expansion code was stolen and adapted from
// readline.

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
tilde_find_prefix (const std::string& s, size_t& len)
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
tilde_find_suffix (const std::string& s)
{
  size_t s_len = s.length ();

  string_vector suffixes = file_ops::tilde_additional_suffixes;

  size_t i = 0;

  for ( ; i < s_len; i++)
    {
      if (file_ops::is_dir_sep (s[i]))
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

static std::string
isolate_tilde_prefix (const std::string& fname)
{
  size_t f_len = fname.length ();

  size_t len = 1;

  while (len < f_len && ! file_ops::is_dir_sep (fname[len]))
    len++;

  return fname.substr (1, len);
}

// Do the work of tilde expansion on FILENAME.  FILENAME starts with a
// tilde.

static std::string
tilde_expand_word (const std::string& filename)
{
  size_t f_len = filename.length ();

  if (f_len == 0 || filename[0] != '~')
    return filename;

  // A leading `~/' or a bare `~' is *always* translated to the value
  // of $HOME or the home directory of the current user, regardless of
  // any preexpansion hook.

  if (f_len == 1 || file_ops::is_dir_sep (filename[1]))
    return octave_env::get_home_directory () + filename.substr (1);

  std::string username = isolate_tilde_prefix (filename);

  size_t user_len = username.length ();

  std::string dirname;

  if (file_ops::tilde_expansion_preexpansion_hook)
    {
      std::string expansion
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
	  std::string expansion
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

std::string
file_ops::tilde_expand (const std::string& name)
{
  if (name.find ('~') == std::string::npos)
    return name;
  else
    {
      std::string result;

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

	  std::string tilde_word = name.substr (pos, fini);

	  pos += fini;

	  std::string expansion = tilde_expand_word (tilde_word);

	  result.append (expansion);
	}

      return result;
    }
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
file_ops::unlink (const std::string& name)
{
  std::string msg;
  return unlink (name, msg);
}

int
file_ops::unlink (const std::string& name, std::string& msg)
{
  msg = std::string ();

  int status = -1;

#if defined (HAVE_UNLINK)
  status = ::unlink (name.c_str ());

  if (status < 0)
    {
      using namespace std;
      msg = ::strerror (errno);
    }
#else
  msg = NOT_SUPPORTED ("unlink");
#endif

  return status;
}

std::string
file_ops::concat (const std::string& dir, const std::string& file)
{
  return dir.empty ()
    ? file
    : (is_dir_sep (dir[dir.length()-1])
       ? dir + file
       : dir + file_ops::dir_sep_char () + file);
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
