// This file is not compiled to a separate object file.
// It is included in pathsearch.cc.

/* Look up a filename in a path.

Copyright (C) 2003-2015 John W. Eaton
Copyright (C) 1993, 94, 95, 96, 97, 98 Karl Berry.
Copyright (C) 1993, 94, 95, 96, 97 Karl Berry & O. Weber.
Copyright (C) 1992, 93, 94, 95, 96, 97 Free Software Foundation, Inc.

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

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <cctype>
#include <cerrno>
#include <cstdlib>
#include <ctime>

#include <map>
#include <fstream>
#include <iostream>
#include <string>

#include <sys/types.h>
#include <unistd.h>

// Needed for NAME_MAX.
#include <dirent.h>

#include "file-ops.h"
#include "file-stat.h"
#include "kpse.h"
#include "oct-env.h"
#include "oct-passwd.h"
#include "pathsearch.h"

#if defined (__WIN32__) && ! defined (__CYGWIN__)
#  define WIN32_LEAN_AND_MEAN 1
#  include <windows.h>
#endif

// Define the characters which separate components of filenames and
// environment variable paths.

#define IS_DEVICE_SEP(ch) octave::sys::file_ops::is_dev_sep (ch)
#define NAME_BEGINS_WITH_DEVICE(name) \
   (name.length () > 0 && IS_DEVICE_SEP ((name)[1]))

#define DIR_SEP_STRING octave::sys::file_ops::dir_sep_str ()
#define IS_DIR_SEP(ch) octave::sys::file_ops::is_dir_sep (ch)

#define ENV_SEP octave::directory_path::path_sep_char ()
#define ENV_SEP_STRING octave::directory_path::path_sep_str ()
#define IS_ENV_SEP(ch) octave::directory_path::is_path_sep (ch)

// Define NAME_MAX, the maximum length of a single component in a
// filename.  No such limit may exist, or may vary depending on the
// filesystem.  We don't use this value to create a character string, we
// only truncate given file names to this length if attempts to get info
// about the file fail with errno == ENAMETOOLONG.

// Most likely the system will truncate filenames if it is not POSIX,
// and so we can use the BSD value here.

#if ! defined (_POSIX_NAME_MAX)
#  define _POSIX_NAME_MAX 255
#endif

#if ! defined (NAME_MAX)
#  define NAME_MAX _POSIX_NAME_MAX
#endif

// If NO_DEBUG is defined (not recommended), skip all this.
#if ! defined (NO_DEBUG)

// OK, we'll have tracing support.
#  define KPSE_DEBUG

// Test if a bit is on.
#  define KPSE_DEBUG_P(bit) (kpse_debug & (1 << (bit)))

#  define KPSE_DEBUG_STAT 0               // stat calls
#  define KPSE_DEBUG_EXPAND 1             // path element expansion
#  define KPSE_DEBUG_SEARCH 2             // searches
#  define KPSE_DEBUG_VARS 3               // variable values
#  define KPSE_LAST_DEBUG KPSE_DEBUG_VARS

#endif

unsigned int kpse_debug = 0;

static std::string kpse_var_expand (const std::string& src);

static std::string kpse_expand (const std::string& s);

void
kpse_path_iterator::set_end (void)
{
  e = b + 1;

  if (e == len)
    ; // OK, we have found the last element.
  else if (e > len)
    b = e = std::string::npos;
  else
    {
      // Find the next colon not enclosed by braces (or the end of the
      // path).

      while (e < len && ! octave::directory_path::is_path_sep (path[e]))
        e++;
    }
}

void
kpse_path_iterator::next (void)
{
  b = e + 1;

  // Skip any consecutive colons.
  while (b < len && octave::directory_path::is_path_sep (path[b]))
    b++;

  if (b >= len)
    b = e = std::string::npos;
  else
    set_end ();
}

/* Truncate any too-long components in NAME, returning the result.  It's
   too bad this is necessary.  See comments in readable.c for why.  */

static std::string
kpse_truncate_filename (const std::string& name)
{
  unsigned c_len = 0;        /* Length of current component.  */
  unsigned ret_len = 0;      /* Length of constructed result.  */

  std::string ret = name;

  size_t len = name.length ();

  for (size_t i = 0; i < len; i++)
    {
      if (IS_DIR_SEP (name[i]) || IS_DEVICE_SEP (name[i]))
        {
          /* At a directory delimiter, reset component length.  */
          c_len = 0;
        }
      else if (c_len > NAME_MAX)
        {
          /* If past the max for a component, ignore this character.  */
          continue;
        }

      /* Copy this character.  */
      ret[ret_len++] = name[i];
      c_len++;
    }

  ret.resize (ret_len);

  return ret;
}

/* If access can read FN, run stat (assigning to stat buffer ST) and
   check that fn is not a directory.  Don't check for just being a
   regular file, as it is potentially useful to read fifo's or some
   kinds of devices.  */

#if defined (__WIN32__)
static inline bool
READABLE (const std::string& fn)
{
  const char *t = fn.c_str ();
  return (GetFileAttributes (t) != 0xFFFFFFFF
          && ! (GetFileAttributes (t) & FILE_ATTRIBUTE_DIRECTORY));
}
#else
static inline bool
READABLE (const std::string& fn)
{
  bool retval = false;

  const char *t = fn.c_str ();

  if (access (t, R_OK) == 0)
    {
      octave::sys::file_stat fs (fn);

      retval = fs && ! fs.is_dir ();
    }

  return retval;
}
#endif

/* POSIX invented the brain-damage of not necessarily truncating
   filename components; the system's behavior is defined by the value of
   the symbol _POSIX_NO_TRUNC, but you can't change it dynamically!

   Generic const return warning.  See extend-fname.c.  */

static std::string
kpse_readable_file (const std::string& name)
{
  std::string ret;

  if (READABLE (name))
    {
      ret = name;

#if defined (ENAMETOOLONG)
    }
  else if (errno == ENAMETOOLONG)
    {
      ret = kpse_truncate_filename (name);

      /* Perhaps some other error will occur with the truncated name,
         so let's call access again.  */

      if (! READABLE (ret))
        {
          /* Failed.  */
          ret = "";
        }
#endif /* ENAMETOOLONG */

    }
  else
    {
      /* Some other error.  */
      if (errno == EACCES)
        {
          /* Maybe warn them if permissions are bad.  */
          perror (name.c_str ());
        }

      ret = "";
    }

  return ret;
}

static bool
kpse_absolute_p (const std::string& filename, int relative_ok)
{
  return (octave::sys::env::absolute_pathname (filename)
          || (relative_ok
              && octave::sys::env::rooted_relative_pathname (filename)));
}

/* The very first search is for texmf.cnf, called when someone tries to
   initialize the TFM path or whatever.  init_path calls kpse_cnf_get
   which calls kpse_all_path_search to find all the texmf.cnf's.  We
   need to do various special things in this case, since we obviously
   don't yet have the configuration files when we're searching for the
   configuration files.  */
static bool first_search = true;

/* This function is called after every search.  */

static void
log_search (const std::list<std::string>& filenames)
{
  if (KPSE_DEBUG_P (KPSE_DEBUG_SEARCH))
    {
      for (const auto &filename : filenames)
        std::cerr << time (0) << " " << filename << std::endl;
    }
}

/* Concatenate each element in DIRS with NAME (assume each ends with a
   /, to save time).  If SEARCH_ALL is false, return the first readable
   regular file.  Else continue to search for more.  In any case, if
   none, return a list containing just NULL.

   We keep a single buffer for the potential filenames and reallocate
   only when necessary.  I'm not sure it's noticeably faster, but it
   does seem cleaner.  (We do waste a bit of space in the return
   value, though, since we don't shrink it to the final size returned.)  */

static std::list<std::string>
dir_search (const std::string& dir, const std::string& name,
            bool search_all)
{
  std::list<std::string> ret;

  std::string potential = dir + name;

  std::string tmp = kpse_readable_file (potential);

  if (! tmp.empty ())
    {
      ret.push_back (potential);

      if (! search_all)
        return ret;
    }

  return ret;
}

/* This is called when NAME is absolute or explicitly relative; if it's
   readable, return (a list containing) it; otherwise, return NULL.  */

static std::list<std::string>
absolute_search (const std::string& name)
{
  std::list<std::string> ret_list;
  std::string found = kpse_readable_file (name);

  /* Add 'found' to the return list even if it's null; that tells
     the caller we didn't find anything.  */
  ret_list.push_back (found);

  return ret_list;
}

/* This is the hard case -- look for NAME in PATH.  If ALL is false,
   return the first file found.  Otherwise, search all elements of PATH.  */

static std::list<std::string>
path_search (const std::string& path, const std::string& name,
             bool /* must_exist */, bool all)
{
  std::list<std::string> ret_list;
  bool done = false;

  for (kpse_path_iterator pi (path); ! done && pi != std::string::npos; pi++)
    {
      std::string elt = *pi;

      std::list<std::string> found;

      /* Do not touch the device if present */
      if (NAME_BEGINS_WITH_DEVICE (elt))
        {
          while (elt.length () > 3
                 && IS_DIR_SEP (elt[2]) && IS_DIR_SEP (elt[3]))
            {
              elt[2] = elt[1];
              elt[1] = elt[0];
              elt = elt.substr (1);
            }
        }
      else
        {
          /* We never want to search the whole disk.  */
          while (elt.length () > 1
                 && IS_DIR_SEP (elt[0]) && IS_DIR_SEP (elt[1]))
            elt = elt.substr (1);
        }

      /* Our caller (search), also tests first_search, and does
         the resetting.  */
      if (first_search)
        found = std::list<std::string> ();

      /* Search the filesystem.  */

      if (found.empty ())
        {
          std::string dir = kpse_element_dir (elt);

          if (! dir.empty ())
            found = dir_search (dir, name, all);
        }

      /* Did we find anything anywhere?  */
      if (! found.empty ())
        {
          if (all)
            ret_list.splice (ret_list.end (), found);
          else
            {
              ret_list.push_back (found.front ());
              done = true;
            }
        }
    }

  return ret_list;
}

/* Search PATH for ORIGINAL_NAME.  If ALL is false, or ORIGINAL_NAME is
   absolute_p, check ORIGINAL_NAME itself.  Otherwise, look at each
   element of PATH for the first readable ORIGINAL_NAME.

   Always return a list; if no files are found, the list will
   contain just NULL.  If ALL is true, the list will be
   terminated with NULL.  */

static std::list<std::string>
search (const std::string& path, const std::string& original_name,
        bool must_exist, bool all)
{
  std::list<std::string> ret_list;
  bool absolute_p;

  /* Make a leading ~ count as an absolute filename, and expand $FOO's.  */
  std::string name = kpse_expand (original_name);

  /* If the first name is absolute or explicitly relative, no need to
     consider PATH at all.  */
  absolute_p = kpse_absolute_p (name, true);

  if (KPSE_DEBUG_P (KPSE_DEBUG_SEARCH))
    std::cerr << "kdebug: start search (file=" << name
              << ", must_exist=" << must_exist
              << ", find_all=" << all << ", path=" << path << ")."
              << std::endl;

  /* Find the file(s). */
  ret_list = absolute_p ? absolute_search (name)
                        : path_search (path, name, must_exist, all);

  /* The very first search is for texmf.cnf.  We can't log that, since
     we want to allow setting TEXMFLOG in texmf.cnf.  */
  if (first_search)
    {
      first_search = false;
    }
  else
    {
      /* Record the filenames we found, if desired.  And wrap them in a
         debugging line if we're doing that.  */

      if (KPSE_DEBUG_P (KPSE_DEBUG_SEARCH))
        std::cerr << "kdebug: search (" << original_name << ") =>";

      log_search (ret_list);

      if (KPSE_DEBUG_P (KPSE_DEBUG_SEARCH))
        std::cerr << std::endl;
    }

  return ret_list;
}

/* Search PATH for the first NAME.  */

/* Call 'kpse_expand' on NAME.  If the result is an absolute or
   explicitly relative filename, check whether it is a readable
   (regular) file.

   Otherwise, look in each of the directories specified in PATH (also do
   tilde and variable expansion on elements in PATH).

   The caller must expand PATH.  This is because it makes more sense to
   do this once, in advance, instead of for every search using it.

   In any case, return the complete filename if found, otherwise NULL.  */

std::string
kpse_path_search (const std::string& path, const std::string& name,
                  bool must_exist)
{
  std::list<std::string> ret_list = search (path, name, must_exist, false);

  return ret_list.empty () ? "" : ret_list.front ();
}

/* Like 'kpse_path_search' with MUST_EXIST true, but return a list of
   all the filenames (or NULL if none), instead of taking the first.  */

std::list<std::string>
kpse_all_path_search (const std::string& path, const std::string& name)
{
  return search (path, name, true, true);
}

/* This is the hard case -- look in each element of PATH for each
   element of NAMES.  If ALL is false, return the first file found.
   Otherwise, search all elements of PATH.  */

std::list<std::string>
path_find_first_of (const std::string& path,
                    const std::list<std::string>& names,
                    bool /* must_exist */, bool all)
{
  std::list<std::string> ret_list;
  bool done = false;

  for (kpse_path_iterator pi (path); ! done && pi != std::string::npos; pi++)
    {
      std::string elt = *pi;

      std::string dir;
      std::list<std::string> found;

      /* Do not touch the device if present */

      if (NAME_BEGINS_WITH_DEVICE (elt))
        {
          while (elt.length () > 3
                 && IS_DIR_SEP (elt[2]) && IS_DIR_SEP (elt[3]))
            {
              elt[2] = elt[1];
              elt[1] = elt[0];
              elt = elt.substr (1);
            }
        }
      else
        {
          /* We never want to search the whole disk.  */
          while (elt.length () > 1
                 && IS_DIR_SEP (elt[0]) && IS_DIR_SEP (elt[1]))
            elt = elt.substr (1);
        }

      /* We have to search one directory at a time.  */
      dir = kpse_element_dir (elt);

      if (! dir.empty ())
        {
          for (auto it = names.cbegin (); it != names.cend () && ! done; it++)
            {
              std::string name = *it;

              /* Our caller (find_first_of), also tests first_search,
                 and does the resetting.  */
              if (first_search)
                found = std::list<std::string> ();

              /* Search the filesystem.  */

              if (found.empty ())
                found = dir_search (dir, name, all);

              /* Did we find anything anywhere?  */
              if (! found.empty ())
                {
                  if (all)
                    ret_list.splice (ret_list.end (), found);
                  else
                    {
                      ret_list.push_back (found.front ());
                      done = true;
                    }
                }
            }
        }
    }

  return ret_list;
}

static std::list<std::string>
find_first_of (const std::string& path, const std::list<std::string>& names,
               bool must_exist, bool all)
{
  std::list<std::string> ret_list;

  if (KPSE_DEBUG_P (KPSE_DEBUG_SEARCH))
    {
      std::cerr << "kdebug: start find_first_of ((";

      for (auto p = names.cbegin (); p != names.cend (); p++)
        {
          if (p == names.cbegin ())
            std::cerr << *p;
          else
            std::cerr << ", " << *p;
        }

      std::cerr << "), path=" << path << ", must_exist="
                << must_exist << "." << std::endl;
    }

  for (const auto &name : names)
    {
      if (kpse_absolute_p (name, true))
        {
          /* If the name is absolute or explicitly relative, no need
             to consider PATH at all.  If we find something, then we
             are done.  */

          ret_list = absolute_search (name);

          if (! ret_list.empty ())
            return ret_list;
        }
    }

  /* Find the file. */
  ret_list = path_find_first_of (path, names, must_exist, all);

  /* The very first search is for texmf.cnf.  We can't log that, since
     we want to allow setting TEXMFLOG in texmf.cnf.  */
  if (first_search)
    {
      first_search = false;
    }
  else
    {
      /* Record the filenames we found, if desired.  And wrap them in a
         debugging line if we're doing that.  */

      if (KPSE_DEBUG_P (KPSE_DEBUG_SEARCH))
        {
          std::cerr << "kdebug: find_first_of (";

          for (auto p = names.cbegin (); p != names.cend (); p++)
            {
              if (p == names.cbegin ())
                std:: cerr << *p;
              else
                std::cerr << ", " << *p;
            }

          std::cerr << ") =>";
        }

      log_search (ret_list);

      if (KPSE_DEBUG_P (KPSE_DEBUG_SEARCH))
        gnulib::putc ('\n', stderr);
    }

  return ret_list;
}

/* Search each element of PATH for each element of NAMES.  Return the
   first one found.  */

/* Search each element of PATH for each element in the list of NAMES.
   Return the first one found.  */

std::string
kpse_path_find_first_of (const std::string& path,
                         const std::list<std::string>& names,
                         bool must_exist)
{
  std::list<std::string> ret_list
    = find_first_of (path, names, must_exist, false);

  return ret_list.empty () ? "" : ret_list.front ();
}

/* Search each element of PATH for each element of NAMES and return a
   list containing everything found, in the order found.  */

/* Like 'kpse_path_find_first_of' with MUST_EXIST true, but return a
   list of all the filenames (or NULL if none), instead of taking the
   first.  */

std::list<std::string>
kpse_all_path_find_first_of (const std::string& path,
                             const std::list<std::string>& names)
{
  return find_first_of (path, names, true, true);
}

/* If NAME has a leading ~ or ~user, Unix-style, expand it to the user's
   home directory, and return a new malloced string.  If no ~, or no
   <pwd.h>, just return NAME.  */

static std::string
kpse_tilde_expand (const std::string& name)
{
  std::string expansion;

  /* If no leading tilde, do nothing.  */
  if (name.empty () || name[0] != '~')
    {
      expansion = name;

      /* If a bare tilde, return the home directory or '.'.  (Very
         unlikely that the directory name will do anyone any good, but
         ...  */
    }
  else if (name.length () == 1)
    {
      expansion = octave::sys::env::get_home_directory ();

      if (expansion.empty ())
        expansion = ".";

      /* If '~/', remove any trailing / or replace leading // in $HOME.
         Should really check for doubled intermediate slashes, too.  */
    }
  else if (IS_DIR_SEP (name[1]))
    {
      unsigned c = 1;
      std::string home = octave::sys::env::get_home_directory ();

      if (home.empty ())
        home = ".";

      size_t home_len = home.length ();

      /* handle leading // */
      if (home_len > 1 && IS_DIR_SEP (home[0]) && IS_DIR_SEP (home[1]))
        home = home.substr (1);

      /* omit / after ~ */
      if (IS_DIR_SEP (home[home_len - 1]))
        c++;

      expansion = home + name.substr (c);

      /* If '~user' or '~user/', look up user in the passwd database (but
         OS/2 doesn't have this concept.  */
    }
  else
#if defined (HAVE_PWD_H)
    {
      unsigned c = 2;

      /* find user name */
      while (name.length () > c && ! IS_DIR_SEP (name[c]))
        c++;

      std::string user = name.substr (1, c-1);

      /* We only need the cast here for (deficient) systems
         which do not declare 'getpwnam' in <pwd.h>.  */
      octave::sys::password p = octave::sys::password::getpwnam (user);

      /* If no such user, just use '.'.  */
      std::string home = p ? p.dir () : std::string (".");

      if (home.empty ())
        home = ".";

      /* handle leading // */
      if (home.length () > 1 && IS_DIR_SEP (home[0]) && IS_DIR_SEP (home[1]))
        home = home.substr (1);

      /* If HOME ends in /, omit the / after ~user. */
      if (name.length () > c && IS_DIR_SEP (home[home.length () - 1]))
        c++;

      expansion = name.length () > c ? home : home + name.substr (c);
    }
#else /* not HAVE_PWD_H */
  expansion = name;
#endif /* not HAVE_PWD_H */

  return expansion;
}

/* Do variable expansion first so ~${USER} works.  (Besides, it's what the
   shells do.)  */

/* Call kpse_var_expand and kpse_tilde_expand (in that order).  Result
   is always in fresh memory, even if no expansions were done.  */

static std::string
kpse_expand (const std::string& s)
{
  std::string var_expansion = kpse_var_expand (s);
  return kpse_tilde_expand (var_expansion);
}

/* Forward declarations of functions from the original expand.c  */
static std::list<std::string> brace_expand (const std::string&);

/* If $KPSE_DOT is defined in the environment, prepend it to any relative
   path components. */

static std::string
kpse_expand_kpse_dot (const std::string& path)
{
  std::string ret;
  std::string kpse_dot = octave::sys::env::getenv ("KPSE_DOT");

  if (kpse_dot.empty ())
    return path;

  for (kpse_path_iterator pi (path); pi != std::string::npos; pi++)
    {
      std::string elt = *pi;

      /* Single "." get special treatment, as does "./" or its equivalent.  */

      size_t elt_len = elt.length ();

      if (kpse_absolute_p (elt, false))
        ret += elt + ENV_SEP_STRING;
      else if (elt_len == 1 && elt[0] == '.')
        ret += kpse_dot + ENV_SEP_STRING;
      else if (elt_len > 1 && elt[0] == '.' && IS_DIR_SEP (elt[1]))
        ret += kpse_dot + elt.substr (1) + ENV_SEP_STRING;
      else
        ret += kpse_dot + DIR_SEP_STRING + elt + ENV_SEP_STRING;
    }

  int len = ret.length ();
  if (len > 0)
    ret.resize (len-1);

  return ret;
}

/* Do brace expansion on ELT; then do variable and ~ expansion on each
   element of the result; then do brace expansion again, in case a
   variable definition contained braces (e.g., $TEXMF).  Return a
   string comprising all of the results separated by ENV_SEP_STRING.  */

static std::string
kpse_brace_expand_element (const std::string& elt)
{
  std::string ret;

  std::list<std::string> expansions = brace_expand (elt);

  for (const auto &expanded_elt : expansions)
    {
      /* Do $ and ~ expansion on each element.  */
      std::string x = kpse_expand (expanded_elt);

      if (x != elt)
        {
          /* If we did any expansions, do brace expansion again.  Since
             recursive variable definitions are not allowed, this recursion
             must terminate.  (In practice, it's unlikely there will ever be
             more than one level of recursion.)  */
          x = kpse_brace_expand_element (x);
        }

      ret += x + ENV_SEP_STRING;
    }

  ret.resize (ret.length () - 1);

  return ret;
}

/* Do brace expansion and call 'kpse_expand' on each element of the
   result; return the final expansion (always in fresh memory, even if
   no expansions were done).  */

static std::string
kpse_brace_expand (const std::string& path)
{
  /* Must do variable expansion first because if we have
       foo = .:~
       TEXINPUTS = $foo
     we want to end up with TEXINPUTS = .:/home/karl.
     Since kpse_path_element is not reentrant, we must get all
     the path elements before we start the loop.  */
  std::string tmp = kpse_var_expand (path);

  std::string ret;

  for (kpse_path_iterator pi (tmp); pi != std::string::npos; pi++)
    {
      std::string elt = *pi;

      /* Do brace expansion first, so tilde expansion happens in {~ka,~kb}.  */
      std::string expansion = kpse_brace_expand_element (elt);
      ret += expansion + ENV_SEP_STRING;
    }

  size_t len = ret.length ();
  if (len > 0)
    ret.resize (len-1);

  return kpse_expand_kpse_dot (ret);
}

/* Expand all special constructs in a path, and include only the actually
   existing directories in the result. */

/* Do brace expansion and call 'kpse_expand' on each argument of the
   result.  The final expansion (always in fresh memory) is a path of
   all the existing directories that match the pattern. */

std::string
kpse_path_expand (const std::string& path)
{
  std::string ret;
  unsigned len;

  len = 0;

  /* Expand variables and braces first.  */
  std::string tmp = kpse_brace_expand (path);

  /* Now expand each of the path elements, printing the results */
  for (kpse_path_iterator pi (tmp); pi != std::string::npos; pi++)
    {
      std::string elt = *pi;

      std::string dir;

      /* Do not touch the device if present */
      if (NAME_BEGINS_WITH_DEVICE (elt))
        {
          while (elt.length () > 3
                 && IS_DIR_SEP (elt[2]) && IS_DIR_SEP (elt[3]))
            {
              elt[2] = elt[1];
              elt[1] = elt[0];
              elt = elt.substr (1);
            }
        }
      else
        {
          /* We never want to search the whole disk.  */
          while (elt.length () > 1
                 && IS_DIR_SEP (elt[0]) && IS_DIR_SEP (elt[1]))
            elt = elt.substr (1);
        }

      /* Search the disk for all dirs in the component specified.
         Be faster to check the database, but this is more reliable.  */
      dir = kpse_element_dir (elt);

      size_t dirlen = dir.length ();

      if (dirlen > 0)
        {
          ret += dir;
          len += dirlen;

          /* Retain trailing slash if that's the root directory.  */
          if (dirlen == 1
              || (dirlen == 3 && NAME_BEGINS_WITH_DEVICE (dir)
                  && IS_DIR_SEP (dir[2])))
            {
              ret += ENV_SEP_STRING;
              len++;
            }

          ret[len-1] = ENV_SEP;
        }
    }

  if (len > 0)
    ret.resize (len-1);

  return ret;
}

/* braces.c -- code for doing word expansion in curly braces.  Taken from
   bash 1.14.5.  [And subsequently modified for kpatshea.]

   Copyright (C) 1987,1991 Free Software Foundation, Inc.  */

#define brace_whitespace(c) (! (c) || (c) == ' ' || (c) == '\t' || (c) == '\n')

/* Basic idea:

   Segregate the text into 3 sections: preamble (stuff before an open brace),
   postamble (stuff after the matching close brace) and amble (stuff after
   preamble, and before postamble).  Expand amble, and then tack on the
   expansions to preamble.  Expand postamble, and tack on the expansions to
   the result so far.  */

/* Return a new array of strings which is the result of appending each
   string in ARR2 to each string in ARR1.  The resultant array is
   len (arr1) * len (arr2) long.  For convenience, ARR1 (and its contents)
   are free ()'ed.  ARR1 can be NULL, in that case, a new version of ARR2
   is returned. */

static std::list<std::string>
array_concat (const std::list<std::string>& arr1,
              const std::list<std::string>& arr2)
{
  std::list<std::string> result;

  if (arr1.empty ())
    result = arr2;
  else if (arr2.empty ())
    result = arr1;
  else
    {
      for (const auto &elt_2 : arr2)
        for (const auto &elt_1 : arr1)
          result.push_back (elt_1 + elt_2);
    }

  return result;
}

static int brace_gobbler (const std::string&, int&, int);
static std::list<std::string> expand_amble (const std::string&);

/* Return an array of strings; the brace expansion of TEXT. */
static std::list<std::string>
brace_expand (const std::string& text)
{
  /* Find the text of the preamble. */
  int i = 0;
  int c = brace_gobbler (text, i, '{');

  std::string preamble = text.substr (0, i);

  std::list<std::string> result (1, preamble);

  if (c == '{')
    {
      /* Find the amble.  This is the stuff inside this set of braces. */
      int start = ++i;
      c = brace_gobbler (text, i, '}');

      /* What if there isn't a matching close brace? */
      if (! c)
        {
          (*current_liboctave_warning_with_id_handler)
            ("Octave:pathsearch-syntax",
             "%s: Unmatched {", text.c_str ());

          result = std::list<std::string> (1, text);
        }
      else
        {
          std::string amble = text.substr (start, i-start);
          result = array_concat (result, expand_amble (amble));

          std::string postamble = text.substr (i+1);
          result = array_concat (result, brace_expand (postamble));
        }
    }

  return result;
}

/* The character which is used to separate arguments. */
static int brace_arg_separator = ',';

/* Expand the text found inside of braces.  We simply try to split the
   text at BRACE_ARG_SEPARATORs into separate strings.  We then brace
   expand each slot which needs it, until there are no more slots which
   need it. */
static std::list<std::string>
expand_amble (const std::string& text)
{
  std::list<std::string> result;

  size_t text_len = text.length ();
  size_t start;
  int i, c;

  for (start = 0, i = 0, c = 1; c && start < text_len; start = ++i)
    {
      int i0 = i;
      int c0 = brace_gobbler (text, i0, brace_arg_separator);
      int i1 = i;
      int c1 = brace_gobbler (text, i1, ENV_SEP);
      c = c0 | c1;
      i = (i0 < i1 ? i0 : i1);

      std::string tem = text.substr (start, i-start);

      std::list<std::string> partial = brace_expand (tem);

      if (result.empty ())
        result = partial;
      else
        result.splice (result.end (), partial);
    }

  return result;
}

/* Start at INDEX, and skip characters in TEXT.  Set INDEX to the
   index of the character matching SATISFY.  This understands about
   quoting.  Return the character that caused us to stop searching;
   this is either the same as SATISFY, or 0. */
static int
brace_gobbler (const std::string& text, int& indx, int satisfy)
{
  int c = 0;
  int level = 0;
  int quoted = 0;
  int pass_next = 0;

  size_t text_len = text.length ();

  size_t i = indx;

  for (; i < text_len; i++)
    {
      c = text[i];

      if (pass_next)
        {
          pass_next = 0;
          continue;
        }

      /* A backslash escapes the next character.  This allows backslash to
         escape the quote character in a double-quoted string. */
      if (c == '\\' && (quoted == 0 || quoted == '"' || quoted == '`'))
        {
          pass_next = 1;
          continue;
        }

      if (quoted)
        {
          if (c == quoted)
            quoted = 0;
          continue;
        }

      if (c == '"' || c == '\'' || c == '`')
        {
          quoted = c;
          continue;
        }

      if (c == satisfy && ! level && ! quoted)
        {
          /* We ignore an open brace surrounded by whitespace, and also
             an open brace followed immediately by a close brace, that
             was preceded with whitespace.  */
          if (c == '{'
              && ((i == 0 || brace_whitespace (text[i-1]))
                  && (i+1 < text_len
                      && (brace_whitespace (text[i+1]) || text[i+1] == '}'))))
            continue;
          /* If this is being compiled as part of bash, ignore the '{'
             in a '${ }' construct */
          if ((c != '{') || i == 0 || (text[i-1] != '$'))
            break;
        }

      if (c == '{')
        level++;
      else if (c == '}' && level)
        level--;
    }

  indx = i;
  return c;
}

/* Expand extra colons.  */

/* Check for leading colon first, then trailing, then doubled, since
   that is fastest.  Usually it will be leading or trailing.  */

/* Replace a leading or trailing or doubled : in PATH with DFLT.  If
   no extra colons, return PATH.  Only one extra colon is replaced.
   DFLT may not be NULL.  */

std::string
kpse_expand_default (const std::string& path, const std::string& fallback)
{
  std::string expansion;

  size_t path_len = path.length ();

  if (path_len == 0)
    expansion = fallback;

  /* Solitary or leading :?  */
  else if (IS_ENV_SEP (path[0]))
    {
      expansion = path_len == 1 ? fallback : fallback + path;
    }

  /* Sorry about the assignment in the middle of the expression, but
     conventions were made to be flouted and all that.  I don't see the
     point of calling strlen twice or complicating the logic just to
     avoid the assignment (especially now that I've pointed it out at
     such great length).  */
  else if (IS_ENV_SEP (path[path_len-1]))
    expansion = path + fallback;

  /* OK, not leading or trailing.  Check for doubled.  */
  else
    {
      /* What we'll return if we find none.  */
      expansion = path;

      for (size_t i = 0; i < path_len; i++)
        {
          if (i + 1 < path_len
              && IS_ENV_SEP (path[i]) && IS_ENV_SEP (path[i+1]))
            {
              /* We have a doubled colon.  */

              /* Copy stuff up to and including the first colon.  */
              /* Copy in FALLBACK, and then the rest of PATH.  */
              expansion = path.substr (0, i+1) + fallback + path.substr (i+1);

              break;
            }
        }
    }

  return expansion;
}

/* Return true if FN is a directory or a symlink to a directory,
   false if not. */

static bool
dir_p (const std::string& fn)
{
  octave::sys::file_stat fs (fn);

  return (fs && fs.is_dir ());
}

/* Given a path element ELT, return a the element with a trailing slash
   or an empty string if the element is not a directory.

   It's up to the caller to expand ELT.  This is because this routine is
   most likely only useful to be called from 'kpse_path_search', which
   has already assumed expansion has been done.  */

std::string
kpse_element_dir (const std::string& elt)
{
  std::string ret;

  /* If given nothing, return nothing.  */
  if (elt.empty ())
    return ret;

  if (dir_p (elt))
    {
      ret = elt;

      char last_char = ret[ret.length () - 1];

      if (! (IS_DIR_SEP (last_char) || IS_DEVICE_SEP (last_char)))
        ret += DIR_SEP_STRING;
    }

  return ret;
}

/* Variable expansion.  */

/* We have to keep track of variables being expanded, otherwise
   constructs like TEXINPUTS = $TEXINPUTS result in an infinite loop.
   (Or indirectly recursive variables, etc.)  Our simple solution is to
   add to a list each time an expansion is started, and check the list
   before expanding.  */

static std::map <std::string, bool> expansions;

static void
expanding (const std::string& var, bool xp)
{
  expansions[var] = xp;
}

/* Return whether VAR is currently being expanding.  */

static bool
expanding_p (const std::string& var)
{
  return (expansions.find (var) != expansions.end ()) ? expansions[var] : false;
}

/* Append the result of value of 'var' to EXPANSION, where 'var' begins
   at START and ends at END.  If 'var' is not set, do not complain.
   This is a subroutine for the more complicated expansion function.  */

static void
expand (std::string &expansion, const std::string& var)
{
  if (expanding_p (var))
    {
      (*current_liboctave_warning_with_id_handler)
        ("Octave:pathsearch-syntax",
         "pathsearch: variable '%s' references itself (eventually)",
         var.c_str ());
    }
  else
    {
      /* Check for an environment variable.  */
      std::string value = octave::sys::env::getenv (var);

      if (! value.empty ())
        {
          expanding (var, true);
          std::string tmp = kpse_var_expand (value);
          expanding (var, false);
          expansion += tmp;
        }
    }
}

/* Can't think of when it would be useful to change these (and the
   diagnostic messages assume them), but ... */

/* starts all variable references */
#if ! defined (IS_VAR_START)
#  define IS_VAR_START(c) ((c) == '$')
#endif

/* variable name constituent */
#if ! defined (IS_VAR_CHAR)
#  define IS_VAR_CHAR(c) (isalnum (c) || (c) == '_')
#endif

/* start delimited variable name (after $) */
#if ! defined (IS_VAR_BEGIN_DELIMITER)
#  define IS_VAR_BEGIN_DELIMITER(c) ((c) == '{')
#endif

#if ! defined (IS_VAR_END_DELIMITER)
#  define IS_VAR_END_DELIMITER(c) ((c) == '}')
#endif

/* Maybe we should support some or all of the various shell ${...}
   constructs, especially ${var-value}.  */

static std::string
kpse_var_expand (const std::string& src)
{
  std::string expansion;

  size_t src_len = src.length ();

  /* Copy everything but variable constructs.  */
  for (size_t i = 0; i < src_len; i++)
    {
      if (IS_VAR_START (src[i]))
        {
          i++;

          /* Three cases: '$VAR', '${VAR}', '$<anything-else>'.  */
          if (IS_VAR_CHAR (src[i]))
            {
              /* $V: collect name constituents, then expand.  */
              size_t var_end = i;

              do
                {
                  var_end++;
                }
              while (IS_VAR_CHAR (src[var_end]));

              var_end--; /* had to go one past */
              expand (expansion, src.substr (i, var_end - i + 1));
              i = var_end;

            }
          else if (IS_VAR_BEGIN_DELIMITER (src[i]))
            {
              /* ${: scan ahead for matching delimiter, then expand.  */
              size_t var_end = ++i;

              while (var_end < src_len && ! IS_VAR_END_DELIMITER (src[var_end]))
                var_end++;

              if (var_end == src_len)
                {
                  (*current_liboctave_warning_with_id_handler)
                    ("Octave:pathsearch-syntax",
                     "%s: No matching } for ${", src.c_str ());
                  i = var_end - 1; /* will incr to eos at top of loop */
                }
              else
                {
                  expand (expansion, src.substr (i, var_end - i));
                  i = var_end; /* will incr past } at top of loop*/
                }
            }
          else
            {
              /* $<something-else>: error.  */
              (*current_liboctave_warning_with_id_handler)
                ("Octave:pathsearch-syntax",
                 "%s: Unrecognized variable construct '$%c'",
                 src.c_str (), src[i]);

              /* Just ignore those chars and keep going.  */
            }
        }
      else
        expansion += src[i];
    }

  return expansion;
}
