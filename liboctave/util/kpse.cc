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

// This file should not include config.h.  It is only included in other
// C++ source files that should have included config.h before including
// this file.

#include <map>
#include <fstream>
#include <iostream>
#include <string>

/* System defines are for non-Unix systems only.  (Testing for all Unix
   variations should be done in configure.)  Presently the defines used
   are: DOS OS2 WIN32.  I do not use any of these systems
   myself; if you do, I'd be grateful for any changes. --kb@mail.tug.org */

/* If we have either DOS or OS2, we are DOSISH.  */
#if defined (DOS) || defined (OS2) || defined (WIN32) || defined (__MSDOS__)
#define DOSISH
#endif

extern "C" {
#if defined (__MINGW32__)
#include <windows.h>
#include <fcntl.h>
#elif defined (WIN32)
#if ! defined (_MSC_VER)
#define __STDC__ 1
#include "win32lib.h"
#endif
#endif /* not WIN32 */
}

/* System dependencies that are figured out by 'configure'.  If we are
   compiling standalone, we get our c-auto.h.  Otherwise, the package
   containing us must provide this (unless it can somehow generate ours
   from c-auto.in).  We use <...> instead of "..." so that the current
   cpp directory (i.e., kpathsea/) won't be searched. */

/* If you want to find subdirectories in a directory with non-Unix
   semantics (specifically, if a directory with no subdirectories does
   not have exactly two links), define this.  */
#if ! defined (DOSISH)
#define ST_NLINK_TRICK
#endif /* not DOSISH */

/* Define the characters which separate components of
   filenames and environment variable paths.  */

/* What separates filename components?  */
#if ! defined (DIR_SEP)
#if defined (DOSISH)
/* Either \'s or 's work.  Wayne Sullivan's web2pc prefers /, so we'll
   go with that.  */
#define DIR_SEP '/'
#define DIR_SEP_STRING "/"
#define IS_DEVICE_SEP(ch) ((ch) == ':')
#define NAME_BEGINS_WITH_DEVICE(name) ((name.length ()>0) && IS_DEVICE_SEP((name)[1]))
/* On DOS, it's good to allow both \ and / between directories.  */
#define IS_DIR_SEP(ch) ((ch) == '/' || (ch) == '\\')
#else
#define DIR_SEP '/'
#define DIR_SEP_STRING "/"
#endif /* not DOSISH */
#endif /* not DIR_SEP */

#if ! defined (IS_DIR_SEP)
#define IS_DIR_SEP(ch) ((ch) == DIR_SEP)
#endif
#if ! defined (IS_DEVICE_SEP)
/* No 'devices' on, e.g., Unix.  */
#define IS_DEVICE_SEP(ch) 0
#endif
#if ! defined (NAME_BEGINS_WITH_DEVICE)
#define NAME_BEGINS_WITH_DEVICE(name) 0
#endif

#include "file-stat.h"
#include "lo-error.h"
#include "oct-env.h"
#include "oct-passwd.h"
#include "str-vec.h"

/* Header files that essentially all of our sources need, and
   that all implementations have.  We include these first, to help with
   NULL being defined multiple times.  */
#include <cstdarg>
#include <cstdlib>
#include <cerrno>
#include <cassert>

#include <sys/types.h>
#include <unistd.h>

#include <dirent.h>

/* define NAME_MAX, the maximum length of a single
   component in a filename.  No such limit may exist, or may vary
   depending on the filesystem.  */

/* Most likely the system will truncate filenames if it is not POSIX,
   and so we can use the BSD value here.  */
#if ! defined (_POSIX_NAME_MAX)
#define _POSIX_NAME_MAX 255
#endif

#if ! defined (NAME_MAX)
#define NAME_MAX _POSIX_NAME_MAX
#endif

#include <cctype>

/* What separates elements in environment variable path lists?  */
#if ! defined (ENV_SEP)
#if defined (SEPCHAR) && defined (SEPCHAR_STR)
#define ENV_SEP SEPCHAR
#define ENV_SEP_STRING SEPCHAR_STR
#elif defined (DOSISH)
#define ENV_SEP ';'
#define ENV_SEP_STRING ";"
#else
#define ENV_SEP ':'
#define ENV_SEP_STRING ":"
#endif /* not DOS */
#endif /* not ENV_SEP */

#if ! defined (IS_ENV_SEP)
#define IS_ENV_SEP(ch) ((ch) == ENV_SEP)
#endif

/* If NO_DEBUG is defined (not recommended), skip all this.  */
#if ! defined (NO_DEBUG)

/* OK, we'll have tracing support.  */
#define KPSE_DEBUG

/* Test if a bit is on.  */
#define KPSE_DEBUG_P(bit) (kpathsea_debug & (1 << (bit)))

#define KPSE_DEBUG_STAT 0               /* stat calls */
#define KPSE_DEBUG_EXPAND 1             /* path element expansion */
#define KPSE_DEBUG_SEARCH 2             /* searches */
#define KPSE_DEBUG_VARS 3               /* variable values */
#define KPSE_LAST_DEBUG KPSE_DEBUG_VARS

#endif /* not NO_DEBUG */

#if defined (KPSE_DEBUG)
static unsigned int kpathsea_debug = 0;
#endif

#if defined (WIN32) && ! defined (__MINGW32__)

/* System description file for Windows NT.  */

/*
 *      Define symbols to identify the version of Unix this is.
 *      Define all the symbols that apply correctly.
 */

#if ! defined (DOSISH)
#define DOSISH
#endif

/* These have to be defined because our compilers treat __STDC__ as being
   defined (most of them anyway). */

#define access  _access

/* Define this so that winsock.h definitions don't get included when
   windows.h is...  For this to have proper effect, config.h must
   always be included before windows.h.  */
#define _WINSOCKAPI_    1

#include <windows.h>

/* For proper declaration of environ.  */
#include <io.h>
#include <fcntl.h>
#include <process.h>

/* ============================================================ */

#endif /* WIN32 */

/* Define common sorts of messages.  */

/* This should be called only after a system call fails.  Don't exit
   with status 'errno', because that might be 256, which would mean
   success (exit statuses are truncated to eight bits).  */
#define FATAL_PERROR(str) \
  do \
    { \
      std::cerr << "pathsearch: "; \
      perror (str); exit (EXIT_FAILURE); \
    } \
  while (0)

#define FATAL(str) \
  do \
    { \
      std::cerr << "pathsearch: fatal: " << str << "." << std::endl; \
      exit (1); \
    } \
  while (0)

#if ! defined (WIN32)
static void xclosedir (DIR *d);
#endif

/* It's a little bizarre to be using the same type for the list and the
   elements of the list, but no reason not to in this case, I think --
   we never need a NULL string in the middle of the list, and an extra
   NULL/NULL element always at the end is inconsequential.  */

struct str_llist_elt
{
  str_llist_elt (void) : str (), next (0) { }

  ~str_llist_elt (void) { }

  std::string str;
  struct str_llist_elt *next;
};

typedef str_llist_elt str_llist_elt_type;
typedef str_llist_elt *str_llist_type;

#define STR_LLIST(sl) ((sl).str)
#define STR_LLIST_NEXT(sl) ((sl).next)

static void str_llist_add (str_llist_type *l, const std::string& str);

static std::string kpse_var_expand (const std::string& src);

static str_llist_type *kpse_element_dirs (const std::string& elt);

static std::string kpse_expand (const std::string& s);

static std::string kpse_expand_default (const std::string& path,
                                        const std::string& dflt);

#include <ctime> /* for 'time' */

static bool
kpse_is_env_sep (char c)
{
  return IS_ENV_SEP (c);
}

/* A way to step through a path, extracting one directory name at a
   time.  */

class kpse_path_iterator
{
public:

  kpse_path_iterator (const std::string& p)
    : path (p), b (0), e (0), len (path.length ()) { set_end (); }

  kpse_path_iterator (const kpse_path_iterator& pi)
    : path (pi.path), b (pi.b), e (pi.e), len (pi.len) { }

  kpse_path_iterator operator ++ (int)
  {
    kpse_path_iterator retval (*this);
    next ();
    return retval;
  }

  std::string operator * (void) { return path.substr (b, e-b); }

  bool operator != (const size_t sz) { return b != sz; }

private:

  const std::string& path;
  size_t b;
  size_t e;
  size_t len;

  void set_end (void)
  {
    e = b + 1;

    if (e == len)
      ; /* OK, we have found the last element.  */
    else if (e > len)
      b = e = std::string::npos;
    else
      {
        /* Find the next colon not enclosed by braces (or the end of
           the path).  */
        while (e < len && ! kpse_is_env_sep (path[e]))
          e++;
      }
  }

  void next (void)
  {
    b = e + 1;

    /* Skip any consecutive colons.  */
    while (b < len && kpse_is_env_sep (path[b]))
      b++;

    if (b >= len)
      b = e = std::string::npos;
    else
      set_end ();
  }

  // No assignment.
  kpse_path_iterator& operator = (const kpse_path_iterator&);
};

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

#if defined (WIN32)
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
dir_list_search (str_llist_type *dirs, const std::string& name,
                 bool search_all)
{
  str_llist_elt_type *elt;
  std::list<std::string> ret;

  for (elt = *dirs; elt; elt = STR_LLIST_NEXT (*elt))
    {
      const std::string dir = STR_LLIST (*elt);

      std::string potential = dir + name;

      std::string tmp = kpse_readable_file (potential);

      if (! tmp.empty ())
        {
          ret.push_back (potential);

          if (! search_all)
            return ret;
        }
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

      /* Try ls-R, unless we're searching for texmf.cnf.  Our caller
         (search), also tests first_search, and does the resetting.  */
      if (first_search)
        found = std::list<std::string> ();

      /* Search the filesystem.  */

      if (found.empty ())
        {
          str_llist_type *dirs = kpse_element_dirs (elt);

          if (dirs && *dirs)
            found = dir_list_search (dirs, name, all);
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

static std::string
kpse_path_search (const std::string& path, const std::string& name,
                  bool must_exist)
{
  std::list<std::string> ret_list = search (path, name, must_exist, false);

  return ret_list.empty () ? "" : ret_list.front ();
}

/* Search all elements of PATH for files named NAME.  Not sure if it's
   right to assert 'must_exist' here, but it suffices now.  */

/* Like 'kpse_path_search' with MUST_EXIST true, but return a list of
   all the filenames (or NULL if none), instead of taking the first.  */

static std::list<std::string>
kpse_all_path_search (const std::string& path, const std::string& name)
{
  return search (path, name, true, true);
}

/* This is the hard case -- look in each element of PATH for each
   element of NAMES.  If ALL is false, return the first file found.
   Otherwise, search all elements of PATH.  */

static std::list<std::string>
path_find_first_of (const std::string& path,
                    const std::list<std::string>& names,
                    bool /* must_exist */, bool all)
{
  std::list<std::string> ret_list;
  bool done = false;

  for (kpse_path_iterator pi (path); ! done && pi != std::string::npos; pi++)
    {
      std::string elt = *pi;

      str_llist_type *dirs;
      str_llist_elt_type *dirs_elt;
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
      dirs = kpse_element_dirs (elt);
      for (dirs_elt = *dirs; dirs_elt; dirs_elt = STR_LLIST_NEXT (*dirs_elt))
        {
          const std::string dir = STR_LLIST (*dirs_elt);

          for (auto it = names.cbegin (); it != names.cend () && ! done; it++)
            {
              std::string name = *it;

              /* Try ls-R, unless we're searching for texmf.cnf.  Our caller
                 (find_first_of), also tests first_search, and does the
                 resetting.  */
              if (first_search)
                found = std::list<std::string> ();

              /* Search the filesystem.  */

              if (found.empty ())
                {
                  static str_llist_type *tmp = 0;

                  if (! tmp)
                    {
                      tmp = new str_llist_type;
                      *tmp = 0;
                      str_llist_add (tmp, "");
                    }

                  STR_LLIST (*(*tmp)) = dir;

                  found = dir_list_search (tmp, name, all);
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

static std::string
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

static std::list<std::string>
kpse_all_path_find_first_of (const std::string& path,
                             const std::list<std::string>& names)
{
  return find_first_of (path, names, true, true);
}

/* General expansion.  Some of this file (the brace-expansion
   code from bash) is covered by the GPL; this is the only GPL-covered
   code in kpathsea.  The part of the file that I wrote (the first
   couple of functions) is covered by the LGPL.  */

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

      /* We assume that the !! magic is only used on absolute components.
         Single "." get special treatment, as does "./" or its equivalent.  */

      size_t elt_len = elt.length ();

      if (kpse_absolute_p (elt, false)
          || (elt_len > 1 && elt[0] == '!' && elt[1] == '!'))
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
   no expansions were done).  We don't call 'kpse_expand_default'
   because there is a whole sequence of defaults to run through; see
   'kpse_init_format'.  */

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
   result, then expand any '//' constructs.  The final expansion (always
   in fresh memory) is a path of all the existing directories that match
   the pattern. */

static std::string
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

      str_llist_type *dirs;

      /* Skip and ignore magic leading chars.  */
      if (elt.length () > 1 && elt[0] == '!' && elt[1] == '!')
        elt = elt.substr (2);

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
      dirs = kpse_element_dirs (elt);

      if (dirs && *dirs)
        {
          str_llist_elt_type *dir;

          for (dir = *dirs; dir; dir = STR_LLIST_NEXT (*dir))
            {
              const std::string thedir = STR_LLIST (*dir);
              unsigned dirlen = thedir.length ();

              ret += thedir;
              len += dirlen;

              /* Retain trailing slash if that's the root directory.  */
              if (dirlen == 1
                  || (dirlen == 3 && NAME_BEGINS_WITH_DEVICE (thedir)
                      && IS_DIR_SEP (thedir[2])))
                {
                  ret += ENV_SEP_STRING;
                  len++;
                }

              ret[len-1] = ENV_SEP;
            }
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

/* Return true if FILENAME could be in PATH_ELT, i.e., if the directory
   part of FILENAME matches PATH_ELT.  Have to consider // wildcards, but
   $ and ~ expansion have already been done.  */

static bool
match (const std::string& filename_arg, const std::string& path_elt_arg)
{
  const char *filename = filename_arg.c_str ();
  const char *path_elt = path_elt_arg.c_str ();

  const char *original_filename = filename;
  bool matched = false;

  for (; *filename && *path_elt; filename++, path_elt++)
    {
      if (*filename == *path_elt) /* normal character match */
        ;

      else if (IS_DIR_SEP (*path_elt)  /* at // */
               && original_filename < filename && IS_DIR_SEP (path_elt[-1]))
        {
          while (IS_DIR_SEP (*path_elt))
            path_elt++; /* get past second and any subsequent /'s */

          if (*path_elt == 0)
            {
              /* Trailing //, matches anything.  We could make this
                 part of the other case, but it seems pointless to do
                 the extra work.  */
              matched = true;
              break;
            }
          else
            {
              /* Intermediate //, have to match rest of PATH_ELT.  */
              for (; ! matched && *filename; filename++)
                {
                  /* Try matching at each possible character.  */
                  if (IS_DIR_SEP (filename[-1]) && *filename == *path_elt)
                    matched = match (filename, path_elt);
                }

              /* Prevent filename++ when *filename='\0'. */
              break;
            }
        }
      else
        /* normal character nonmatch, quit */
        break;
    }

  /* If we've reached the end of PATH_ELT, check that we're at the last
     component of FILENAME, we've matched.  */
  if (! matched && *path_elt == 0)
    {
      /* Probably PATH_ELT ended with 'vf' or some such, and FILENAME
         ends with 'vf/ptmr.vf'.  In that case, we'll be at a
         directory separator.  On the other hand, if PATH_ELT ended
         with a / (as in 'vf/'), FILENAME being the same 'vf/ptmr.vf',
         we'll be at the 'p'.  Upshot: if we're at a dir separator in
         FILENAME, skip it.  But if not, that's ok, as long as there
         are no more dir separators.  */

      if (IS_DIR_SEP (*filename))
        filename++;

      while (*filename && ! IS_DIR_SEP (*filename))
        filename++;

      matched = *filename == 0;
    }

  return matched;
}

/* Expand extra colons.  */

/* Check for leading colon first, then trailing, then doubled, since
   that is fastest.  Usually it will be leading or trailing.  */

/* Replace a leading or trailing or doubled : in PATH with DFLT.  If
   no extra colons, return PATH.  Only one extra colon is replaced.
   DFLT may not be NULL.  */

static std::string
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

/* Translate a path element to its corresponding director{y,ies}.  */

/* To avoid giving prototypes for all the routines and then their real
   definitions, we give all the subroutines first.  The entry point is
   the last routine in the file.  */

/* Make a copy of DIR (unless it's null) and save it in L.  Ensure that
   DIR ends with a DIR_SEP for the benefit of later searches.  */

static void
dir_list_add (str_llist_type *l, const std::string& dir)
{
  char last_char = dir[dir.length () - 1];

  std::string saved_dir = dir;

  if (! (IS_DIR_SEP (last_char) || IS_DEVICE_SEP (last_char)))
    saved_dir += DIR_SEP_STRING;

  str_llist_add (l, saved_dir);
}

/* Return true if FN is a directory or a symlink to a directory,
   false if not. */

static bool
dir_p (const std::string& fn)
{
  octave::sys::file_stat fs (fn);

  return (fs && fs.is_dir ());
}

/* If DIR is a directory, add it to the list L.  */

static void
checked_dir_list_add (str_llist_type *l, const std::string& dir)
{
  if (dir_p (dir))
    dir_list_add (l, dir);
}

/* The cache.  Typically, several paths have the same element; for
   example, /usr/local/lib/texmf/fonts//.  We don't want to compute the
   expansion of such a thing more than once.  Even though we also cache
   the dir_links call, that's not enough -- without this path element
   caching as well, the execution time doubles.  */

struct cache_entry
{
  cache_entry (void) : key (), value (0) { }

  ~cache_entry (void) { }

  std::string key;
  str_llist_type *value;
};

static cache_entry *the_cache = 0;
static unsigned cache_length = 0;

/* Associate KEY with VALUE.  We implement the cache as a simple linear
   list, since it's unlikely to ever be more than a dozen or so elements
   long.  We don't bother to check here if PATH has already been saved;
   we always add it to our list.  We copy KEY but not VALUE; not sure
   that's right, but it seems to be all that's needed.  */

static void
cache (const std::string key, str_llist_type *value)
{
  cache_entry *new_cache = new cache_entry [cache_length+1];

  for (unsigned i = 0; i < cache_length; i++)
    {
      new_cache[i].key = the_cache[i].key;
      new_cache[i].value = the_cache[i].value;
    }

  delete [] the_cache;

  the_cache = new_cache;

  the_cache[cache_length].key = key;
  the_cache[cache_length].value = value;

  cache_length++;
}

/* To retrieve, just check the list in order.  */

static str_llist_type *
cached (const std::string& key)
{
  unsigned p;

  for (p = 0; p < cache_length; p++)
    {
      if (key == the_cache[p].key)
        return the_cache[p].value;
    }

  return 0;
}

#if defined (WIN32)

/* Shared across recursive calls, it acts like a stack. */
static std::string dirname;

#else /* WIN32 */

/* Return -1 if FN isn't a directory, else its number of links.
   Duplicate the call to stat; no need to incur overhead of a function
   call for that little bit of cleanliness. */

static int
dir_links (const std::string& fn)
{
  int retval;

  octave::sys::file_stat fs (fn);

  retval = fs && (fs.is_dir () ? fs.nlink () : -1);

#if defined (KPSE_DEBUG)
  if (KPSE_DEBUG_P (KPSE_DEBUG_STAT))
    std::cerr << "kdebug: dir_links (" << fn << ") => " << retval << std::endl;
#endif

  return retval;
}

#endif /* WIN32 */

static inline void
do_subdir (str_llist_type *str_list_ptr, const std::string& elt,
           unsigned elt_length, const std::string& post)
{
#if defined (WIN32)
  WIN32_FIND_DATA find_file_data;
  HANDLE hnd;
  int proceed;
#else
  DIR *dir;
  struct dirent *e;
#endif /* not WIN32 */

  std::string name = elt.substr (0, elt_length);

  assert (IS_DIR_SEP (elt[elt_length - 1])
          || IS_DEVICE_SEP (elt[elt_length - 1]));

#if defined (WIN32)

  dirname = name + "/*.*";         /* "*.*" or "*" -- seems equivalent. */

  hnd = FindFirstFile (dirname.c_str (), &find_file_data);

  if (hnd == INVALID_HANDLE_VALUE)
    return;

  /* Include top level before subdirectories, if nothing to match.  */
  if (post.empty ())
    dir_list_add (str_list_ptr, name);
  else
    {
      /* If we do have something to match, see if it exists.  For
         example, POST might be 'pk/ljfour', and they might have a
         directory '$TEXMF/fonts/pk/ljfour' that we should find.  */
      name += post;
      checked_dir_list_add (str_list_ptr, name);
      name.resize (elt_length);
    }

  proceed = 1;

  while (proceed)
    {
      if (find_file_data.cFileName[0] != '.')
        {
          /* Construct the potential subdirectory name.  */
          name += find_file_data.cFileName;

          if (find_file_data.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY)
            {
              /* It's a directory, so append the separator.  */
              name += DIR_SEP_STRING;
              unsigned potential_len = name.length ();

              do_subdir (str_list_ptr, name, potential_len, post);
            }
          name.resize (elt_length);
        }

      proceed = FindNextFile (hnd, &find_file_data);
    }

  FindClose (hnd);

#else /* not WIN32 */

  /* If we can't open it, quit.  */
  dir = gnulib::opendir (name.c_str ());

  if (! dir)
    return;

  /* Include top level before subdirectories, if nothing to match.  */
  if (post.empty ())
    dir_list_add (str_list_ptr, name);
  else
    {
      /* If we do have something to match, see if it exists.  For
         example, POST might be 'pk/ljfour', and they might have a
         directory '$TEXMF/fonts/pk/ljfour' that we should find.  */
      name += post;
      checked_dir_list_add (str_list_ptr, name);
      name.resize (elt_length);
    }

  while ((e = gnulib::readdir (dir)))
    {
      /* If it begins with a '.', never mind.  (This allows "hidden"
         directories that the algorithm won't find.)  */

      if (e->d_name[0] != '.')
        {
          /* Construct the potential subdirectory name.  */
          name += e->d_name;

          /* If we can't stat it, or if it isn't a directory, continue.  */
          int links = dir_links (name);

          if (links >= 0)
            {
              /* It's a directory, so append the separator.  */
              name += DIR_SEP_STRING;
              unsigned potential_len = name.length ();

              /* Should we recurse?  To see if the subdirectory is a
                 leaf, check if it has two links (one for . and one for
                 ..).  This means that symbolic links to directories do
                 not affect the leaf-ness.  This is arguably wrong, but
                 the only alternative I know of is to stat every entry
                 in the directory, and that is unacceptably slow.

                 The #if here makes all this configurable at
                 compile-time, so that if we're using VMS directories or
                 some such, we can still find subdirectories, even if it
                 is much slower.  */
#if defined (ST_NLINK_TRICK)
              if (links != 2)
#endif /* not ST_NLINK_TRICK */
                /* All criteria are met; find subdirectories.  */
                do_subdir (str_list_ptr, name, potential_len, post);
#if defined (ST_NLINK_TRICK)
              else if (post.empty ())
                /* Nothing to match, no recursive subdirectories to
                   look for: we're done with this branch.  Add it.  */
                dir_list_add (str_list_ptr, name);
#endif
            }

          /* Remove the directory entry we just checked from 'name'.  */
          name.resize (elt_length);
        }
    }

  xclosedir (dir);
#endif /* not WIN32 */
}

/* Here is the entry point.  Returns directory list for ELT.  */

/* Given a path element ELT, return a pointer to a NULL-terminated list
   of the corresponding (existing) directory or directories, with
   trailing slashes, or NULL.  If ELT is the empty string, check the
   current working directory.

   It's up to the caller to expand ELT.  This is because this routine is
   most likely only useful to be called from 'kpse_path_search', which
   has already assumed expansion has been done.  */

static str_llist_type *
kpse_element_dirs (const std::string& elt)
{
  str_llist_type *ret;

  /* If given nothing, return nothing.  */
  if (elt.empty ())
    return 0;

  /* If we've already cached the answer for ELT, return it.  */
  ret = cached (elt);
  if (ret)
    return ret;

  /* We're going to have a real directory list to return.  */
  ret = new str_llist_type;
  *ret = 0;

  /* We handle the hard case in a subroutine.  */
  checked_dir_list_add (ret, elt);

  /* Remember the directory list we just found, in case future calls are
     made with the same ELT.  */
  cache (elt, ret);

#if defined (KPSE_DEBUG)
  if (KPSE_DEBUG_P (KPSE_DEBUG_EXPAND))
    {
      std::cerr << "kdebug: path element " << elt << " =>";
      if (ret)
        {
          str_llist_elt_type *e;
          for (e = *ret; e; e = STR_LLIST_NEXT (*e))
            std::cerr << " " << STR_LLIST (*e);
        }
      std::cerr << std::endl;
    }
#endif /* KPSE_DEBUG */

  return ret;
}

#if ! defined (WIN32)
void
xclosedir (DIR *d)
{
  int ret = gnulib::closedir (d);

  if (ret != 0)
    FATAL ("closedir failed");
}
#endif

/* Implementation of a linked list of strings.  */

/* Add the new string STR to the end of the list L.  */

static void
str_llist_add (str_llist_type *l, const std::string& str)
{
  str_llist_elt_type *e;
  str_llist_elt_type *new_elt = new str_llist_elt_type;

  /* The new element will be at the end of the list.  */
  STR_LLIST (*new_elt) = str;
  STR_LLIST_NEXT (*new_elt) = 0;

  /* Find the current end of the list.  */
  for (e = *l; e && STR_LLIST_NEXT (*e); e = STR_LLIST_NEXT (*e))
    ;

  if (! e)
    *l = new_elt;
  else
    STR_LLIST_NEXT (*e) = new_elt;
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
         "kpathsea: variable '%s' references itself (eventually)",
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
#if ! defined (IS_VAR_START)
/* starts all variable references */
#define IS_VAR_START(c) ((c) == '$')
#endif
#if ! defined (IS_VAR_CHAR)
/* variable name constituent */
#define IS_VAR_CHAR(c) (isalnum (c) || (c) == '_')
#endif
#if ! defined (IS_VAR_BEGIN_DELIMITER)
/* start delimited variable name (after $) */
#define IS_VAR_BEGIN_DELIMITER(c) ((c) == '{')
#endif
#if ! defined (IS_VAR_END_DELIMITER)
#define IS_VAR_END_DELIMITER(c) ((c) == '}')
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
