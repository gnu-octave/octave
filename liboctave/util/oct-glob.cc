////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2010-2023 The Octave Project Developers
//
// See the file COPYRIGHT.md in the top-level directory of this
// distribution or <https://octave.org/copyright/>.
//
// This file is part of Octave.
//
// Octave is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// Octave is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Octave; see the file COPYING.  If not, see
// <https://www.gnu.org/licenses/>.
//
////////////////////////////////////////////////////////////////////////

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <algorithm>
#include <string>

#include "glob-wrappers.h"

#include "oct-glob.h"
#include "file-ops.h"
#include "file-stat.h"
#include "unwind-prot.h"

#if defined (OCTAVE_USE_WINDOWS_API)
#  include <windows.h>
#  include <shlwapi.h>
#  include <wchar.h>

#  include "lo-sysdep.h"
#endif

// These functions are defined here and not in glob_match.cc so that we
// can include the glob.h file from gnulib, which defines glob to
// be rpl_glob.  If we include glob.h in glob_match.cc, then it
// transforms the glob_match::glob function to be glob_match::rpl_glob,
// which is not what we want...

OCTAVE_BEGIN_NAMESPACE(octave)

static bool
single_match_exists (const std::string& file)
{
  sys::file_stat s (file);

  return s.exists ();
}

OCTAVE_BEGIN_NAMESPACE(sys)

bool
fnmatch (const string_vector& pat, const std::string& str, int fnm_flags)
{
  int npat = pat.numel ();

  const char *cstr = str.c_str ();

  for (int i = 0; i < npat; i++)
    if (octave_fnmatch_wrapper (pat(i).c_str (), cstr, fnm_flags)
        != octave_fnm_nomatch_wrapper ())
      return true;

  return false;
}

string_vector
glob (const string_vector& pat)
{
  string_vector retval;

  int npat = pat.numel ();

  int k = 0;

  void *glob_info = octave_create_glob_info_struct ();

  unwind_action cleanup_glob_info_struct
  ([=] () { octave_destroy_glob_info_struct (glob_info); });

  for (int i = 0; i < npat; i++)
    {
      std::string xpat = pat(i);

      if (! xpat.empty ())
        {
#if (defined (OCTAVE_HAVE_WINDOWS_FILESYSTEM)           \
     && ! defined (OCTAVE_HAVE_POSIX_FILESYSTEM))
          std::replace (xpat.begin (), xpat.end (), '\\', '/');
#endif

          int err = octave_glob_wrapper (xpat.c_str (),
                                         octave_glob_nosort_wrapper (),
                                         glob_info);

          if (! err)
            {
              int n = octave_glob_num_matches (glob_info);

              const char *const *matches
                = octave_glob_match_list (glob_info);

              // FIXME: we shouldn't have to check to see if
              // a single match exists, but it seems that glob() won't
              // check for us unless the pattern contains globbing
              // characters.  Hmm.

              if (n > 1
                  || (n == 1
                      && single_match_exists (std::string (matches[0]))))
                {
                  retval.resize (k+n);

                  for (int j = 0; j < n; j++)
                    {
                      std::string tmp = matches[j];

#if defined (OCTAVE_HAVE_WINDOWS_FILESYSTEM)    \
  && ! defined (OCTAVE_HAVE_POSIX_FILESYSTEM)
                      std::replace (tmp.begin (), tmp.end (), '/', '\\');
#endif

                      retval[k++] = tmp;
                    }
                }

              octave_globfree_wrapper (glob_info);
            }
        }
    }

  return retval.sort ();
}

#if defined (OCTAVE_USE_WINDOWS_API)

static void
find_files (std::list<std::string>& dirlist, const std::string& dir,
            const std::string& pat, std::string& file)
{
  // remove leading file separators
  bool is_file_empty = file.empty ();
  while (! file.empty () && sys::file_ops::is_dir_sep (file[0]))
    file = file.substr (1, std::string::npos);

  bool is_trailing_file_sep = ! is_file_empty && file.empty ();

  if (! pat.compare (".") || ! pat.compare (".."))
    {
      // shortcut for trivial patterns that would expand to a folder name

      // get next component of path (or file name)
      std::size_t sep_pos
        = file.find_first_of (sys::file_ops::dir_sep_chars ());
      std::string pat_str = file.substr (0, sep_pos);
      std::string file_str = (sep_pos != std::string::npos)
                             ? file.substr (sep_pos) : "";

      // Original pattern ends with "." or "..". Take it as we have it.
      if (pat_str.empty ())
        {
          if (is_trailing_file_sep)
            pat_str = sys::file_ops::dir_sep_char ();
          dirlist.push_back (sys::file_ops::concat (dir, pat) + pat_str);
          return;
        }

      // call this function recursively with next path component in PAT
      find_files (dirlist, sys::file_ops::concat (dir, pat),
                  pat_str, file_str);
      return;
    }

  // find first file in directory that matches pattern in PAT
  std::wstring wpat = u8_to_wstring (sys::file_ops::concat (dir, pat));
  _WIN32_FIND_DATAW ffd;
  HANDLE h_find = FindFirstFileW (wpat.c_str (), &ffd);
  // ignore any error
  if (h_find == INVALID_HANDLE_VALUE)
    return;

  unwind_action close_h_find ([=] () { FindClose (h_find); });

  // find all files that match pattern
  do
    {
      // must be directory if pattern continues
      if (! (ffd.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY)
          && (! file.empty () || is_trailing_file_sep))
        continue;

      std::string found_dir = u8_from_wstring (ffd.cFileName);

      if (file.empty ())
        {
          // Don't include "." and ".." in matches.
          if (found_dir.compare (".") && found_dir.compare (".."))
            {
              if (is_trailing_file_sep)
                found_dir += sys::file_ops::dir_sep_char ();
              dirlist.push_back (sys::file_ops::concat (dir, found_dir));
            }
        }
      else
        {
          // get next component of path (or file name)
          std::size_t sep_pos
            = file.find_first_of (sys::file_ops::dir_sep_chars ());
          std::string pat_str = file.substr (0, sep_pos);
          std::string file_str = (sep_pos != std::string::npos)
                                 ? file.substr (sep_pos) : "";

          // call this function recursively with next path component in PAT
          find_files (dirlist, sys::file_ops::concat (dir, found_dir),
                      pat_str, file_str);
        }
    }
  while (FindNextFileW (h_find, &ffd) != 0);
}

#endif

// Glob like Windows "dir".  Treat only * and ? as wildcards,
// and "*.*" matches filenames even if they do not contain ".".
string_vector
windows_glob (const string_vector& pat)
{
  string_vector retval;

  int npat = pat.numel ();

#if defined (OCTAVE_USE_WINDOWS_API)

  std::list<std::string> dirlist;

  for (int i = 0; i < npat; i++)
    {
      std::string xpat = pat(i);
      if (xpat.empty ())
        continue;

      std::string dir = "";

      // separate component until first file separator
      std::size_t sep_pos
        = xpat.find_first_of (sys::file_ops::dir_sep_chars ());

      // handle UNC paths
      if (sep_pos == 0 && xpat.length () > 1
          && sys::file_ops::is_dir_sep (xpat[1]))
        {
          // start pattern with a file, i.e., "\\SERVER\share\file"
          sep_pos = xpat.find_first_of (sys::file_ops::dir_sep_chars (), 2);
          if (sep_pos != std::string::npos)
            sep_pos = xpat.find_first_of (sys::file_ops::dir_sep_chars (),
                                          sep_pos + 1);
          if (sep_pos != std::string::npos)
            {
              dir = xpat.substr(0, sep_pos);
              xpat = xpat.substr (sep_pos+1);
              sep_pos = xpat.find_first_of (sys::file_ops::dir_sep_chars ());
            }
        }

      std::string file = (sep_pos != std::string::npos)
                         ? xpat.substr (sep_pos) : "";
      xpat = xpat.substr (0, sep_pos);

      if ((sep_pos == 2 || xpat.length () == 2) && xpat[1] == ':')
        {
          // include disc root with first file or folder

          // remove leading file separators in path without disc root
          while (file.length () > 1 && sys::file_ops::is_dir_sep (file[0]))
            file = file.substr (1, std::string::npos);

          sep_pos = file.find_first_of (sys::file_ops::dir_sep_chars ());
          dir = xpat;
          xpat = file.substr (0, sep_pos);
          file = (sep_pos != std::string::npos)
                 ? file.substr (sep_pos) : "";
          if (xpat.empty ())
            {
              // don't glob if input is only disc root
              std::wstring wpat = u8_to_wstring (pat(i));
              if (PathFileExistsW (wpat.c_str ()))
                {
                  if (sys::file_ops::is_dir_sep (pat(i).back ()))
                    dirlist.push_back (dir +
                                       sys::file_ops::dir_sep_char ());
                  else
                    dirlist.push_back (dir);
                }
              continue;
            }
        }

      find_files (dirlist, dir, xpat, file);
    }

  retval = string_vector (dirlist);

#else

  int k = 0;

  void *glob_info = octave_create_glob_info_struct ();

  unwind_action cleanup_glob_info_struct
  ([=] () { octave_destroy_glob_info_struct (glob_info); });

  for (int i = 0; i < npat; i++)
    {
      std::string xpat = pat(i);

      if (! xpat.empty ())
        {
          std::string escaped;
          escaped.reserve (xpat.length ());

          for (std::size_t j = 0; j < xpat.length (); j++)
            {
#  if (defined (OCTAVE_HAVE_WINDOWS_FILESYSTEM)           \
       && ! defined (OCTAVE_HAVE_POSIX_FILESYSTEM))
              if (xpat[j] == '\\')
                escaped += '/';
              else
#  endif
                {
                  if (xpat[j] == ']' || xpat[j] == '[')
                    escaped += '\\';

                  escaped += xpat[j];
                }
            }

          // Replace trailing "*.*" by "*".
          int len = escaped.length ();
          if (len >= 3 && escaped.substr (len - 3) == "*.*")
            escaped = escaped.substr (0, len - 2);

          int err = octave_glob_wrapper (escaped.c_str (),
                                         octave_glob_nosort_wrapper (),
                                         glob_info);

          if (! err)
            {
              int n = octave_glob_num_matches (glob_info);

              const char *const *matches
                = octave_glob_match_list (glob_info);

              // FIXME: we shouldn't have to check to see if
              // a single match exists, but it seems that glob() won't
              // check for us unless the pattern contains globbing
              // characters.  Hmm.

              if (n > 1
                  || (n == 1
                      && single_match_exists (std::string (matches[0]))))
                {
                  retval.resize (k + n);

                  for (int j = 0; j < n; j++)
                    {
                      std::string tmp = matches[j];

                      std::string unescaped;
                      unescaped.reserve (tmp.length ());

                      for (std::size_t m = 0; m < tmp.length (); m++)
                        {
#  if (defined (OCTAVE_HAVE_WINDOWS_FILESYSTEM)           \
     && ! defined (OCTAVE_HAVE_POSIX_FILESYSTEM))
                          if (tmp[m] == '/')
                            unescaped += '\\';
                          else
#  endif
                            {
                              if (tmp[m] == '\\'
                                  && ++m == tmp.length ())
                                break;

                              unescaped += tmp[m];
                            }
                        }

                      retval[k++] = unescaped;
                    }
                }

              octave_globfree_wrapper (glob_info);
            }
        }
    }
#endif

  return retval.sort ();
}

OCTAVE_END_NAMESPACE(sys)
OCTAVE_END_NAMESPACE(octave)
