/*

Copyright (C) 2010-2016 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or
(at your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <algorithm>
#include <string>

#include "glob-wrappers.h"

#include "oct-glob.h"
#include "file-stat.h"
#include "unwind-prot.h"

// These functions are defined here and not in glob_match.cc so that we
// can include the glob.h file from gnulib, which defines glob to
// be rpl_glob.  If we include glob.h in glob_match.cc, then it
// transforms the glob_match::glob function to be glob_match::rpl_glob,
// which is not what we want...

static bool
single_match_exists (const std::string& file)
{
  octave::sys::file_stat s (file);

  return s.exists ();
}

namespace octave
{
  namespace sys
  {
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

      octave::unwind_protect frame;

      void *glob_info = octave_create_glob_info_struct ();

      frame.add_fcn (octave_destroy_glob_info_struct, glob_info);

      for (int i = 0; i < npat; i++)
        {
          std::string xpat = pat(i);

          if (! xpat.empty ())
            {
#if (defined (OCTAVE_HAVE_WINDOWS_FILESYSTEM)           \
     && ! defined (OCTAVE_HAVE_POSIX_FILESYSTEM))
              std::replace_if (xpat.begin (), xpat.end (),
                               std::bind2nd (std::equal_to<char> (), '\\'),
                               '/');
#endif

              int err = octave_glob_wrapper (xpat.c_str (),
                                             octave_glob_nosort_wrapper (),
                                             glob_info);

              if (! err)
                {
                  int n = octave_glob_num_matches (glob_info);

                  const char * const *matches
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
                          std::replace_if (tmp.begin (), tmp.end (),
                                           std::bind2nd (std::equal_to<char> (),
                                                         '/'),
                                           '\\');
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

    // Glob like Windows "dir".  Treat only * and ? as wildcards,
    // and "*.*" matches filenames even if they do not contain ".".
    string_vector
    windows_glob (const string_vector& pat)
    {
      string_vector retval;

      int npat = pat.numel ();

      int k = 0;

      octave::unwind_protect frame;

      void *glob_info = octave_create_glob_info_struct ();

      frame.add_fcn (octave_destroy_glob_info_struct, glob_info);

      for (int i = 0; i < npat; i++)
        {
          std::string xpat = pat(i);

          if (! xpat.empty ())
            {
              std::string escaped;
              escaped.reserve (xpat.length ());

              for (size_t j = 0; j < xpat.length (); j++)
                {
#if (defined (OCTAVE_HAVE_WINDOWS_FILESYSTEM)           \
     && ! defined (OCTAVE_HAVE_POSIX_FILESYSTEM))
                  if (xpat[j] == '\\')
                    escaped += '/';
                  else
#endif
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

                  const char * const *matches
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

                          for (size_t m = 0; m < tmp.length (); m++)
                            {
#if (defined (OCTAVE_HAVE_WINDOWS_FILESYSTEM)           \
     && ! defined (OCTAVE_HAVE_POSIX_FILESYSTEM))
                              if (tmp[m] == '/')
                                unescaped += '\\';
                              else
#endif
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

      return retval.sort ();
    }
  }
}
