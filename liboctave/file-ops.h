/*

Copyright (C) 1996, 1997, 2000, 2002, 2005, 2006, 2007, 2008 John W. Eaton

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

#if !defined (octave_file_ops_h)
#define octave_file_ops_h 1

#include <string>

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#include "str-vec.h"

struct
OCTAVE_API
file_ops
{
public:

  static int mkdir (const std::string&, mode_t);
  static int mkdir (const std::string&, mode_t, std::string&);

  static int mkfifo (const std::string&, mode_t);
  static int mkfifo (const std::string&, mode_t, std::string&);

  static int link (const std::string&, const std::string&);
  static int link (const std::string&, const std::string&, std::string&);

  static int symlink (const std::string&, const std::string&);
  static int symlink (const std::string&, const std::string&, std::string&);

  static int readlink (const std::string&, std::string&);
  static int readlink (const std::string&, std::string&, std::string&);

  static int rename (const std::string&, const std::string&);
  static int rename (const std::string&, const std::string&, std::string&);

  static int rmdir (const std::string&);
  static int rmdir (const std::string&, std::string&);

  static int recursive_rmdir (const std::string&);
  static int recursive_rmdir (const std::string&, std::string&);

  static std::string canonicalize_file_name (const std::string&);
  static std::string canonicalize_file_name (const std::string&, std::string&);

  static std::string tempnam (const std::string&, const std::string&);
  static std::string tempnam (const std::string&, const std::string&,
			      std::string&);

  typedef std::string (*tilde_expansion_hook) (const std::string&);

  static tilde_expansion_hook tilde_expansion_preexpansion_hook;

  static tilde_expansion_hook tilde_expansion_failure_hook;

  static string_vector tilde_additional_prefixes;

  static string_vector tilde_additional_suffixes;

  static std::string tilde_expand (const std::string&);
  static string_vector tilde_expand (const string_vector&);

  static int umask (mode_t);

  static int unlink (const std::string&);
  static int unlink (const std::string&, std::string&);

  static bool is_dir_sep (char c)
  {
    std::string tmp = dir_sep_chars ();
    return tmp.find (c) != std::string::npos;
  }

  static std::string concat (const std::string&, const std::string&);

  static char dir_sep_char (void)
  {
    return static_members::dir_sep_char ();
  }

  static std::string dir_sep_str (void)
  {
    return static_members::dir_sep_str ();
  }

  static std::string dir_sep_chars (void)
  {
    return static_members::dir_sep_chars ();
  }

  // Return the tail member of a path.
  static std::string tail (std::string path)
  {
    size_t ipos = path.find_last_of (dir_sep_chars ());

    if (ipos != std::string::npos)
      ipos++;
    else
      ipos = 0;

    return path.substr (ipos);
  }

private:

  // Use a singleton class for these data members instead of just
  // making them static members of the dir_path class so that we can
  // ensure proper initialization.

  class static_members
  {
  public:

    static_members (void);

    static char dir_sep_char (void)
    {
      return instance_ok () ? instance->xdir_sep_char : 0;
    }

    static std::string dir_sep_str (void)
    {
      return instance_ok () ? instance->xdir_sep_str : std::string ();
    }

    static std::string dir_sep_chars (void)
    {
      return instance_ok () ? instance->xdir_sep_chars : std::string ();
    }

  private:

    // The real thing.
    static static_members *instance;

    // No copying!

    static_members (const static_members&);

    static_members& operator = (const static_members&);

    static bool instance_ok (void);

    char xdir_sep_char;
    std::string xdir_sep_str;
    std::string xdir_sep_chars;
  };
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
