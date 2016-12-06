/*

Copyright (C) 1996-2016 John W. Eaton

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

#if ! defined (octave_pathsearch_h)
#define octave_pathsearch_h 1

#include "octave-config.h"

#include <list>
#include <string>

namespace octave
{
  class
  OCTAVE_API
  directory_path
  {
  public:

    directory_path (const std::string& s = "", const std::string& d = "")
      : m_orig_path (s), m_default_path (d), m_initialized (false),
        m_expanded_path (), m_path_elements ()
    {
      if (! m_orig_path.empty ())
        init ();
    }

    directory_path (const directory_path& dp)
      : m_orig_path (dp.m_orig_path),
        m_default_path (dp.m_default_path),
        m_initialized (dp.m_initialized),
        m_expanded_path (dp.m_expanded_path),
        m_path_elements (dp.m_path_elements)
    { }

    directory_path& operator = (const directory_path& dp)
    {
      if (this != &dp)
        {
          m_orig_path = dp.m_orig_path;
          m_default_path = dp.m_default_path;
          m_initialized = dp.m_initialized;
          m_expanded_path = dp.m_expanded_path;
          m_path_elements = dp.m_path_elements;
        }

      return *this;
    }

    ~directory_path (void) = default;

    void set (const std::string& s)
    {
      m_initialized = false;
      m_orig_path = s;
      init ();
    }

    std::list<std::string> elements (void);

    std::list<std::string> all_directories (void);

    std::string find_first (const std::string&);

    std::string find (const std::string& nm) { return find_first (nm); }

    std::list<std::string> find_all (const std::string&);

    std::string find_first_of (const std::list<std::string>& names);

    std::list<std::string>
    find_all_first_of (const std::list<std::string>& names);

    void rehash (void)
    {
      m_initialized = false;
      init ();
    }

    static char path_sep_char (void)
    {
      return static_members::path_sep_char ();
    }

    static void path_sep_char (char c)
    {
      static_members::path_sep_char (c);
    }

    static std::string path_sep_str (void)
    {
      return static_members::path_sep_str ();
    }

    static bool is_path_sep (char c) { return c == path_sep_char (); }

  private:

    // The colon separated list that we were given.
    std::string m_orig_path;

    // The default path.  If specified, replaces leading, trailing, or
    // doubled colons in p_orig.
    std::string m_default_path;

    // TRUE means we've unpacked the path p.
    bool m_initialized;

    // A version of the colon separate list on which we have performed
    // tilde, variable, and possibly default path expansion.
    std::string m_expanded_path;

    // The elements of the list.
    std::list<std::string> m_path_elements;

    void init (void);

    // Use a singleton class for these data members instead of just
    // making them static members of the directory_path class so that
    // we can ensure proper initialization.

    class OCTAVE_API static_members
    {
    public:

      static_members (void);

      static char path_sep_char (void)
      {
        return instance_ok () ? instance->xpath_sep_char : 0;
      }

      static void path_sep_char (char c)
      {
        if (instance_ok ())
          {
            instance->xpath_sep_char = c;
            instance->xpath_sep_str = std::string (1, c);
          }
      }

      static std::string path_sep_str (void)
      {
        return instance_ok () ? instance->xpath_sep_str : "";
      }

    private:

      static static_members *instance;

      static void cleanup_instance (void) { delete instance; instance = 0; }

      static bool instance_ok (void);

      // No copying!

      static_members (const static_members&) = delete;

      static_members& operator = (const static_members&) = delete;

      char xpath_sep_char;

      std::string xpath_sep_str;
    };
  };
}

#if defined (OCTAVE_USE_DEPRECATED_FUNCTIONS)

OCTAVE_DEPRECATED ("use 'octave::directory_path' instead")
typedef octave::directory_path dir_path;

#endif

#endif

