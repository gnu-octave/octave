/*

Copyright (C) 1996-2017 John W. Eaton

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

#include <cstdlib>

#include <string>

#include "kpse.h"
#include "lo-error.h"
#include "lo-utils.h"
#include "oct-env.h"
#include "pathsearch.h"
#include "singleton-cleanup.h"

namespace octave
{
  directory_path::static_members *directory_path::static_members::instance = nullptr;

  directory_path::static_members::static_members (void)
    : xpath_sep_char (SEPCHAR), xpath_sep_str (SEPCHAR_STR) { }

  bool
  directory_path::static_members::instance_ok (void)
  {
    bool retval = true;

    if (! instance)
      {
        instance = new static_members ();

        if (instance)
          singleton_cleanup_list::add (cleanup_instance);
      }

    if (! instance)
      (*current_liboctave_error_handler)
        ("unable to create directory_path::static_members object!");

    return retval;
  }

  std::list<std::string>
  directory_path::elements (void)
  {
    return m_initialized ? m_path_elements : std::list<std::string> ();
  }

  std::list<std::string>
  directory_path::all_directories (void)
  {
    std::list<std::string> retval;

    if (m_initialized)
      {
        for (const auto& elt : m_path_elements)
          {
            std::string elt_dir = kpse_element_dir (elt);

            if (! elt_dir.empty ())
              retval.push_back (elt_dir);
          }
      }

    return retval;
  }

  std::string
  directory_path::find_first (const std::string& nm)
  {
    return m_initialized ? kpse_path_search (m_expanded_path, nm) : "";
  }

  std::list<std::string>
  directory_path::find_all (const std::string& nm)
  {
    return (m_initialized
            ? kpse_all_path_search (m_expanded_path, nm)
            : std::list<std::string> ());
  }

  std::string
  directory_path::find_first_of (const std::list<std::string>& names)
  {
    return (m_initialized
            ? kpse_path_find_first_of (m_expanded_path, names) : "");
  }

  std::list<std::string>
  directory_path::find_all_first_of (const std::list<std::string>& names)
  {
    return (m_initialized
            ? kpse_all_path_find_first_of (m_expanded_path, names)
            : std::list<std::string> ());
  }

  void
  directory_path::init (void)
  {
    static bool octave_kpse_initialized = false;

    if (! octave_kpse_initialized)
      {
        std::string val = sys::env::getenv ("KPATHSEA_DEBUG");

        if (! val.empty ())
          kpse_debug |= atoi (val.c_str ());

        octave_kpse_initialized = true;
      }

    m_expanded_path
      = kpse_path_expand (m_default_path.empty ()
                          ? m_orig_path
                          : kpse_expand_default (m_orig_path, m_default_path));

    for (kpse_path_iterator pi (m_expanded_path); pi != std::string::npos; pi++)
      m_path_elements.push_back (*pi);

    m_initialized = true;
  }
}
