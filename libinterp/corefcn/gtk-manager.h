////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2007-2023 The Octave Project Developers
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

#if ! defined (octave_gtk_manager_h)
#define octave_gtk_manager_h 1

#include "octave-config.h"

#include <map>
#include <set>
#include <string>

#include "Cell.h"
#include "graphics-toolkit.h"

OCTAVE_BEGIN_NAMESPACE(octave)

class OCTINTERP_API gtk_manager
{
public:

  gtk_manager (void) { }

  ~gtk_manager (void)
  {
    unload_all_toolkits ();
  }

  graphics_toolkit get_toolkit (void) const;

  void register_toolkit (const std::string& name);

  void unregister_toolkit (const std::string& name);

  void load_toolkit (const graphics_toolkit& tk)
  {
    m_loaded_toolkits[tk.get_name ()] = tk;
  }

  void unload_toolkit (const std::string& name)
  {
    m_loaded_toolkits.erase (name);
  }

  graphics_toolkit find_toolkit (const std::string& name) const
  {
    auto p = m_loaded_toolkits.find (name);

    if (p != m_loaded_toolkits.end ())
      return p->second;
    else
      return graphics_toolkit ();
  }

  Cell available_toolkits_list (void) const
  {
    Cell m (1, m_available_toolkits.size ());

    octave_idx_type i = 0;
    for (const auto& tkit : m_available_toolkits)
      m(i++) = tkit;

    return m;
  }

  Cell loaded_toolkits_list (void) const
  {
    Cell m (1, m_loaded_toolkits.size ());

    octave_idx_type i = 0;
    for (const auto& nm_tkit_p : m_loaded_toolkits)
      m(i++) = nm_tkit_p.first;

    return m;
  }

  void unload_all_toolkits (void)
  {
    while (! m_loaded_toolkits.empty ())
      {
        auto p = m_loaded_toolkits.begin ();

        std::string name = p->first;

        p->second.close ();

        // The toolkit may have unloaded itself.  If not, we'll do it here.
        if (m_loaded_toolkits.find (name) != m_loaded_toolkits.end ())
          unload_toolkit (name);
      }
  }

  std::string default_toolkit (void) const { return m_dtk; }

private:

  // The name of the default toolkit.
  std::string m_dtk;

  // The list of toolkits that we know about.
  std::set<std::string> m_available_toolkits;

  // The list of toolkits we have actually loaded.
  std::map<std::string, graphics_toolkit> m_loaded_toolkits;
};

OCTAVE_END_NAMESPACE(octave)

#endif
