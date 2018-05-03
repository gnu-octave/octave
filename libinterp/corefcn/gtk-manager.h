/*

Copyright (C) 2007-2018 John W. Eaton

This file is part of Octave.

Octave is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<https://www.gnu.org/licenses/>.

*/

#if ! defined (octave_gtk_manager_h)
#define octave_gtk_manager_h 1

#include "octave-config.h"

#include <map>
#include <set>
#include <string>

#include "Cell.h"
#include "graphics-toolkit.h"

namespace octave
{
  class gtk_manager
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
      loaded_toolkits[tk.get_name ()] = tk;
    }

    void unload_toolkit (const std::string& name)
    {
      loaded_toolkits.erase (name);
    }

    graphics_toolkit find_toolkit (const std::string& name) const
    {
      const_loaded_toolkits_iterator p = loaded_toolkits.find (name);

      if (p != loaded_toolkits.end ())
        return p->second;
      else
        return graphics_toolkit ();
    }

    Cell available_toolkits_list (void) const
    {
      Cell m (1, available_toolkits.size ());

      octave_idx_type i = 0;
      for (const auto& tkit : available_toolkits)
        m(i++) = tkit;

      return m;
    }

    Cell loaded_toolkits_list (void) const
    {
      Cell m (1, loaded_toolkits.size ());

      octave_idx_type i = 0;
      for (const auto& nm_tkit_p : loaded_toolkits)
        m(i++) = nm_tkit_p.first;

      return m;
    }

    void unload_all_toolkits (void)
    {
      while (! loaded_toolkits.empty ())
        {
          auto p = loaded_toolkits.begin ();

          std::string name = p->first;

          p->second.close ();

          // The toolkit may have unloaded itself.  If not, we'll do it here.
          if (loaded_toolkits.find (name) != loaded_toolkits.end ())
            unload_toolkit (name);
        }
    }

    std::string default_toolkit (void) const { return dtk; }

  private:

    // The name of the default toolkit.
    std::string dtk;

    // The list of toolkits that we know about.
    std::set<std::string> available_toolkits;

    // The list of toolkits we have actually loaded.
    std::map<std::string, graphics_toolkit> loaded_toolkits;

    typedef std::set<std::string>::iterator available_toolkits_iterator;

    typedef std::set<std::string>::const_iterator
    const_available_toolkits_iterator;

    typedef std::map<std::string, graphics_toolkit>::iterator
    loaded_toolkits_iterator;

    typedef std::map<std::string, graphics_toolkit>::const_iterator
    const_loaded_toolkits_iterator;
  };
}

#endif
