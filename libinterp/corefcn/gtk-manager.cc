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

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include "error.h"
#include "graphics-toolkit.h"
#include "gtk-manager.h"
#include "ovl.h"
#include "parse.h"

namespace octave
{
  graphics_toolkit
  gtk_manager::get_toolkit (void) const
  {
    graphics_toolkit retval;

    if (dtk.empty ())
      error ("no graphics toolkits are available!");

    const_loaded_toolkits_iterator pl = loaded_toolkits.find (dtk);

    if (pl == loaded_toolkits.end ())
      {
        const_available_toolkits_iterator pa = available_toolkits.find (dtk);

        if (pa == available_toolkits.end ())
          error ("default graphics toolkit '%s' is not available!",
                 dtk.c_str ());

        octave_value_list args;
        args(0) = dtk;
        octave::feval ("graphics_toolkit", args);

        pl = loaded_toolkits.find (dtk);

        if (pl == loaded_toolkits.end ())
          error ("failed to load %s graphics toolkit", dtk.c_str ());

        retval = pl->second;
      }
    else
      retval = pl->second;

    return retval;
  }

  void
  gtk_manager::register_toolkit (const std::string& name)
  {
    if (dtk.empty () || name == "qt"
        || (name == "fltk"
            && available_toolkits.find ("qt") == available_toolkits.end ()))
      dtk = name;

    available_toolkits.insert (name);
  }

  void
  gtk_manager::unregister_toolkit (const std::string& name)
  {
    available_toolkits.erase (name);

    if (dtk == name)
      {
        if (available_toolkits.empty ())
          dtk.clear ();
        else
          {
            const_available_toolkits_iterator pa = available_toolkits.begin ();

            dtk = *pa++;

            while (pa != available_toolkits.end ())
              {
                std::string tk_name = *pa++;

                if (tk_name == "qt"
                    || (tk_name == "fltk"
                        && (available_toolkits.find ("qt")
                            == available_toolkits.end ())))
                  dtk = tk_name;
              }
          }
      }
  }
}
