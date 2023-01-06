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

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include "error.h"
#include "graphics-toolkit.h"
#include "gtk-manager.h"
#include "ovl.h"
#include "parse.h"

OCTAVE_BEGIN_NAMESPACE(octave)

graphics_toolkit
gtk_manager::get_toolkit (void) const
{
  graphics_toolkit retval;

  if (m_dtk.empty ())
    error ("no graphics toolkits are available!");

  auto pl = m_loaded_toolkits.find (m_dtk);

  if (pl == m_loaded_toolkits.end ())
    {
      auto pa = m_available_toolkits.find (m_dtk);

      if (pa == m_available_toolkits.end ())
        error ("default graphics toolkit '%s' is not available!",
               m_dtk.c_str ());

      octave_value_list args;
      args(0) = m_dtk;
      feval ("graphics_toolkit", args);

      pl = m_loaded_toolkits.find (m_dtk);

      if (pl == m_loaded_toolkits.end ())
        error ("failed to load %s graphics toolkit", m_dtk.c_str ());

      retval = pl->second;
    }
  else
    retval = pl->second;

  return retval;
}

void
gtk_manager::register_toolkit (const std::string& name)
{
  if (m_dtk.empty () || name == "qt"
      || (name == "fltk"
          && m_available_toolkits.find ("qt") == m_available_toolkits.end ()))
    m_dtk = name;

  m_available_toolkits.insert (name);
}

void
gtk_manager::unregister_toolkit (const std::string& name)
{
  m_available_toolkits.erase (name);

  if (m_dtk == name)
    {
      if (m_available_toolkits.empty ())
        m_dtk.clear ();
      else
        {
          auto pa = m_available_toolkits.cbegin ();

          m_dtk = *pa++;

          while (pa != m_available_toolkits.cend ())
            {
              std::string tk_name = *pa++;

              if (tk_name == "qt"
                  || (tk_name == "fltk"
                      && (m_available_toolkits.find ("qt")
                          == m_available_toolkits.cend ())))
                m_dtk = tk_name;
            }
        }
    }
}

OCTAVE_END_NAMESPACE(octave)
