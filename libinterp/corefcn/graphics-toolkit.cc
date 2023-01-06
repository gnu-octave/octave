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

#include "graphics.h"
#include "gtk-manager.h"
#include "interpreter-private.h"

OCTAVE_BEGIN_NAMESPACE(octave)

void
base_graphics_toolkit::update (const graphics_handle& h, int id)
{
  gh_manager& gh_mgr = __get_gh_manager__ ();

  graphics_object go = gh_mgr.get_object (h);

  update (go, id);
}

bool
base_graphics_toolkit::initialize (const graphics_handle& h)
{
  gh_manager& gh_mgr = __get_gh_manager__ ();

  graphics_object go = gh_mgr.get_object (h);

  return initialize (go);
}

void
base_graphics_toolkit::finalize (const graphics_handle& h)
{
  gh_manager& gh_mgr = __get_gh_manager__ ();

  graphics_object go = gh_mgr.get_object (h);

  finalize (go);
}

OCTAVE_END_NAMESPACE(octave)
