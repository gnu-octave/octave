////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2019-2023 The Octave Project Developers
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

#include <QApplication>
#include <QMetaType>
#include <QThread>

#include "graphics-init.h"
#include "octave-qobject.h"
#include "qt-graphics-toolkit.h"
#include "QtHandlesUtils.h"

#include "graphics.h"
#include "gtk-manager.h"
#include "interpreter.h"

OCTAVE_BEGIN_NAMESPACE(octave)

void graphics_init (interpreter& interp, base_qobject& oct_qobj)
{
#if defined (HAVE_QT_GRAPHICS)

  gh_manager& gh_mgr = interp.get_gh_manager ();

  autolock guard (gh_mgr.graphics_lock ());

  qRegisterMetaType<graphics_object> ("graphics_object");

  gh_mgr.enable_event_processing (true);

  qt_graphics_toolkit *qt_gtk = new qt_graphics_toolkit (interp, oct_qobj);

  if (QThread::currentThread ()
      != QApplication::instance ()->thread ())
    qt_gtk->moveToThread (QApplication::instance ()->thread ());

  graphics_toolkit tk (qt_gtk);

  gtk_manager& gtk_mgr = interp.get_gtk_manager ();

  gtk_mgr.register_toolkit ("qt");

  gtk_mgr.load_toolkit (tk);

#else

  octave_unused_parameter (interp);
  octave_unused_parameter (oct_qobj);

#endif
}

OCTAVE_END_NAMESPACE(octave)
