////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2011-2023 The Octave Project Developers
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

#include "ToggleTool.h"

#include "ToolBarButton.cc"

#include "octave-qobject.h"

OCTAVE_BEGIN_NAMESPACE(octave)

ToggleTool *
ToggleTool::create (octave::base_qobject& oct_qobj,
                    octave::interpreter& interp, const graphics_object& go)
{
  Object *parent = parentObject (interp, go);

  if (parent)
    {
      QWidget *parentWidget = parent->qWidget<QWidget> ();

      if (parentWidget)
        return new ToggleTool (oct_qobj, interp, go,
                               new QAction (parentWidget));
    }

  return nullptr;
}

ToggleTool::ToggleTool (octave::base_qobject& oct_qobj,
                        octave::interpreter& interp,
                        const graphics_object& go, QAction *action)
  : ToolBarButton<uitoggletool> (oct_qobj, interp, go, action)
{
  uitoggletool::properties& tp = properties<uitoggletool> ();

  action->setCheckable (true);
  action->setChecked (tp.is_state ());

  connect (action, &QAction::toggled, this, &ToggleTool::triggered);
}

ToggleTool::~ToggleTool (void)
{ }

void
ToggleTool::update (int pId)
{
  uitoggletool::properties& tp = properties<uitoggletool> ();
  QAction *action = qWidget<QAction> ();

  switch (pId)
    {
    case uitoggletool::properties::ID_STATE:
      action->setChecked (tp.is_state ());
      break;

    default:
      ToolBarButton<uitoggletool>::update (pId);
      break;
    }
}

void
ToggleTool::triggered (bool checked)
{
  emit gh_set_event (m_handle, "state", checked, false);
  emit gh_callback_event (m_handle, checked ? "oncallback" : "offcallback");
  emit gh_callback_event (m_handle, "clickedcallback");
}

OCTAVE_END_NAMESPACE(octave);
