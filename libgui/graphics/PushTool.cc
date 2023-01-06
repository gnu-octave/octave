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

#include "PushTool.h"

#include "ToolBarButton.cc"

#include "octave-qobject.h"

OCTAVE_BEGIN_NAMESPACE(octave)

PushTool *
PushTool::create (octave::base_qobject& oct_qobj,
                  octave::interpreter& interp, const graphics_object& go)
{
  Object *parent = parentObject (interp, go);

  if (parent)
    {
      QWidget *parentWidget = parent->qWidget<QWidget> ();

      if (parentWidget)
        return new PushTool (oct_qobj, interp, go,
                             new QAction (parentWidget));
    }

  return nullptr;
}

PushTool::PushTool (octave::base_qobject& oct_qobj,
                    octave::interpreter& interp,
                    const graphics_object& go, QAction *action)
  : ToolBarButton<uipushtool> (oct_qobj, interp, go, action)
{
  connect (action, &QAction::triggered, this, &PushTool::clicked);
}

PushTool::~PushTool (void)
{ }

void
PushTool::update (int pId)
{
  switch (pId)
    {
    default:
      ToolBarButton<uipushtool>::update (pId);
      break;
    }
}

void
PushTool::clicked (void)
{
  emit gh_callback_event (m_handle, "clickedcallback");
}

OCTAVE_END_NAMESPACE(octave);
