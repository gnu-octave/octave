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

#if ! defined (octave_ToggleTool_h)
#define octave_ToggleTool_h 1

#include "ToolBarButton.h"

OCTAVE_BEGIN_NAMESPACE(octave)

class base_qobject;
class interpreter;

class ToggleTool : public ToolBarButton<uitoggletool>
{
  Q_OBJECT

public:
  ToggleTool (octave::base_qobject& oct_qobj, octave::interpreter& interp,
              const graphics_object& go,
              QAction *action);
  ~ToggleTool (void);

  static ToggleTool *
  create (octave::base_qobject& oct_qobj, octave::interpreter& interp,
          const graphics_object& go);

protected:
  void update (int pId);

private slots:
  void triggered (bool checked);
};

OCTAVE_END_NAMESPACE(octave)

#endif
