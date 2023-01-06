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

#if ! defined (octave_ButtonControl_h)
#define octave_ButtonControl_h 1

#include "BaseControl.h"

class QAbstractButton;

OCTAVE_BEGIN_NAMESPACE(octave)

class base_qobject;
class interpreter;

class ButtonControl : public BaseControl
{
  Q_OBJECT

public:
  ButtonControl (octave::base_qobject& oct_qobj, octave::interpreter& interp,
                 const graphics_object& go, QAbstractButton *btn);
  ~ButtonControl (void);

protected:
  void update (int pId);

private slots:
  void clicked (void);
  void toggled (bool checked);

private:
  bool m_blockCallback;
};

OCTAVE_END_NAMESPACE(octave)

#endif
