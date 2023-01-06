////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2016-2023 The Octave Project Developers
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

#if ! defined (octave_ButtonGroup_h)
#define octave_ButtonGroup_h 1

#include "Object.h"

class QAbstractButton;
class QButtonGroup;
class QFrame;
class QLabel;
class QRadioButton;

OCTAVE_BEGIN_NAMESPACE(octave)

class base_qobject;
class interpreter;

class Container;

class ButtonGroup : public Object
{
  Q_OBJECT

public:
  ButtonGroup (octave::base_qobject& oct_qobj, octave::interpreter& interp,
               const graphics_object& go, QButtonGroup *buttongroup,
               QFrame *frame);
  ~ButtonGroup (void);

  Container * innerContainer (void) { return m_container; }

  bool eventFilter (QObject *watched, QEvent *event);

  static ButtonGroup *
  create (octave::base_qobject& oct_qobj, octave::interpreter& interp,
          const graphics_object& go);

  void addButton (QAbstractButton *btn);

  void selectNothing (void);

protected:
  void update (int pId);
  void redraw (void);

private slots:
  void buttonToggled (bool toggled);
  void buttonClicked (QAbstractButton *btn);

private:
  void updateLayout (void);

private:
  QButtonGroup *m_buttongroup;
  QRadioButton *m_hiddenbutton;
  Container *m_container;
  QLabel *m_title;
  bool m_blockUpdates;
};

OCTAVE_END_NAMESPACE(octave)

#endif
