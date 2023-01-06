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

#if ! defined (octave_Panel_h)
#define octave_Panel_h 1

#include "Object.h"

class QFrame;
class QLabel;

OCTAVE_BEGIN_NAMESPACE(octave)

class base_qobject;
class interpreter;

class Container;

class Panel : public Object
{
public:
  Panel (octave::base_qobject& oct_qobj, octave::interpreter& interp,
         const graphics_object& go, QFrame *frame);
  ~Panel (void);

  Container * innerContainer (void) { return m_container; }

  bool eventFilter (QObject *watched, QEvent *event);

  static Panel *
  create (octave::base_qobject& oct_qobj, octave::interpreter& interp,
          const graphics_object& go);

  void do_connections (const QObject *receiver,
                       const QObject *emitter = nullptr);

protected:
  void update (int pId);
  void redraw (void);

private:
  void updateLayout (void);

private:
  Container *m_container;
  QLabel *m_title;
  bool m_blockUpdates;
  Matrix m_previous_bbox;
};

OCTAVE_END_NAMESPACE(octave)

#endif
