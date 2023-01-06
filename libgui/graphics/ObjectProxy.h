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

#if ! defined (octave_ObjectProxy_h)
#define octave_ObjectProxy_h 1

#include "uint8NDArray.h"

#include <QObject>

class QString;

OCTAVE_BEGIN_NAMESPACE(octave)

class Object;

class ObjectProxy : public QObject
{
  Q_OBJECT

public:
  ObjectProxy (Object *obj = nullptr);

  void update (int pId);
  void finalize (void);
  void redraw (void);
  void show (void);
  void print (const QString& file_cmd, const QString& term);
  uint8NDArray get_pixels (void);

  Object * object (void) { return m_object; }
  void setObject (Object *obj);

signals:
  void sendUpdate (int pId);
  void sendRedraw (void);
  void sendShow (void);

private:
  void init (Object *obj);

private:
  Object *m_object;
};

OCTAVE_END_NAMESPACE(octave);

#endif
