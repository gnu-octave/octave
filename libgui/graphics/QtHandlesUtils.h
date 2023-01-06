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

#if ! defined (octave_QtHandlesUtils_h)
#define octave_QtHandlesUtils_h 1

#include <QColor>
#include <QFont>
#include <QImage>
#include <QString>
#include <QStringList>

#include <string>

#include "graphics.h"

class QKeyEvent;
class QMouseEvent;
class QWheelEvent;

OCTAVE_BEGIN_NAMESPACE(octave)

namespace Utils
{
  QString fromStdString (const std::string& s);
  std::string toStdString (const QString& s);

  QStringList fromStringVector (const string_vector& v);
  string_vector toStringVector (const QStringList& l);

  Cell toCellString (const QStringList& l);

  template <typename T>
  QFont computeFont (const typename T::properties& props, int height = -1);

  QColor fromRgb (const Matrix& rgb);
  Matrix toRgb (const QColor& c);

  Qt::Alignment fromHVAlign (const std::string& halign,
                             const std::string& valign);

  std::string figureSelectionType (QMouseEvent *event,
                                   bool isDoubleClick = false);

  Matrix figureCurrentPoint (const graphics_object& fig, QMouseEvent *event);
  Matrix figureCurrentPoint (const graphics_object& fig);

  template <typename T>
  inline typename T::properties&
  properties (graphics_object obj)
  { return dynamic_cast<typename T::properties&> (obj.get_properties ()); }

  QImage makeImageFromCData (const octave_value& v, int width = -1,
                             int height = -1);

  octave_scalar_map makeKeyEventStruct (QKeyEvent *event);
  octave_scalar_map makeScrollEventStruct (QWheelEvent *event);
}

OCTAVE_END_NAMESPACE(octave)

#endif
