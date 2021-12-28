////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2000-2022 The Octave Project Developers
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

#include "gui-utils.h"

namespace octave
{
  OCTGUI_API QColor
  interpolate_color (const QColor& col1, const QColor& col2,
                     double fs, double fv)
  {
    qreal h1, s1, v1, h2, s2, v2;

    col1.getHsvF (&h1, &s1, &v1);
    col2.getHsvF (&h2, &s2, &v2);

    return QColor::fromHsvF (h1, s1*fs, v1 + fv*(v2 - v1));
  }
}
