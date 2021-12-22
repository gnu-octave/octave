////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1993-2021 The Octave Project Developers
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

#if ! defined (octave_gui_utils_h)
#define octave_gui_utils_h 1

#include "octave-config.h"

#include <QColor>

namespace octave
{

  /*!
      Deterimine an alternative color to @p col1 with less contrast
      to @p col2. The HSV representation of the new color is calculated by
        \f{eqnarray*}{
           H &=& H_1\\
           S &=& f_s S_1\\
           V &=& V_1 + f_s (V_2 - V_1)
        \f}

      @param col1 Base color to which the alternative has to be computed
      @param col2 Color to which the the new color should have less contrast
      @param fs Factor for changing the saturation \f$(0\ldots\infty)\f$
      @param fv Factor for interpolating the brightness \f$(0\ldots 1)\f$.
             For 0, \f$V = V_1\f$ and for 1, \f$V = V_2\f$.

      @return New color as QColor
  */

  extern OCTGUI_API QColor
  interpolate_color (const QColor& col1, const QColor& col2,
                     double fs, double fv);

}

#endif
