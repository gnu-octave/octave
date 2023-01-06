////////////////////////////////////////////////////////////////////////
//
// This class provides a simple color picker based on tQColorButton
// by Harald Jedele, 23.03.01, GPL version 2 or any later version.
//
// Copyright (C) 2013-2023 The Octave Project Developers
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

#include "color-picker.h"

OCTAVE_BEGIN_NAMESPACE(octave)

// Constructor with initial color as parameter
color_picker::color_picker (QColor old_color, QWidget *p)
: QPushButton (p)
{
  m_color = old_color;
  setFlat (true);
  setFocusPolicy (Qt::NoFocus);  // no focus, would change the color
  update_button ();
  connect (this, &color_picker::clicked, this, &color_picker::select_color);
}

// Slot for button clicked: select a new color using QColorDialog
void color_picker::select_color (void)
{
  QColor new_color = QColorDialog::getColor (m_color);

  if (new_color.isValid () && new_color != m_color)
    {
      m_color = new_color;
      update_button ();
    }
}

// Set the color of the button
void color_picker::set_color (QColor new_color)
{
  m_color = new_color;
  update_button ();
}

// Draw the button with the actual color (using a stylesheet)
void color_picker::update_button (void)
{
  // Is this the right place to look for a "foreground" color that would
  // provide a reasonable border for the color swatches?
  QWidget *p = parentWidget ();

  QString bordercolor
    = (p ? p->palette ().text ().color ().name () : QString ("#000000"));

  setStyleSheet (QString ("background-color: %1; border: 1px solid %2;")
                 .arg (m_color.name ())
                 .arg (bordercolor));

  repaint ();
}

OCTAVE_END_NAMESPACE(octave)
