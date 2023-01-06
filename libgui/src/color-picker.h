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

#if ! defined (octave_color_picker_h)
#define octave_color_picker_h 1

#include <QColorDialog>
#include <QPushButton>

OCTAVE_BEGIN_NAMESPACE(octave)

class color_picker : public QPushButton
{
  Q_OBJECT

public:

  color_picker (QColor color = QColor (0, 0, 0), QWidget *parent = nullptr);

  QColor color (void) const { return m_color; }

  void set_color (QColor new_color);

private slots:

  void select_color (void);

private:

  virtual void update_button (void);

  QColor m_color;
};

OCTAVE_END_NAMESPACE(octave)

#endif
