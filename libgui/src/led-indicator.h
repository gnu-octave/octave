////////////////////////////////////////////////////////////////////////
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

#if ! defined (led_indicator_h)
#define led_indicator_h 1

#include <QLabel>

OCTAVE_BEGIN_NAMESPACE(octave)

class led_indicator: public QLabel
{
  Q_OBJECT

public:

  enum led_state
    {
      LED_STATE_NO = -1,
      LED_STATE_INACTIVE,
      LED_STATE_ACTIVE
    };

  led_indicator (led_state initial_state = LED_STATE_INACTIVE,
                 QWidget *parent = 0);

public slots:

  void set_state (led_state state);

private:

  QString style_sheet (const QColor& col);

};

OCTAVE_END_NAMESPACE(octave)

#endif
