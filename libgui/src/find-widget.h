////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2024 The Octave Project Developers
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

// ---------------------------------------------------------------------
//
//  This class provides a horizontal layout containing a small
//  find dialog.
//
//  Usage:
//    1. Create widget and include it in the widget's layout
//    2. Connect to the signals
//         find_widget::find_signal (const QString& text, bool backward)
//         find_widget::find_incremental_signal (const QString& text)
//       for searching forward or backward for "text" and for providing
//       an incremental search while typing
//    3. Provide suitable actions in the slots of the signals above
//    4. Call the find widget's methods notice_settings when settings
//       are updated and save_settings when settings are saved
//    5. Other methods or slots are
//        - file_widget::activate_find (): slot for hiding/showing
//                                         find widget
//        - file_widget::text (): get current search text
//        - file_widget::set_text (const QString& text): set search text
//
// ---------------------------------------------------------------------

#if ! defined (octave_find_widget_h)
#define octave_find_widget_h 1

#include <QHBoxLayout>
#include <QLabel>
#include <QLineEdit>
#include <QShortcut>
#include <QString>
#include <QToolButton>
#include <QWidget>

OCTAVE_BEGIN_NAMESPACE(octave)

class find_widget : public QWidget
{
  Q_OBJECT

public:

  // x_button: provide a close button for the widget or not
  find_widget (bool x_button = true, QWidget *parent = nullptr);
  ~find_widget () = default;

  QString text (void);
  void set_text (const QString& text);
  void notice_settings (void);
  void save_settings (void);

public Q_SLOTS:

  void activate_find (void);

private Q_SLOTS:

  void find (void);
  void find_backward (void);
  void find_incremental (void);

Q_SIGNALS:

  void find_signal (const QString&, bool);
  void find_incremental_signal (const QString&);

protected:


private:

  bool m_is_closeable;

  QLineEdit *m_find_line_edit;

  QShortcut *m_findnext_shortcut;
  QShortcut *m_findprev_shortcut;

};

OCTAVE_END_NAMESPACE(octave)

#endif
