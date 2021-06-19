////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2021 The Octave Project Developers
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

#if ! defined (octave_command_widget_h)
#define octave_command_widget_h 1

#include <QWidget>

#include "octave-qobject.h"
#include "gui-settings.h"

class QLabel;
class QLineEdit;
class QStrung;
class QTextBrowser;

namespace octave
{
  class base_qobject;

  class command_widget : public QWidget
  {
    Q_OBJECT

  public:

    command_widget (base_qobject& oct_qobj, QWidget *p);

  signals:

    void clear_line_edit (void);

    void interpreter_pause (void);
    void interpreter_resume (void);
    void interpreter_stop (void);

    void interpreter_event (const fcn_callback& fcn);
    void interpreter_event (const meth_callback& meth);

  public slots:

    void notice_settings (const gui_settings *settings);

  protected slots:

    void accept_input_line (void);

    void insert_interpreter_output (const QString& msg);

  private:

    bool m_incomplete_parse;
    QLabel *m_prompt;
    QLineEdit *m_line_edit;
    QTextBrowser *m_output_display;
    QColor m_input_color;
  };
}

#endif
