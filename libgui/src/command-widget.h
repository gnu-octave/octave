////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2021-2023 The Octave Project Developers
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

#include <Qsci/qsciscintilla.h>

#include "octave-qobject.h"
#include "gui-settings.h"

class QsciScintilla;

OCTAVE_BEGIN_NAMESPACE(octave)

class base_qobject;
class command_widget;

class console : public QsciScintilla
{
  Q_OBJECT

public:

  console (command_widget *p, base_qobject& oct_qobj);

public slots:

  void cursor_position_changed (int line, int col);

  void text_changed (void);

  void move_cursor_to_end (void);

  void new_command_line (const QString& command = QString ());

  void execute_command (const QString& command);

protected:

  void keyPressEvent (QKeyEvent *e);

private:

  void append_string (const QString& string);

  void accept_command_line (void);

  int m_command_position;
  int m_cursor_position;
  bool m_text_changed;
  command_widget *m_command_widget;
  QString m_last_key_string;

};

class command_widget : public QWidget
{
  Q_OBJECT

public:

  command_widget (base_qobject& oct_qobj, QWidget *p);

  console * get_console ( ) { return m_console; };

  void init_command_prompt ();

  QString prompt (void);

signals:

  void clear_line_edit (void);

  void interpreter_pause (void);
  void interpreter_resume (void);
  void interpreter_stop (void);

  void interpreter_event (const fcn_callback& fcn);
  void interpreter_event (const meth_callback& meth);

  void new_command_line_signal (const QString& command = QString ());

public slots:

  void process_input_line (const QString& input_line);

  void update_prompt (const QString& prompt);

  void insert_interpreter_output (const QString& msg);

  void notice_settings (const gui_settings *settings);

private:

  bool m_incomplete_parse;
  QString m_prompt;
  console *m_console;
};

OCTAVE_END_NAMESPACE(octave)

#endif
