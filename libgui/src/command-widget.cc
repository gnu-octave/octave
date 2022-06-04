////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2021-2022 The Octave Project Developers
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

#include <QGroupBox>
#include <QHBoxLayout>
#include <QLabel>
#include <QLineEdit>
#include <QPushButton>
#include <QTextEdit>
#include <QTextBlock>
#include <QVBoxLayout>

#include "command-widget.h"

#include "cmd-edit.h"
#include "event-manager.h"
#include "gui-preferences-cs.h"
#include "gui-preferences-global.h"
#include "gui-utils.h"
#include "input.h"
#include "interpreter.h"

namespace octave
{
  command_widget::command_widget (base_qobject&, QWidget *p)
    : QWidget (p), m_incomplete_parse (false),
      m_prompt (QString ()),
      m_console (new console (this))
  {
    QPushButton *pause_button = new QPushButton (tr("Pause"), this);
    QPushButton *stop_button = new QPushButton (tr("Stop"), this);
    QPushButton *resume_button = new QPushButton (tr("Continue"), this);

    QGroupBox *input_group_box = new QGroupBox ();
    QHBoxLayout *input_layout = new QHBoxLayout;
    input_layout->addWidget (pause_button);
    input_layout->addWidget (stop_button);
    input_layout->addWidget (resume_button);
    input_group_box->setLayout (input_layout);

    QVBoxLayout *main_layout = new QVBoxLayout ();
    main_layout->addWidget (m_console);
    main_layout->addWidget (input_group_box);

    setLayout (main_layout);

    setFocusProxy (m_console);

    connect (pause_button, &QPushButton::clicked,
             this, &command_widget::interpreter_pause);

    connect (resume_button, &QPushButton::clicked,
             this, &command_widget::interpreter_resume);

    connect (stop_button, &QPushButton::clicked,
             this, &command_widget::interpreter_stop);

    connect (this, &command_widget::new_command_line_signal,
             m_console, &console::new_command_line);

    insert_interpreter_output ("\n\n    Welcome to Octave\n\n");

  }

  void command_widget::init_command_prompt ()
  {
    emit interpreter_event
      ([=] (interpreter& interp)
       {
         // INTERPRETER THREAD

         event_manager& evmgr = interp.get_event_manager ();
         input_system& input_sys = interp.get_input_system ();
         std::string prompt = input_sys.PS1 ();
         evmgr.update_prompt (command_editor::decode_prompt_string (prompt));

         emit new_command_line_signal ();
       });
  }

  void command_widget::update_prompt (const QString& prompt)
  {
    m_prompt = prompt;
  }

  QString command_widget::prompt ()
  {
    return m_prompt;
  }

  void command_widget::insert_interpreter_output (const QString& msg)
  {
    QTextCursor cursor = m_console->textCursor ();

    cursor.insertText (msg);

    m_console->setTextCursor (cursor);
  }

  void command_widget::process_input_line (const QString& input_line)
  {
    emit interpreter_event
      ([=] (interpreter& interp)
       {
         // INTERPRETER THREAD

         interp.parse_and_execute (input_line.toStdString (),
                                   m_incomplete_parse);

         event_manager& evmgr = interp.get_event_manager ();
         input_system& input_sys = interp.get_input_system ();

         std::string prompt
           = m_incomplete_parse ? input_sys.PS2 () : input_sys.PS1 ();

         evmgr.update_prompt (command_editor::decode_prompt_string (prompt));

         emit new_command_line_signal ();
       });

  }

  void command_widget::notice_settings (const gui_settings *settings)
  {
    // Set terminal font:
    QFont term_font = QFont ();
    term_font.setStyleHint (QFont::TypeWriter);
    QString default_font = settings->value (global_mono_font).toString ();
    term_font.setFamily
      (settings->value (cs_font.key, default_font).toString ());
    term_font.setPointSize
      (settings->value (cs_font_size).toInt ());

    m_console->setFont (term_font);

    // Colors
    int mode = settings->value (cs_color_mode).toInt ();
    QColor fgc = settings->color_value (cs_colors[0], mode);
    QColor bgc = settings->color_value (cs_colors[1], mode);

    m_console->setStyleSheet (QString ("color: %1; background-color:%2;")
                                     .arg (fgc.name ()).arg (bgc.name ()));
  }


  // The console itself using QTextEdit with QTextBlock and QTextCursor.
  // This implementation is based on the basic concept of "qpconsole" as
  // proposed by user "DerManu" in the Qt-forum thread
  // https://forum.qt.io/topic/28765/command-terminal-using-qtextedit

  console::console (command_widget *p)
    : QTextEdit (p),
      m_command_block_number (-1),
      m_command_widget (p),
      m_document (new QTextDocument (this))
  {
    setDocument (m_document);
  }

  // Prepare a new command line with the current prompt
  void console::new_command_line (const QString& command)
  {
    QTextCursor cursor (m_document->lastBlock ());

    if (! m_document->lastBlock ().text ().isEmpty ())
      {
        cursor.movePosition (QTextCursor::EndOfBlock);
        cursor.insertBlock ();
      }

    cursor.insertText (m_command_widget->prompt () + command);
    setTextCursor (cursor);
  }

  // Accept the current command line (or block)
  void console::accept_command_line ()
  {
    QString input_line = m_document->lastBlock().text();

    if (input_line.startsWith (m_command_widget->prompt ()))
      input_line.remove(0, m_command_widget->prompt ().length ());

    input_line = input_line.trimmed ();
  
    append_block ();

    if (input_line.isEmpty ())
      new_command_line ();
    else
      m_command_widget->process_input_line (input_line);
  }

  // Append a block to the document
  void console::append_block ()
  {
    QTextCursor cursor (m_document->lastBlock ());
    cursor.movePosition (QTextCursor::EndOfBlock);
    cursor.insertBlock ();
    setTextCursor (cursor);
  }

  // Execute a command
  void console::execute_command (const QString& command)
  {
    if (command.trimmed ().isEmpty ())
      return;

    new_command_line (command);
    accept_command_line ();
  }

  // Re-implement key event
  void console::keyPressEvent (QKeyEvent *e)
  {
    if (e->key () == Qt::Key_Return)
      // On "return", accept the current command line
      accept_command_line ();
    else
      // Otherwise, process the expected event
      QTextEdit::keyPressEvent(e);                          
  }

}
