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

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <QGroupBox>
#include <QHBoxLayout>
#include <QLabel>
#include <QLineEdit>
#include <QPushButton>
#include <QTextBrowser>
#include <QVBoxLayout>

#include "command-widget.h"

#include "cmd-edit.h"
#include "event-manager.h"
#include "input.h"
#include "interpreter.h"

namespace octave
{
  // FIXME: this class needs a different name and should probably be
  // defined in a separate file.

  command_widget::command_widget (base_qobject& oct_qobj, QWidget *p)
    : QWidget (p), m_incomplete_parse (false),
      m_prompt (new QLabel ("", this)),
      m_line_edit (new QLineEdit (this)),
      m_output_display (new QTextBrowser (this))
  {
    QPushButton *pause_button = new QPushButton (tr("Pause"), this);
    QPushButton *stop_button = new QPushButton (tr("Stop"), this);
    QPushButton *resume_button = new QPushButton (tr("Continue"), this);

    QGroupBox *input_group_box = new QGroupBox (tr("Command Input"));
    QHBoxLayout *input_layout = new QHBoxLayout;
    input_layout->addWidget (m_prompt);
    input_layout->addWidget (m_line_edit);
    input_layout->addWidget (pause_button);
    input_layout->addWidget (stop_button);
    input_layout->addWidget (resume_button);
    input_group_box->setLayout (input_layout);

    QGroupBox *output_group_box = new QGroupBox (tr("Command Output"));
    QHBoxLayout *output_layout = new QHBoxLayout ();
    output_layout->addWidget (m_output_display);
    output_group_box->setLayout (output_layout);

    QVBoxLayout *main_layout = new QVBoxLayout ();
    main_layout->addWidget (input_group_box);
    main_layout->addWidget (output_group_box);

    setLayout (main_layout);

    connect (m_line_edit, SIGNAL (returnPressed (void)),
             this, SLOT (accept_input_line (void)));

    connect (this, SIGNAL (clear_line_edit (void)),
             m_line_edit, SLOT (clear (void)));

    connect (pause_button, SIGNAL (clicked (void)),
             &oct_qobj, SLOT (interpreter_pause (void)));

    connect (stop_button, SIGNAL (clicked (void)),
             &oct_qobj, SLOT (interpreter_stop (void)));

    connect (resume_button, SIGNAL (clicked (void)),
             &oct_qobj, SLOT (interpreter_resume (void)));

    connect (p, SIGNAL (update_prompt_signal (const QString&)),
             m_prompt, SLOT (setText (const QString&)));

    connect (p, SIGNAL (interpreter_output_signal (const QString&)),
             this, SLOT (insert_interpreter_output (const QString&)));
  }

  void command_widget::insert_interpreter_output (const QString& msg)
  {
    QTextCursor cursor = m_output_display->textCursor ();

    cursor.insertText (msg);

    m_output_display->setTextCursor (cursor);
  }

  void command_widget::accept_input_line (void)
  {
    QTextCursor cursor = m_output_display->textCursor ();

    QString input_line = m_line_edit->text ();

    if (! m_incomplete_parse)
      cursor.insertHtml ("<b>[in]:</b> ");
    cursor.insertText (input_line);
    cursor.insertHtml ("<br>");

    m_output_display->setTextCursor (cursor);

    emit interpreter_event
      ([=] (interpreter& interp)
       {
         // INTERPRETER THREAD

         interp.parse_and_execute (input_line.toStdString () + "\n",
                                   m_incomplete_parse);

         event_manager& evmgr = interp.get_event_manager ();
         input_system& input_sys = interp.get_input_system ();

         std::string prompt
           = m_incomplete_parse ? input_sys.PS2 () : input_sys.PS1 ();

         evmgr.update_prompt (command_editor::decode_prompt_string (prompt));
       });

    emit clear_line_edit ();
  }
}
