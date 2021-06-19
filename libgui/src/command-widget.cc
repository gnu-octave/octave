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
#include "gui-preferences-cs.h"
#include "gui-preferences-global.h"
#include "gui-utils.h"
#include "input.h"
#include "interpreter.h"

namespace octave
{
  command_widget::command_widget (base_qobject&, QWidget *p)
    : QWidget (p), m_incomplete_parse (false),
      m_prompt (new QLabel ("", this)),
      m_line_edit (new QLineEdit (this)),
      m_output_display (new QTextBrowser (this)),
      m_input_color (QColor ())
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
    main_layout->addWidget (output_group_box);
    main_layout->addWidget (input_group_box);

    setLayout (main_layout);

    setFocusProxy (m_line_edit);

    connect (m_line_edit, &QLineEdit::returnPressed,
             this, &command_widget::accept_input_line);

    connect (this, &command_widget::clear_line_edit,
             m_line_edit, &QLineEdit::clear);

    connect (pause_button, &QPushButton::clicked,
             this, &command_widget::interpreter_pause);

    connect (resume_button, &QPushButton::clicked,
             this, &command_widget::interpreter_resume);

    connect (stop_button, &QPushButton::clicked,
             this, &command_widget::interpreter_stop);

    connect (p, SIGNAL (update_prompt_signal (const QString&)),
             m_prompt, SLOT (setText (const QSTring&)));

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

    QString style;
    if (! m_incomplete_parse)
      {
        style = QString ("<div style=\"color:%1; font-weight:bold;\">[in]:</div> ")
                .arg (m_input_color.name ());
        m_output_display->insertHtml (style);
      }
    style = QString ("<div style=\"color:%1\">%2</div><br>")
            .arg (m_input_color.name ()).arg (input_line);
    m_output_display->insertHtml (style);

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
       });

    emit clear_line_edit ();
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

    m_line_edit->setFont (term_font);
    m_output_display->setFont (term_font);

    // Colors
    int mode = settings->value (cs_color_mode).toInt ();
    QColor fgc = settings->color_value (cs_colors[0], mode);
    QColor bgc = settings->color_value (cs_colors[1], mode);

    m_output_display->setStyleSheet (QString ("color: %1; background-color:%2;")
                                     .arg (fgc.name ()).arg (bgc.name ()));
    m_line_edit->setStyleSheet (QString ("color: %1; background-color:%2;")
                                .arg (fgc.name ()).arg (bgc.name ()));

    m_input_color = interpolate_color (fgc, bgc, 0.75, 0.5);
  }

}
