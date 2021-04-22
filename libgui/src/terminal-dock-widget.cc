////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2011-2021 The Octave Project Developers
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

#include <QDesktopWidget>

// This header is only needed for the new terminal widget.
#include "command-widget.h"

// This header is only needed for the old terminal widget.
#include "QTerminal.h"

#include "gui-preferences-cs.h"
#include "gui-preferences-global.h"

#include "octave-qobject.h"
#include "terminal-dock-widget.h"

namespace octave
{
  terminal_dock_widget::terminal_dock_widget (QWidget *p,
                                              base_qobject& oct_qobj)
    : octave_dock_widget ("TerminalDockWidget", p, oct_qobj),
      m_experimental_terminal_widget (oct_qobj.experimental_terminal_widget ())
  {
    // FIXME: we could do this in a better way, but improving it doesn't
    // matter much if we will eventually be removing the old terminal.
    if (m_experimental_terminal_widget)
      m_terminal = new command_widget (oct_qobj, this);
    else
      m_terminal = QTerminal::create (oct_qobj, this, p);

    m_terminal->setObjectName ("OctaveTerminal");
    m_terminal->setFocusPolicy (Qt::StrongFocus);

    setWindowIcon (QIcon (":/actions/icons/logo.png"));
    set_title (tr ("Command Window"));

    setWidget (m_terminal);
    setFocusProxy (m_terminal);

    connect (p, SIGNAL (settings_changed (const gui_settings *)),
             m_terminal, SLOT (notice_settings (const gui_settings *)));

    if (m_experimental_terminal_widget)
      {
        // Any interpreter_event signal from the terminal widget is
        // handled the same as for the parent terminal dock widget.

        connect (m_terminal, SIGNAL (interpreter_event (const fcn_callback&)),
                 this, SIGNAL (interpreter_event (const fcn_callback&)));

        connect (m_terminal, SIGNAL (interpreter_event (const meth_callback&)),
                 this, SIGNAL (interpreter_event (const meth_callback&)));
      }
    else
      {
        // Connect the interrupt signal (emitted by Ctrl-C)
        connect (m_terminal, SIGNAL (interrupt_signal (void)),
                 &oct_qobj, SLOT (interpreter_interrupt (void)));

        // Connect the visibility signal to the terminal for
        // dis-/enabling timers.
        connect (this, SIGNAL (visibilityChanged (bool)),
                 m_terminal, SLOT (handle_visibility_changed (bool)));
      }

    // Chose a reasonable size at startup in order to avoid truncated
    // startup messages
    resource_manager& rmgr = m_octave_qobj.get_resource_manager ();
    gui_settings *settings = rmgr.get_settings ();

    QFont font = QFont ();
    font.setStyleHint (QFont::TypeWriter);
    QString default_font = settings->value (global_mono_font).toString ();
    font.setFamily
      (settings->value (cs_font.key, default_font).toString ());
    font.setPointSize
      (settings->value (cs_font_size).toInt ());

    QFontMetrics metrics(font);

    int win_x =  metrics.maxWidth()*80;
    int win_y =  metrics.height()*25;

    int max_x = QApplication::desktop ()->screenGeometry (this).width ();
    int max_y = QApplication::desktop ()->screenGeometry (this).height ();

    if (win_x > max_x)
      win_x = max_x;
    if (win_y > max_y)
      win_y = max_y;

    setGeometry (0, 0, win_x, win_y);
  }

  bool terminal_dock_widget::has_focus (void) const
  {
    QWidget *w = widget ();
    return w->hasFocus ();
  }

  void terminal_dock_widget::interpreter_output (const QString& msg)
  {
    if (m_experimental_terminal_widget)
      emit interpreter_output_signal (msg);
  }

  void terminal_dock_widget::update_prompt (const QString& prompt)
  {
    if (m_experimental_terminal_widget)
      emit update_prompt_signal (prompt);
  }

}
