////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2011-2023 The Octave Project Developers
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

#include <QScreen>

// This header is only needed for the new terminal widget.
#if defined (HAVE_QSCINTILLA)
#  include "command-widget.h"
#endif

// This header is only needed for the old terminal widget.
#include "QTerminal.h"

#include "gui-preferences-cs.h"
#include "gui-preferences-global.h"

#include "octave-qobject.h"
#include "terminal-dock-widget.h"

OCTAVE_BEGIN_NAMESPACE(octave)

terminal_dock_widget::terminal_dock_widget (QWidget *p,
                                            base_qobject& oct_qobj)
: octave_dock_widget ("TerminalDockWidget", p, oct_qobj),
  m_experimental_terminal_widget (oct_qobj.experimental_terminal_widget ())
{
  // FIXME: we could do this in a better way, but improving it doesn't
  // matter much if we will eventually be removing the old terminal.
  if (m_experimental_terminal_widget)
    {
#if defined (HAVE_QSCINTILLA)
      command_widget *widget = new command_widget (oct_qobj, this);
      console *con = widget->get_console ();

      connect (this, &terminal_dock_widget::settings_changed,
               widget, &command_widget::notice_settings);

      connect (this, &terminal_dock_widget::update_prompt_signal,
               widget, &command_widget::update_prompt);

      connect (this, &terminal_dock_widget::interpreter_output_signal,
               widget, &command_widget::insert_interpreter_output);

      connect (this, &terminal_dock_widget::execute_command_signal,
               con, &console::execute_command);

      connect (this, &terminal_dock_widget::new_command_line_signal,
               con, &console::new_command_line);

      m_terminal = widget;
#endif
    }
  else
    {
      QTerminal *widget = QTerminal::create (oct_qobj, this);

      connect (this, &terminal_dock_widget::settings_changed,
               widget, &QTerminal::notice_settings);

      // Connect the visibility signal to the terminal for
      // dis-/enabling timers.
      connect (this, &terminal_dock_widget::visibilityChanged,
               widget, &QTerminal::handle_visibility_changed);

      m_terminal = widget;
    }

  m_terminal->setObjectName ("OctaveTerminal");
  m_terminal->setFocusPolicy (Qt::StrongFocus);

  set_title (tr ("Command Window"));

  setWidget (m_terminal);
  setFocusProxy (m_terminal);

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

  int max_x = QGuiApplication::primaryScreen ()->availableGeometry ().width ();
  int max_y = QGuiApplication::primaryScreen ()->availableGeometry ().height ();

  if (win_x > max_x)
    win_x = max_x;
  if (win_y > max_y)
    win_y = max_y;

  setGeometry (0, 0, win_x, win_y);

  if (! p)
    make_window ();
}

bool terminal_dock_widget::has_focus (void) const
{
  QWidget *w = widget ();
  return w->hasFocus ();
}

QTerminal * terminal_dock_widget::get_qterminal (void)
{
  return (m_experimental_terminal_widget
          ? nullptr : dynamic_cast<QTerminal *> (m_terminal));
}

#if defined (HAVE_QSCINTILLA)
command_widget * terminal_dock_widget::get_command_widget (void)
{
  return (m_experimental_terminal_widget
          ? dynamic_cast<command_widget *> (m_terminal) : nullptr);
}
#endif

void terminal_dock_widget::notice_settings (const gui_settings *settings)
{
  emit settings_changed (settings);
}

void terminal_dock_widget::init_command_prompt ()
{
  if (m_experimental_terminal_widget)
    {
#if defined (HAVE_QSCINTILLA)
      command_widget *cmd = get_command_widget ();
      if (cmd)
        cmd->init_command_prompt ();
#endif
    }
}

OCTAVE_END_NAMESPACE(octave)
