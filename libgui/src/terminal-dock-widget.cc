////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2011-2024 The Octave Project Developers
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

#include <QGuiApplication>
#include <QScreen>

// This header is only needed for the new terminal widget.
#if defined (HAVE_QSCINTILLA)
#  include "command-widget.h"
#endif

// This header is only needed for the old terminal widget.
#include "QTerminal.h"

#include "gui-preferences-cs.h"
#include "gui-preferences-global.h"
#include "gui-preferences-sc.h"
#include "gui-settings.h"

#include "terminal-dock-widget.h"

OCTAVE_BEGIN_NAMESPACE(octave)

terminal_dock_widget::terminal_dock_widget (QWidget *p,
                                            bool experimental_terminal_widget)
  : octave_dock_widget ("TerminalDockWidget", p),
    m_experimental_terminal_widget (experimental_terminal_widget)
{
  init_control_d_shortcut_behavior ();

  // FIXME: we could do this in a better way, but improving it doesn't
  // matter much if we will eventually be removing the old terminal.
  if (m_experimental_terminal_widget)
    {
#if defined (HAVE_QSCINTILLA)
      command_widget *widget = new command_widget (this);
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
      QTerminal *widget = QTerminal::create (this);

      connect (this, &terminal_dock_widget::settings_changed,
               widget, &QTerminal::notice_settings);

      // Connect the visibility signal to the terminal for
      // dis-/enabling timers.
      connect (this, &terminal_dock_widget::visibilityChanged,
               widget, &QTerminal::handle_visibility_changed);

      connect (widget, qOverload<const fcn_callback&> (&QTerminal::interpreter_event),
               this, qOverload<const fcn_callback&> (&terminal_dock_widget::interpreter_event));

      connect (widget, qOverload<const meth_callback&> (&QTerminal::interpreter_event),
               this, qOverload<const meth_callback&> (&terminal_dock_widget::interpreter_event));

      m_terminal = widget;
    }

  m_terminal->setObjectName ("OctaveTerminal");
  m_terminal->setFocusPolicy (Qt::StrongFocus);

  set_title (tr ("Command Window"));

  setWidget (m_terminal);
  setFocusProxy (m_terminal);

  // Chose a reasonable size at startup in order to avoid truncated
  // startup messages

  gui_settings settings;

  QFont font = QFont ();
  font.setStyleHint (QFont::TypeWriter);
  QString default_font = settings.string_value (global_mono_font);
  font.setFamily
    (settings.value (cs_font.settings_key (), default_font).toString ());
  font.setPointSize
    (settings.int_value (cs_font_size));

  QFontMetrics metrics (font);

  int win_x =  metrics.maxWidth ()*80;
  int win_y =  metrics.height ()*25;

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

bool
terminal_dock_widget::has_focus () const
{
  QWidget *w = widget ();
  return w->hasFocus ();
}

QTerminal *
terminal_dock_widget::get_qterminal ()
{
  return (m_experimental_terminal_widget
          ? nullptr : dynamic_cast<QTerminal *> (m_terminal));
}

#if defined (HAVE_QSCINTILLA)
command_widget *
terminal_dock_widget::get_command_widget ()
{
  return (m_experimental_terminal_widget
          ? dynamic_cast<command_widget *> (m_terminal) : nullptr);
}
#endif

void
terminal_dock_widget::notice_settings ()
{
  Q_EMIT settings_changed ();
}

void
terminal_dock_widget::init_command_prompt ()
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

void
terminal_dock_widget::init_control_d_shortcut_behavior ()
{
  gui_settings settings;

  // Reset use of Ctrl-D.  Do this before the call to beginGroup
  // because sc_main_ctrld.key already begins with the sc_group
  // prefix.
  settings.setValue (sc_main_ctrld.settings_key (), false);

  settings.beginGroup (sc_group);
  const QStringList shortcut_settings_keys = settings.allKeys ();
  settings.endGroup ();

  for (const auto& settings_key : shortcut_settings_keys)
    {
      // Check whether Ctrl+D is used from main window, i.e. is a
      // global shortcut.

      QString section = get_shortcut_section (settings_key);

      if (section.startsWith ("main_"))
        {
          sc_pref scpref = all_shortcut_preferences::value (settings_key);

          QKeySequence actual = QKeySequence (settings.sc_value (scpref));

          if (actual == QKeySequence (Qt::ControlModifier | Qt::Key_D))
            {
              settings.setValue (sc_main_ctrld.settings_key (), true);
              break;
            }
        }
    }
}

OCTAVE_END_NAMESPACE(octave)
