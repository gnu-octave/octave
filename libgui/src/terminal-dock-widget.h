////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2011-2022 The Octave Project Developers
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

#if ! defined (octave_terminal_dock_widget_h)
#define octave_terminal_dock_widget_h 1

#include <QString>

#include "octave-dock-widget.h"

class QTerminal;

namespace octave
{
  class command_widget;
  class base_qobject;

  class terminal_dock_widget : public octave_dock_widget
  {
    Q_OBJECT

  public:

    terminal_dock_widget (QWidget *parent, base_qobject& oct_qobj);

    ~terminal_dock_widget (void) = default;

    bool has_focus (void) const;

    void init_command_prompt ();

    // FIXME: The next two functions could be eliminated (or combined)
    // if we had a common interface for the old and new terminal
    // widgets.

    // Only valid if using the old terminal widget.
    QTerminal * get_qterminal (void);

    // Only valid if using the new terminal widget.
    command_widget * get_command_widget (void);

  signals:

    void settings_changed (const gui_settings *settings);

    // Note: UPDATE_PROMPT_SIGNAL and INTERPRETER_OUTPUT_SIGNAL are
    // currently only used by the new experimental terminal widget.

    void update_prompt_signal (const QString&);

    void interpreter_output_signal (const QString&);

  public slots:

    void notice_settings (const gui_settings *settings);

    // Note: INTERPRETER_OUTPUT and UPDATE_PROMPT are currently only
    // used by the new experimental terminal widget.

    void interpreter_output (const QString&);

    void update_prompt (const QString&);

  private:

    bool m_experimental_terminal_widget;

    // FIXME!!!  Maybe my_term should just be derived from QTerminal?
    QWidget *m_terminal;
  };
}

#endif
