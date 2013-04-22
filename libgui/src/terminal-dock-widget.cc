/*

Copyright (C) 2013 John W. Eaton
Copyright (C) 2011-2012 Jacob Dawid

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "terminal-dock-widget.h"

terminal_dock_widget::terminal_dock_widget (QWidget *p)
  : octave_dock_widget (p), terminal (new QTerminal (p))
{
  terminal->setObjectName ("OctaveTerminal");
  terminal->setFocusPolicy (Qt::StrongFocus);

  setObjectName ("TerminalDockWidget");
  setWindowIcon (QIcon(":/actions/icons/logo.png"));
  setWindowTitle (tr ("Command Window"));
  setWidget (terminal);

  connect (parent (), SIGNAL (settings_changed (const QSettings *)),
           this, SLOT (notice_settings (const QSettings *)));

  connect (this, SIGNAL (visibilityChanged (bool)),
           this, SLOT (handle_visibility (bool)));

  connect (parent (), SIGNAL (relay_command_signal (const QString&)),
           this, SLOT (relay_command (const QString&)));

  // Forward signals to QTerminal widget.

  connect (this, SIGNAL (notice_settings_signal (const QSettings *)),
           terminal, SLOT (notice_settings (const QSettings *)));

  connect (this, SIGNAL (relay_command_signal (const QString&)),
           terminal, SLOT (relay_command (const QString&)));

  connect (this, SIGNAL (copyClipboard_signal (void)),
           terminal, SLOT (copyClipboard (void)));

  connect (this, SIGNAL (pasteClipboard_signal (void)),
           terminal, SLOT (pasteClipboard (void)));
}

void
terminal_dock_widget::notice_settings (const QSettings *settings)
{
  emit notice_settings_signal (settings);
}

void
terminal_dock_widget::relay_command (const QString& command)
{
  emit relay_command_signal (command);
}

void
terminal_dock_widget::copyClipboard (void)
{
  emit copyClipboard_signal ();
}

void
terminal_dock_widget::pasteClipboard (void)
{
  emit pasteClipboard_signal ();
}

void
terminal_dock_widget::focus (void)
{
  octave_dock_widget::focus ();

  QWidget *w = widget ();

  w->setFocus ();
  w->activateWindow ();
  w->raise ();
}
