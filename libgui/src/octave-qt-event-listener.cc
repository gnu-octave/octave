/*

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

#include "octave-qt-event-listener.h"
#include <QApplication>

octave_qt_event_listener::octave_qt_event_listener (QObject *p)
  : QObject (p), octave_event_listener ()
{
}

void
octave_qt_event_listener::current_directory_has_changed (const std::string& directory)
{
  emit current_directory_has_changed_signal
    (QString::fromLocal8Bit (directory.data (), directory.size ()));
}

void
octave_qt_event_listener::about_to_exit ()
{
  qApp->quit ();
}

void
octave_qt_event_listener::entered_debug_mode ()
{ emit entered_debug_mode_signal (); }

void
octave_qt_event_listener::quit_debug_mode ()
{ emit quit_debug_mode_signal (); }

