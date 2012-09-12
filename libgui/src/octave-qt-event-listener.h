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

#ifndef OCTAVEQTEVENTLISTENER_H
#define OCTAVEQTEVENTLISTENER_H

#include <QObject>
#include <QString>
#include "octave-event-listener.h"

class octave_qt_event_listener
    : public QObject, public octave_event_listener
{
    Q_OBJECT
  public:
    octave_qt_event_listener (QObject *parent = 0);

    void current_directory_has_changed (const std::string& directory);
    void about_to_exit ();

    void entered_debug_mode ();
    void quit_debug_mode ();

  signals:
    void current_directory_has_changed_signal (const QString& directory);
    void entered_debug_mode_signal ();
    void quit_debug_mode_signal ();
};

#endif // OCTAVEQTEVENTLISTENER_H
