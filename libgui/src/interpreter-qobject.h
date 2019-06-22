/*

Copyright (C) 2013-2019 John W. Eaton
Copyright (C) 2011-2019 Jacob Dawid

This file is part of Octave.

Octave is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<https://www.gnu.org/licenses/>.

*/

#if ! defined (octave_interpreter_qobject_h)
#define octave_interpreter_qobject_h 1

#include <QObject>

namespace octave
{
  class qt_application;

  class interpreter_qobject : public QObject
  {
    Q_OBJECT

  public:

    interpreter_qobject (qt_application& app_context);

    ~interpreter_qobject (void) = default;

  signals:

    void octave_ready_signal (void);
    void octave_finished_signal (int);

  public slots:

    //! Initialize and execute the octave interpreter.

    void execute (void);

  private:

    qt_application& m_app_context;
  };
}

#endif
