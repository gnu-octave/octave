/*

Copyright (C) 2014-2018 Torsten

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

// Author: Torsten <ttl@justmail.de>

#if ! defined (octave_octave_cmd_h)
#define octave_octave_cmd_h 1

#include <QSemaphore>
#include <QMutex>
#include <QPointer>
#include <QString>
#include <QFileInfo>

#include "octave-qt-link.h"
#include "ovl.h"


namespace octave
{
  class interpreter;

  class octave_cmd : public QObject
  {
    Q_OBJECT;

  public:

    octave_cmd (void) : QObject () {  };

    virtual ~octave_cmd (void) = default;

    virtual void execute (interpreter&) { }
  };

  class octave_cmd_exec : public octave_cmd
  {
  public:

    octave_cmd_exec (const QString& cmd) : octave_cmd () { m_cmd = cmd; }

    void execute (interpreter& interp);

  protected:

    QString m_cmd;
  };

  class octave_cmd_eval : public octave_cmd
  {
  public:

    octave_cmd_eval (const QFileInfo& info) : octave_cmd () { m_info = info; }

    void execute (interpreter& interp);

  protected:

    QFileInfo m_info;
  };

  class octave_cmd_builtin : public octave_cmd
  {
    public:

    enum cmd_upd {
      CMD_UPD_NO        = 0,
      CMD_UPD_WORKSPACE = 1
    };

    // C'tor for Fxxx requiring the interpreter
    octave_cmd_builtin (
          octave_value_list (*Ff) (octave::interpreter&,
                                   const octave_value_list&, int),
          octave_value_list argin = ovl (), int nargout = 0,
          cmd_upd update = CMD_UPD_NO, octave_qt_link *oct_qt_link = nullptr)
      : octave_cmd (), m_callback_fi (Ff), m_callback_f (nullptr),
        m_argin (argin), m_nargout (nargout), m_update (update),
        m_octave_qt_link (oct_qt_link)
    { };

    // C'tor for Fxxx not requiring the interpreter
    octave_cmd_builtin (
          octave_value_list (*Ff) (const octave_value_list&, int),
          octave_value_list argin = ovl (), int nargout = 0,
          cmd_upd update = CMD_UPD_NO, octave_qt_link *oct_qt_link = nullptr)
      : octave_cmd (), m_callback_fi (nullptr), m_callback_f (Ff),
        m_argin (argin), m_nargout (nargout), m_update (update),
        m_octave_qt_link (oct_qt_link)
    { };

    void execute (interpreter& interp);

  protected:

    octave_value_list (*m_callback_fi) (octave::interpreter&,
                                        const octave_value_list&, int);
    octave_value_list (*m_callback_f) (const octave_value_list&, int);
    octave_value_list m_argin;
    int m_nargout;
    cmd_upd m_update;
    octave_qt_link *m_octave_qt_link;
  };

  class octave_cmd_debug : public octave_cmd_exec
  {
  public:

    octave_cmd_debug (const QString& cmd, bool suppress_location)
      : octave_cmd_exec (cmd), m_suppress_dbg_location (suppress_location) { }

    void execute (interpreter& interp);

  protected:

    bool m_suppress_dbg_location;
  };

  //! Queuing octave commands from the GUI for the worker thread.

  class octave_command_queue : QObject
  {
    Q_OBJECT;

  public:

    octave_command_queue (void)
      : QObject (), m_queue (QList<QPointer<octave_cmd>> ()), m_processing (1),
        m_queue_mutex ()
    { }

    ~octave_command_queue (void) = default;

    //! Adds a new octave command to the command queue.
    //!
    //! @param cmd The octave command to be queued.

    void add_cmd (octave_cmd *cmd);

    //! Callback routine for executing the command by the worker thread.

    void execute_command_callback (void);

  private:

    QList<QPointer<octave_cmd>> m_queue;
    QSemaphore m_processing;
    QMutex m_queue_mutex;
  };
}

#endif
