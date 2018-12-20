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

// Author: Torsten <mttl@mailbox.org>

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
    Q_OBJECT;

  public:

    enum cmd_upd {
      CMD_UPD_NO        = 0,
      CMD_UPD_WORKSPACE = 1
    };

    //! Command using built in functions requiring interpreter, no return values
    /*! Command calling a built in function, which uses an interpreter and does
     *  not have any return values
     * @param Ff Pointer to the builtin function
     * @param argin Input parameters for Ff as octave value list (default empty)
     * @param update A memeber of cmd_upd for possibly required updates after
     *               the executing Ff
     */
    octave_cmd_builtin (
          octave_value_list (*Ff) (octave::interpreter&, const octave_value_list&, int),
          octave_value_list argin = ovl (), cmd_upd update = CMD_UPD_NO)
      : octave_cmd (), m_callback_fi (Ff), m_callback_f (nullptr),
        m_argin (argin), m_nargout (0), m_argout_receiver (nullptr),
        m_argout_handler (nullptr), m_update (update)
    {  }

    //! Command using built in functions not requiring interpreter, no return values
    octave_cmd_builtin (
          octave_value_list (*Ff) (const octave_value_list&, int),
          octave_value_list argin = ovl (), cmd_upd update = CMD_UPD_NO)
      : octave_cmd (), m_callback_fi (nullptr), m_callback_f (Ff),
        m_argin (argin), m_nargout (0), m_argout_receiver (nullptr),
        m_argout_handler (nullptr), m_update (update)
    {  }

    //! Command using built in functions requiring interpreter, with return values
    /*! Command calling a built in function, which uses an interpreter and
     *  has return values
     * @param Ff Pointer to the builtin function
     * @param argin Input parameters for Ff as octave value list
     * @param argout Number of output values
     * @param argout_receiver Receiver of the the signal containing the return values
     * @param argout_handler  Slot for the signal containing the return values
     * @param update A member of cmd_upd for possibly required updates after
     *               the executing Ff
     * argout_receiver and argout_handler live in the GUI thread. Using a thread
     * crossing signal/slot mechanism, the GUI thread is not blocked if the
     * worker thread is busy and can not execute the desired command immediately.
     */
    octave_cmd_builtin (
          octave_value_list (*Ff) (octave::interpreter&, const octave_value_list&, int),
          octave_value_list argin, int nargout, QObject *argout_receiver,
          const char *argout_handler = nullptr, cmd_upd update = CMD_UPD_NO)
      : octave_cmd (), m_callback_fi (Ff), m_callback_f (nullptr),
        m_argin (argin), m_nargout (nargout), m_argout_receiver (argout_receiver),
        m_argout_handler (argout_handler), m_update (update)
    {
      init_cmd_retval ();
    }

    //! Command using built in functions not requiring interpreter, with return values
    octave_cmd_builtin (
          octave_value_list (*Ff) (const octave_value_list&, int),
          octave_value_list argin, int nargout, QObject *argout_receiver,
          const char *argout_handler = nullptr, cmd_upd update = CMD_UPD_NO)
      : octave_cmd (), m_callback_fi (nullptr), m_callback_f (Ff),
        m_argin (argin), m_nargout (nargout), m_argout_receiver (argout_receiver),
        m_argout_handler (argout_handler), m_update (update)
    {
      init_cmd_retval ();
    }

    void execute (interpreter& interp);

  signals:

    //! Signal for sending the return values to the GUI thread
    void argout_signal (const octave_value_list&);

  protected:

    octave_value_list (*m_callback_fi) (octave::interpreter&,
                                        const octave_value_list&, int);
    octave_value_list (*m_callback_f) (const octave_value_list&, int);

    octave_value_list m_argin;

    int m_nargout;
    QObject *m_argout_receiver;
    const char *m_argout_handler;

    cmd_upd m_update;

  private:

    //! Internal method connecting the signal for the return values
    /*! Internal method for connecting the signal for later sending the return
     * values to the caller. The connection has to be queued ensuring that
     * the receiver's slot is actually executed in the GUI thread
     */
    void init_cmd_retval (void);
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
