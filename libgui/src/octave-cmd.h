/*

Copyright (C) 2014 Torsten

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

// Author: Torsten <ttl@justmail.de>

#if ! defined (octave_octave_cmd_h)
#define octave_octave_cmd_h 1

#include <QSemaphore>
#include <QMutex>
#include <QString>
#include <QFileInfo>

#include "octave-qt-link.h"

class octave_cmd
{
public:

  octave_cmd () { };
  virtual ~octave_cmd () { };

  virtual void execute () { };
};


// ---------------------------------------------------------------------
//  class octave_cmd_exec

class octave_cmd_exec : public octave_cmd
{
public:

  octave_cmd_exec (const QString& cmd) : octave_cmd () { _cmd = cmd; };
  void execute ();

protected:

  QString _cmd;
};


// ---------------------------------------------------------------------
//  class octave_cmd_eval

class octave_cmd_eval : public octave_cmd
{
public:

  octave_cmd_eval (const QFileInfo& info) : octave_cmd () { _info = info; };
  void execute ();

protected:

  QFileInfo _info;
};


// ---------------------------------------------------------------------
//  class octave_cmd_debug

class octave_cmd_debug : public octave_cmd_exec
{
public:

  octave_cmd_debug (const QString& cmd, bool suppress_location)
    : octave_cmd_exec (cmd)
  {
    _suppress_dbg_location = suppress_location;
  };

  void execute ();

protected:

  bool _suppress_dbg_location;
};


/**
 * @class octave_command_queue
 *
 * Queuing commands from the GUI for the worker thread
 */
// ---------------------------------------------------------------------
//  class octave_command_queue: queue of octave commands

class octave_command_queue : QObject
{
  Q_OBJECT;

public:

  octave_command_queue (void) : QObject (),
      _queue (QList<octave_cmd *> ()),
      _processing (1),
      _queue_mutex () { };
  ~octave_command_queue (void) { };

  /**
   * Adds a new octave command to the command queue.
   * @param cmd The octave command to be queued
   */
  void add_cmd (octave_cmd *cmd);
  /**
   * Callback routine for executing the command by the worker thread
   */
  void execute_command_callback (void);

private:

  QList<octave_cmd *>  _queue;
  QSemaphore   _processing;
  QMutex       _queue_mutex;
};

#endif
