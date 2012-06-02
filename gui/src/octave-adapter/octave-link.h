/* OctaveGUI - A graphical user interface for Octave
 * Copyright (C) 2011 John P. Swensen, Jacob Dawid (jacob.dawid@googlemail.com)
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */

#ifndef OCTAVELINK_H
#define OCTAVELINK_H

// Octave includes
#undef PACKAGE_BUGREPORT
#undef PACKAGE_NAME
#undef PACKAGE_STRING
#undef PACKAGE_TARNAME
#undef PACKAGE_VERSION
#undef PACKAGE_URL
#include "octave/config.h"
#include "octave/cmd-edit.h"
#include "octave/error.h"
#include "octave/file-io.h"
#include "octave/input.h"
#include "octave/lex.h"
#include "octave/load-path.h"
#include "octave/octave.h"
#include "octave/oct-hist.h"
#include "octave/oct-map.h"
#include "octave/oct-obj.h"
#include "octave/ops.h"
#include "octave/ov.h"
#include "octave/ov-usr-fcn.h"
#include "octave/symtab.h"
#include "octave/pt.h"
#include "octave/pt-eval.h"
#include "octave/config.h"
#include "octave/Range.h"
#include "octave/toplev.h"
#include "octave/procstream.h"
#include "octave/sighandlers.h"
#include "octave/debug.h"
#include "octave/sysdep.h"
#include "octave/ov.h"
#include "octave/unwind-prot.h"
#include "octave/utils.h"
#include "octave/variables.h"

// Standard includes
#include <iostream>
#include <string>
#include <vector>
#include <readline/readline.h>

// Qt includes
#include <QMutexLocker>
#include <QMutex>
#include <QFileInfo>
#include <QList>
#include <QString>
#include <QStringList>
#include <QVector>
#include <QSemaphore>
#include <QObject>
#include <QStringListModel>
#include <QTimer>

#include "workspace-model.h"
#include "octave-main-thread.h"
#include "symbol-information.h"

/**
  * \class OctaveLink
  * \brief Provides threadsafe access to octave.
  * \author Jacob Dawid
  * This class is a wrapper around octave and provides threadsafety by
  * buffering access operations to octave and executing them in the readline
  * even hook, which lives in the octave thread.
  */
class octave_link : public QObject
{
  Q_OBJECT
public:
  /** Provides a way to access the unique octave_link object. */
  static octave_link *
  instance () { return &_singleton; }

  /** Starts octave. */
  void launch_octave ();

  /** Attempts to close octave. */
  void terminate_octave ();

  /** Returns the current history model. */
  QStringListModel *get_history_model ();

  /** Returns the current workspace model. */
  workspace_model *get_workspace_model ();

  /** Triggers an update of the history model. */
  void trigger_update_history_model ();

  /** Updates the current working directory. */
  void update_current_working_directory ();

  /** Acquires the symbol information. You need to acquire that before
    * actually accessing it. Make sure that you release it properly in order
    * to avoid deadlocks. */
  void acquire_symbol_information ();

  /** Releases access to the symbol information. */
  void release_symbol_information ();

  /** Update symbol information from octave's symboltable. */
  void build_symbol_information ();

  /** Provides acces to the current symbol information.
    * WARNING: Always acquire the symbol information before actually
    * using it and make sure you release it properly afterwards.
    */
  const QList <symbol_information>& get_symbol_information () const;

signals:
  /** Emitted, whenever the working directory of octave changed. */
  void working_directory_changed (QString directory);

private:
  /** Singleton. */
  octave_link ();
  ~octave_link ();

  /** Stores the current history_model. */
  QStringListModel *_history_model;

  /** Stores the current workspace model. */
  workspace_model *_workspace_model;

  /** Thread running octave_main. */
  octave_main_thread *_octave_main_thread;

  /** Timer for periodically updating the workspace model from the current
    * symbol information. */
  QTimer _update_workspace_model_timer;

  /** Semaphore to lock access to the symbol information. */
  QSemaphore *_symbol_information_semaphore;

  /** Stores the current symbol information. */
  QList <symbol_information> _symbol_information;

  /** Stores the last known current working directory of octave. */
  QString _current_working_directory;

  /** Unique instance. Singelton! */
  static octave_link _singleton;
};
#endif // OCTAVELINK_H
