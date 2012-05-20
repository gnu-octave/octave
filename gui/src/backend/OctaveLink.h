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

#include "WorkspaceModel.h"
#include "OctaveCallbackThread.h"
#include "OctaveMainThread.h"

/**
  * \class OctaveLink
  * Manages a link to an octave instance.
  */
class OctaveLink:public QObject
{
  Q_OBJECT
public:
  static OctaveLink *
  instance ()
  {
    return &m_singleton;
  }

  void launchOctave ();
  void terminateOctave ();
  QStringListModel *historyModel ();
  WorkspaceModel *workspaceModel ();

  void triggerUpdateHistoryModel ();
  void triggerUpdateSymbolTable ();

private:
  OctaveLink ();
  ~OctaveLink ();

  //QSemaphore * m_symbolTableSemaphore;
  //QList < symbol_table::symbol_record > m_symbolTableBuffer;

  QStringListModel *m_historyModel;
  WorkspaceModel *m_workspaceModel;

  // Threads for running octave and managing the data interaction.
  OctaveMainThread *m_octaveMainThread;
  OctaveCallbackThread *m_octaveCallbackThread;

  static OctaveLink m_singleton;
};
#endif // OCTAVELINK_H
