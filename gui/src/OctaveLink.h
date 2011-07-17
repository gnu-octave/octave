/*
 *
 * Copyright (C) 2007, 2008, 2009 John P. Swensen
 *
 * OctaveGUI - A graphical user interface for Octave
 * Copyright (C) 2011 Jacob Dawid
 * jacob.dawid@googlemail.com
 * This file is as a part of OctaveDE.
 *
 * Octave is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 2, or (at your option) any
 * later version.
 *
 * Octave is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 * for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Octave; see the file COPYING.  If not, write to the Free
 * Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301, USA.
 *
 * */

#ifndef OCTAVELINK_H
#define OCTAVELINK_H

// Octave includes
#undef PACKAGE_BUGREPORT
#undef PACKAGE_NAME
#undef PACKAGE_STRING
#undef PACKAGE_TARNAME
#undef PACKAGE_VERSION
#undef PACKAGE_URL
#include <octave/config.h>
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
#include <QVector>
#include <QSemaphore>
#include <QObject>

typedef symbol_table::symbol_record SymbolRecord;
typedef octave_value OctaveValue;

/**
  * \class OctaveLink
  * Manages a link to an octave instance.
  */
class OctaveLink : QObject
{
    Q_OBJECT
public:
    static OctaveLink *instance() { return &m_singleton; }
    static int readlineEventHook(void);
    static QString octaveValueAsQString(OctaveValue octaveValue);

    /**
      * Returns a copy of the current symbol table buffer.
      * \return Copy of the current symbol table buffer.
      */
    QList<SymbolRecord> currentSymbolTable();

    /**
      * Returns a copy of the current history buffer.
      * \return Copy of the current history buffer.
      */
    string_vector currentHistory();

    void processOctaveServerData();

    /**
      * Updates the current symbol table with new data
      * from octave.
      */
    void fetchSymbolTable();

    /**
      * Updates the current history buffer with new data
      * from octave.
      */
    void fetchHistory();

signals:
    void symbolTableChanged();
    void historyChanged();

private:
    OctaveLink();
    ~OctaveLink();

    /** Variable related member variables. */
    QSemaphore *m_symbolTableSemaphore;
    QList<SymbolRecord> m_symbolTableBuffer;

    /** History related member variables. */
    QSemaphore *m_historySemaphore;
    string_vector m_historyBuffer;
    int m_previousHistoryLength;

    static OctaveLink m_singleton;
};
#endif // OCTAVELINK_H

