/*
 *
 * Copyright (C) 2007, 2008, 2009 John P. Swensen
 *
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
//#ifdef HAVE_CONFIG_H
#undef PACKAGE_BUGREPORT
#undef PACKAGE_NAME
#undef PACKAGE_STRING
#undef PACKAGE_TARNAME
#undef PACKAGE_VERSION
#undef PACKAGE_URL
#include <octave/config.h>
//#endif

#include <cassert>
#include <cstdlib>
#include <cstring>
#include <ctime>

#include <fstream>
#include <iostream>

#ifdef HAVE_UNISTD_H
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#include <unistd.h>
#endif
#include <sys/time.h>

#include <sys/time.h>

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

#include "octave/toplev.h"
#include "octave/procstream.h"
//#include "octave/prog-args.h"
#include "octave/sighandlers.h"
#include "octave/debug.h"
#include "octave/sysdep.h"
#include "octave/ov.h"
#include "octave/unwind-prot.h"
#include "octave/utils.h"
#include "octave/variables.h"

#include <readline/readline.h>
#undef PACKAGE_BUGREPORT
#undef PACKAGE_NAME
#undef PACKAGE_STRING
#undef PACKAGE_TARNAME
#undef PACKAGE_VERSION
#undef PACKAGE_URL
#include "octave/config.h"
#include "octave/Range.h"
#include "octave/ov.h"
class octave_value;
class octave_value_list;

#include <cstdio>
#include <string>
#include <vector>
#include <QMutex>
#include <QList>
#include <QString>
#include <QVector>

typedef symbol_table::symbol_record SymbolRecord;

/**
  * \class OctaveLink
  * Manages a link to an octave instance.
  */
class OctaveLink
{
public:
    static OctaveLink *instance() { return &m_singleton; }
    static int readlineEventHook(void);

    /**
     * Enumeration used to identify breakpoint actions
     */
    enum BreakPointAction
    {
        None,
        StepInto,
        StepOver,
        StepOut,
        Continue,
        Break
    };

    /**
     * Structure used to store breakpoint info.
     *
     * Notes: used for add, remove, list operations, as well as for the BreakpointReached event.
     */
    typedef struct BreakPoint
    {
        /**
        * The full path and filename where the breakpoint resides.
        */
        std::string fileName;

        /**
        * The line number where the breakpoint resides.
        * In the future, -1 can indicate an existing but disabled breakpoint.  This
        * assumes that no one will ever have an M file longer than 2Million lines.
        */
        int lineNumber;
    } BreakPoint;

    typedef struct RequestedVariable
    {
        std::string name;
        octave_value ov;
    } RequestedVariable;

    // Functions used to access data form the client side.
    /** Debugging related methods. */

    /** TODO: Describe. */
    std::vector<BreakPoint> breakPointList(int& status);

    /** TODO: Describe. */
    std::vector<BreakPoint> reachedBreakpoint();

    /** TODO: Describe. */
    bool isBreakpointReached(int& status);

    /** TODO: Describe. */
    int addBreakpoint(BreakPoint bp_info);

    /** TODO: Describe. */
    int	removeBreakpoint(BreakPoint bp_info);

    /** TODO: Describe. */
    int	modifyBreakpoint(BreakPoint old_bp_info, BreakPoint new_bp_info);

    /** TODO: Describe. */
    int	setBreakpointAction(BreakPointAction action);

    /** Variable related methods. */
    QList<SymbolRecord> variableInfoList(void);

    /** TODO: Describe. */
    QList<RequestedVariable> requestedVariables(void);

    /** TODO: Describe. */
    int	setRequestedVariableNames(QList<QString> variableNames);

    /**
      * History related methods.
      */
    string_vector getHistoryList(void);

    // FUNCTIONS USED TO ACCESS DATA FROM THE OCTAVE SERVER SIDE

    // NOTE: THIS IMPLIES THAT THESE ARE ONLY CALLED FROM
    // OCTAVE DURING A TIME IN WHICH THINGS ARE KNOWN TO
    // BE "THREAD-SAFE".  PROPOSED LOCATIONS:
    //   src/toplev.cc - main_loop() at the end of the do...while
    //   src/pt-bp.h   - MAYBE_DO_BREAKPOINT just prior to the do_keyboard
    // Most of these will call octave API functions to "pull" the data, rather
    // than having octave pass in the data.  This will help make changes
    // exlusive to this class if/when the Octave API changes.
    /**
     * Calls all the appropriate functions that follow to update Octave
     * according to the data sent from the client in a thread-safe manner.
     *
     * Algorithm:
     *   Acquire lock
     *   process_breakpoint_add_remove_modify
     *   set_current_breakpoint
     *   set_breakpoint_list
     *   ...
     *   Release lock
     */
    int processOctaveServerData(void);

    /** Debugging related methods. */
    /** TODO: Describe. */
    int setBreakPointList(void);

    /** TODO: Describe. */
    // duplicate of process_breakpoint_action or helper function???
    int setCurrentBreakpoint(std::string filename, int line_number);

    /** TODO: Describe. */
    int processBreakpointAndRemoveModify(void);

    /** TODO: Describe. */
    int process_breakpoint_action(void);

    /** Variable related methods. */
    /** Retrieves all variables from Octave. */
    void retrieveVariables(void);

    /** TODO: Describe. */
    int processRequestedVariables(void);

    /** History related methods. */
    /** TODO: Describe. */
    int setHistoryList(void);

private:
    OctaveLink();
    ~OctaveLink();

    /** Mutex variable used to protect access to internal class data. */
    QMutex m_internalAccessMutex;

    std::vector<BreakPoint> m_currentBreakpoints;
    std::vector<BreakPoint> m_reachedBreakpoints;
    std::vector<BreakPoint> m_addedBreakpoints;
    std::vector<BreakPoint> m_removedBreakpoints;
    std::vector<BreakPoint> m_modifiedBreakpointsOld;
    std::vector<BreakPoint> m_modifiedBreakpointsNew;
    BreakPointAction m_breakPointAction;

    /** Variable related member variables. */
    QList<SymbolRecord> m_variableSymbolTableList;
    QList<QString> m_variablesRequestList;

    // NOTE: Create an overloaded operator<< for octave_value to do the
    // flattening.  This will allow us to append easily to an ostringstream
    // for output.
    QList<RequestedVariable> m_requestedVariables;

    /** History related member variables. */
    string_vector m_historyList;
    int m_previousHistoryLength;
    static OctaveLink m_singleton;
};
#endif // OCTAVELINK_H

