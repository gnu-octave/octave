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

#if defined (HAVE_PTHREAD_H)
    #include <pthread.h>
#elif defined (__WIN32__)
    #include <windows.h>
        typedef CRITICAL_SECTION pthread_mutex_t;
        #define pthread_mutex_init(x,a) InitializeCriticalSection(x)
        inline int pthread_mutex_trylock(pthread_mutex_t *x)
        {
            EnterCriticalSection(x);
            return 0;
        }
        #define pthread_mutex_lock(x) pthread_mutex_trylock(x)
        #define pthread_mutex_unlock(x) LeaveCriticalSection(x)
        typedef HANDLE pthread_t;
#endif

/**
  * \class OctaveLink
  * Manages a link to an octave instance.
  */
class OctaveLink
{
public:
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

    typedef struct VariableMetaData
    {
        /** The name of the variable. */
        std::string variableName;

        /** The dimensional size of the variable. */
        std::vector<int> dimensionalSize;

        /** The size of the variable in bytes. */
        unsigned long long byteSize;

        /** The name of the variable type. */
        std::string typeName;

        friend int operator==(const VariableMetaData& left,
                              const VariableMetaData& right) {
            return (left.variableName == right.variableName) &&
                   (left.dimensionalSize == right.dimensionalSize) &&
                   (left.byteSize == right.byteSize) &&
                   (left.typeName == right.typeName);
        }
    } VariableMetaData;

    OctaveLink();
    ~OctaveLink();

    bool isProcessing(void) { return m_isProcessingServerData; }

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
    std::vector<VariableMetaData> variableInfoList(void);

    /** TODO: Describe. */
    std::vector<RequestedVariable> requestedVariables(void);

    /** TODO: Describe. */
    int	setRequestedVariableNames(std::vector<std::string> variableNames);

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
    /** TODO: Describe. */
    int setVariableInfoList(void);

    /** TODO: Describe. */
    int processRequestedVariables(void);

    /** History related methods. */
    /** TODO: Describe. */
    int setHistoryList(void);

private:
    /** Mutex variable used to protect access to internal class data. */
    pthread_mutex_t m_serverMutex;

    /**
     * Mutex variable used to protect access to octave internals on asynchronous requests.
     *
     * Notes: This is necessary for asynchronous requests like detailed variable information
     * in a debugger mouse-over, inspection of matrix variables by double-clicking in the
     * main window, etc.
     */
    pthread_mutex_t m_octaveLockMutex;

    std::vector<BreakPoint> m_currentBreakpoints;
    std::vector<BreakPoint> m_reachedBreakpoints;
    std::vector<BreakPoint> m_addedBreakpoints;
    std::vector<BreakPoint> m_removedBreakpoints;
    std::vector<BreakPoint> m_modifiedBreakpointsOld;
    std::vector<BreakPoint> m_modifiedBreakpointsNew;
    BreakPointAction m_breakPointAction;

    /** Variable related member variables. */
    std::vector<VariableMetaData> m_variableSymbolTableList;
    std::vector<std::string> m_variablesRequestList;

    // NOTE: Create an overloaded operator<< for octave_value to do the
    // flattening.  This will allow us to append easily to an ostringstream
    // for output.
    std::vector<RequestedVariable> m_requestedVariables;

    /** History related member variables. */
    string_vector m_historyList;
    int m_previousHistoryLength;

    bool m_isProcessingServerData;
};

int server_rl_event_hook_function(void);
bool server_rl_is_processing(void);

extern OctaveLink oct_octave_server;

#endif // OCTAVELINK_H

