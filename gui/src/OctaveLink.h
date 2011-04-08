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

#if !defined (octave_server_h)
#define octave_server_h 1

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


typedef int status_t;

/**
 * Enumeration used to identify breakpoint actions
 */
typedef enum bp_action_enum
{
  BP_ACTION_NONE	= 0,
  BP_ACTION_STEP_INTO	= 1,
  BP_ACTION_STEP_OVER	= 2,
  BP_ACTION_STEP_OUT    = 3,
  BP_ACTION_CONTINUE	= 3,
  BP_ACTION_BREAK	= 4,
} bp_action_t;

/**
 * Structure used to store breakpoint info.
 *
 * Notes: used for add, remove, list operations, as well as for the BreakpointReached event.
 */
typedef struct bp_info_struct
{
  /**
   * The full path and filename where the breakpoint resides.
   */
  std::string filename;   

  /**
   * The line number where the breakpoint resides.
   * In the future, -1 can indicate an existing but disabled breakpoint.  This
   * assumes that no one will ever have an M file longer than 2Million lines.
   */
  int line_number;
} bp_info_t;

/**
 * Structure used to store variable information similar to that returned by
 * the 'whos' function.
 */
typedef struct variable_info_struct variable_info_t;
typedef struct variable_info_struct
{
  /**
   * The name of the variable
   */
  std::string variable_name;

  /**
   * The dimensional size of the variable.
   */
  std::vector<int> size;

  /**
   * The size of the variable in bytes.
   */
  unsigned long long byte_size;

  /**
   * The name of the variable type.
   */
  std::string type_name;
  
  friend int operator==(const variable_info_t& left,
                        const variable_info_t& right)
  {
    return (left.variable_name==right.variable_name) &&
         (left.size==right.size) &&
         (left.byte_size==right.byte_size) &&
         (left.type_name==right.type_name);
  }
  
} variable_info_t;

typedef struct requested_variable_struct
{
  std::string name;
  octave_value ov;
} requested_variable_t;

class octave_server
{
  private:
    /**
     * Mutex variable used to protect access to internal class data.
     */
    pthread_mutex_t server_mutex;
    
    
    /**
     * Mutex variable used to protect access to octave internals on asynchronous requests.
     * 
     * Notes: This is necessary for asynchronous requests like detailed variable information
     * in a debugger mouse-over, inspection of matrix variables by double-clicking in the 
     * main window, etc.
     */
    pthread_mutex_t octave_lock_mutex;
	  
	  
    /***********************************************************************
     * DEBUGGING RELATED VARIABLE
     **********************************************************************/
    std::vector<bp_info_t> current_breakpoints;
    std::vector<bp_info_t> breakpoint_reached;
    std::vector<bp_info_t> added_breakpoints;
    std::vector<bp_info_t> removed_breakpoints;
    std::vector<bp_info_t> modify_breakpoints_old;
    std::vector<bp_info_t> modify_breakpoints_new;
    bp_action_t 	   bp_action;
   
    /***********************************************************************
     * VARIABLE INTERROGATION RELATED VARIABLES
     **********************************************************************/
    std::vector<variable_info_t> variable_symtab_list;
    std::vector<std::string>     variables_request_list;

    // NOTE: Create an overloaded operator<< for octave_value to do the
    // flattening.  This will allow us to append easily to an ostringstream
    // for output.
    std::vector<requested_variable_t>    requested_variables;    
    
    /***********************************************************************
     * HISTORY RELATED VARIABLES
     **********************************************************************/
    int 			 prevHistLen;
    string_vector                history_list;

    bool                         is_processing_server_data;
    
  public:

    octave_server();
    ~octave_server();

    bool isProcessing(void) {return is_processing_server_data;};

    /*************************************************************************
     *************************************************************************
     * FUNCTIONS USED TO ACCESS DATA FROM THE CLIENT SIDE
     *************************************************************************
     *************************************************************************/

    /***********************************************************************
     * DEBUGGING RELATED FUNCTIONS
     **********************************************************************/ 
    std::vector<bp_info_t> get_breakpoint_list(int& status);
    bool                   is_breakpoint_reached(int& status);
    std::vector<bp_info_t> get_breakpoint_reached();    
    status_t 		   add_breakpoint( bp_info_t bp_info );
    status_t		   remove_breakpoint( bp_info_t bp_info );
    status_t		   modify_breakpoint( bp_info_t old_bp_info, bp_info_t new_bp_info );
    status_t		   set_breakpoint_action( bp_action_t action );
   
    /***********************************************************************
     * VARIABLES RELATED FUNCTIONS
     **********************************************************************/
    std::vector<variable_info_t>	get_variable_info_list(void);
    std::vector<requested_variable_t>  	get_requested_variables(void);
    status_t				set_requested_variables_names( std::vector<std::string> variable_names );

    /***********************************************************************
     * HISTORY RELATED FUNCTIONS
     **********************************************************************/
    string_vector	get_history_list(void);

    /*************************************************************************
     *************************************************************************
     * FUNCTIONS USED TO ACCESS DATA FROM THE OCTAVE SERVER SIDE
     *
     * NOTE: THIS IMPLIES THAT THESE ARE ONLY CALLED FROM
     * OCTAVE DURING A TIME IN WHICH THINGS ARE KNOWN TO
     * BE "THREAD-SAFE".  PROPOSED LOCATIONS:
     *     src/toplev.cc - main_loop() at the end of the do...while
     *     src/pt-bp.h   - MAYBE_DO_BREAKPOINT just prior to the do_keyboard
     * Most of these will call octave API functions to "pull" the data, rather
     * than having octave pass in the data.  This will help make changes 
     * exlusive to this class if/when the Octave API changes.
     *************************************************************************
     *************************************************************************/
    
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
    status_t process_octave_server_data(void);
 
    /***********************************************************************
     * DEBUGGING RELATED FUNCTIONS
     **********************************************************************/   
    status_t set_breakpoint_list(void);
    status_t set_current_breakpoint(std::string filename, int line_number); // duplicate of process_breakpoint_action or helper function???
    status_t process_breakpoint_add_remove_modify(void);
    status_t process_breakpoint_action(void);

    /***********************************************************************
     * VARIABLES INTERROGATION RELATED FUNCTIONS
     **********************************************************************/
    status_t set_variable_info_list(void);
    status_t process_requested_variables(void);
    
    /***********************************************************************
     * HISTORY RELATED FUNCTIONS
     **********************************************************************/    
    status_t set_history_list(void);

};

int server_rl_event_hook_function(void);
bool server_rl_is_processing(void);

extern octave_server oct_octave_server;

#endif // octave_server_h

