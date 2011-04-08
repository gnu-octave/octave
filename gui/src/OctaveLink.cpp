/*

Copyright (C) 2007,2008,2009 John P. Swensen

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, write to the Free
Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.

*/

// Born July 13, 2007.

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

#include "OctaveLink.h"

#include <QFileInfo>

octave_server oct_octave_server;


static octave_user_code *
get_user_code (const std::string& fname = std::string ())
{
  octave_user_code *dbg_fcn = 0;

  if (fname.empty ())
    dbg_fcn = octave_call_stack::caller_user_code ();
  else
    {
      octave_value fcn = symbol_table::find_function (fname);

      if (fcn.is_defined () && fcn.is_user_code ())
	dbg_fcn = fcn.user_code_value ();
    }

  return dbg_fcn;
}

//*************************************************************************
int server_rl_event_hook_function(void)
{
  static int rl_event_count = 0;
  rl_event_count++;

  //if (rl_event_count%10 == 0)
  //  octave_stdout << "rl_event_count:" << rl_event_count << std::endl;


  // TODO: No need to run too quickly.  The documentation says it will run
  // at most 10 times per second.  This may be too fast and we will need to
  // artificially slow it down somehow.  Not sure at this time how.
  oct_octave_server.process_octave_server_data();

  return 0;
}

bool server_rl_is_processing(void)
{
  return oct_octave_server.isProcessing();
}

//*************************************************************************
octave_server::octave_server()
{
  // Create the mutexes 
  pthread_mutex_init(&server_mutex,NULL);
  pthread_mutex_init(&octave_lock_mutex,NULL);

  prevHistLen = 0;

  is_processing_server_data = false;
}


//*************************************************************************
octave_server::~octave_server()
{

}

/*******************************************************************************
 *******************************************************************************
 * CLIENT SIDE FUNCTIONS
 *******************************************************************************
 *******************************************************************************/

//*************************************************************************
std::vector<variable_info_t> octave_server::get_variable_info_list(void)
{
  // Acquire the mutex
  if( pthread_mutex_trylock( &server_mutex ) != 0 )
    return std::vector<variable_info_t>();

  // Copy the list of variable information
  std::vector<variable_info_t> retval( variable_symtab_list.size() );
  std::copy( variable_symtab_list.begin(), variable_symtab_list.end(), retval.begin() );
  variable_symtab_list = std::vector<variable_info_t>();

  // Release the mutex
  pthread_mutex_unlock( &server_mutex );

  return retval;
}


//*************************************************************************
std::vector<requested_variable_t> octave_server::get_requested_variables(void)
{
  // Acquire the mutex
  if( pthread_mutex_trylock( &server_mutex ) != 0 )
    return std::vector<requested_variable_t>();

  // Copy the list of requested variables
  std::vector<requested_variable_t> retval( requested_variables.size() );
  std::copy( requested_variables.begin(), requested_variables.end(), retval.begin() );
  requested_variables = std::vector<requested_variable_t>();
  
  // Release the mutex
  pthread_mutex_unlock( &server_mutex );

  return retval;
}


//*************************************************************************
status_t octave_server::set_requested_variables_names( std::vector<std::string> variables_names )
{
  // Acquire the mutex
  if( pthread_mutex_trylock( &server_mutex ) != 0 )
    return -1;

  // Set the list of requested variables
  variables_request_list = std::vector<std::string>( variables_names.size() );
  std::copy( variables_names.begin(), variables_names.end(), variables_request_list.begin() );

  // Release the mutex
  pthread_mutex_unlock( &server_mutex );

  return 0;
}


//*************************************************************************
string_vector octave_server::get_history_list(void)
{
  // Acquire mutex
  if( pthread_mutex_trylock( &server_mutex ) != 0 )
    return string_vector();

  // Copy the list of command history items
  string_vector retval( history_list );
  history_list = string_vector();

  // Release mutex
  pthread_mutex_unlock( &server_mutex );

  return retval;
}

std::vector<bp_info_t> octave_server::get_breakpoint_list(int& status)
{
  // Acquire the mutex
  if( pthread_mutex_trylock( &server_mutex ) != 0 )
  {
    status = -1;
    return std::vector<bp_info_t>();
  }

  // Copy the list of variable information
  std::vector<bp_info_t> retval (current_breakpoints.size());
  std::copy( current_breakpoints.begin(), current_breakpoints.end(), retval.begin() );

  // Release the mutex
  pthread_mutex_unlock( &server_mutex );

  status = 0;
  return retval;
}

bool octave_server::is_breakpoint_reached (int& status)
{
  // Acquire the mutex
  if( pthread_mutex_trylock( &server_mutex ) != 0 )
  {
    status = -1;
    return false;
  }

  // Copy the list of variable information
  bool retval = (breakpoint_reached.size()>0);

  //if (retval)
  //  octave_stdout << "Breakpoint reached" << std::endl;

  // Release the mutex
  pthread_mutex_unlock( &server_mutex );

  status = 0;
  return retval;
}


std::vector<bp_info_t> octave_server::get_breakpoint_reached()
{
  // Acquire the mutex
  if( pthread_mutex_trylock( &server_mutex ) != 0 )
    return std::vector<bp_info_t>();

  // Copy the list of variable information
  std::vector<bp_info_t> retval (breakpoint_reached.size());
  std::copy (breakpoint_reached.begin(), breakpoint_reached.end(), retval.begin() );

  //if (breakpoint_reached.size()>0)
  //  octave_stdout << "Breakpoint reached" << std::endl;

  // Release the mutex
  pthread_mutex_unlock( &server_mutex );

  return retval;
}

status_t octave_server::add_breakpoint( bp_info_t bp_info )
{
  // Acquire the mutex
  if( pthread_mutex_trylock( &server_mutex ) != 0 )
    return -1;

  // Copy the list of variable information
  added_breakpoints.push_back (bp_info);

  // Release the mutex
  pthread_mutex_unlock( &server_mutex );

  return 0;
}

status_t octave_server::remove_breakpoint( bp_info_t bp_info )
{
  // Acquire the mutex
  if( pthread_mutex_trylock( &server_mutex ) != 0 )
    return -1;

  // Copy the list of variable information
  removed_breakpoints.push_back (bp_info);

  // Release the mutex
  pthread_mutex_unlock( &server_mutex );

  return 0;
}

/*
    status_t		   modify_breakpoint( bp_info_t old_bp_info, bp_info_t new_bp_info );
*/

status_t octave_server::set_breakpoint_action (bp_action_t action)
{
  // Acquire the mutex
  if( pthread_mutex_trylock( &server_mutex ) != 0 )
    return -1;

  bp_action = action;
  
  // Release the mutex
  pthread_mutex_unlock( &server_mutex );

  return 0;
}


/*******************************************************************************
 *******************************************************************************
 * SERVER SIDE FUNCTIONS
 *******************************************************************************
 *******************************************************************************/

//*************************************************************************
status_t octave_server::process_octave_server_data(void)
{
  struct timeval start, stop;
#ifndef __WIN32__
  gettimeofday(&start, NULL);
#endif

  // Acquire mutex
  if( pthread_mutex_lock( &server_mutex ) != 0 )
  {
    octave_stdout << "Error acquiring the octave_server data lock mutex" << std::endl;
    return -1;
  }
  is_processing_server_data = true;
  
  process_breakpoint_action();
  process_breakpoint_add_remove_modify();
  process_requested_variables();
  set_variable_info_list();
  set_history_list();
  set_breakpoint_list();

  // Release mutex
  pthread_mutex_unlock( &server_mutex );
  is_processing_server_data = false;

#ifndef __WIN32__
  gettimeofday(&stop, NULL);
  double elapsed = stop.tv_sec - start.tv_sec + 1E-6 * (stop.tv_usec - start.tv_usec);
  //octave_stdout << "SERVER ELAPSED: " << elapsed << std::endl;
#endif


  return 0;
}


//*************************************************************************
status_t octave_server::set_variable_info_list( void )
{
  static std::vector<variable_info_t> lastVars;
  std::vector<variable_info_t> currVars;


  std::list<symbol_table::symbol_record> lvars = symbol_table::all_variables();
  std::list<symbol_table::symbol_record>::iterator it;

  for ( it = lvars.begin() ; it != lvars.end() ; it++ )
  {
    octave_value varval( it->varval() );
    std::string nm = (*it).name();

    dim_vector dims = varval.dims ();

    variable_info_t tempVar;
    tempVar.variable_name = it->name();
    tempVar.size.push_back( varval.rows() );
    tempVar.size.push_back( varval.columns() );
    tempVar.byte_size = varval.byte_size();
    tempVar.type_name = varval.type_name();

    currVars.push_back(tempVar);
  }

  if ( lastVars != currVars )
  {
    lastVars = currVars;
    
    // Copy currVars into octave_server::variable_symtab_list
    variable_symtab_list = std::vector<variable_info_t>( currVars.size() );

    std::copy( currVars.begin(), currVars.end(), variable_symtab_list.begin() );
  }

  
  return 0;
}


//*************************************************************************
status_t octave_server::process_requested_variables( void )
{
  /*

  // Clear the list of existing requested variables
  requested_variables = std::vector<requested_variable_t>();
  
  //// Get the list of variables and copy them into requested_variables vector
  // XXX FIXME XXX Should separate argv to lists with and without dots.
  if ( top_level_sym_tab != NULL )
  {
    if ( global_command == 0 )
    {
      Array<symbol_record *> xsymbols = top_level_sym_tab->symbol_list ( string_vector(), 0xFF, SYMTAB_ALL_SCOPES);
      Array<symbol_record *> xsubsymbols = top_level_sym_tab->subsymbol_list ( string_vector(), 0xFF, SYMTAB_ALL_SCOPES);

      int sym_len = xsymbols.length (), subsym_len = xsubsymbols.length (),
                                                     len = sym_len + subsym_len;

      Array<symbol_record *> symbols (len);

      if (len > 0)
      {
        //size_t bytes = 0;
        //size_t elements = 0;

        int i;

        // Joining symbolic tables.
        for (i = 0; i < sym_len; i++)
          symbols(i) = xsymbols(i);

        for (i = 0; i < subsym_len; i++)
          symbols(i+sym_len) = xsubsymbols(i);
      }

      for (int j = 0; j < len; j++)
      {
        if ( symbols(j)->is_user_variable() && 
	     std::find( variables_request_list.begin(), variables_request_list.end(), 
			symbols(j)->name() ) != variables_request_list.end() )
        {
          requested_variable_t tempVar;
          tempVar.name = symbols(j)->name();
          tempVar.ov = symbols(j);
          requested_variables.push_back(tempVar);
        }
      }

    }
  }

  variables_request_list = std::vector<std::string>();
  */
  return 0;
}


//*************************************************************************
status_t octave_server::set_history_list( void )
{
  
  // Build up the current list
  int currentLen = command_history::length();
  if ( currentLen != prevHistLen )
  {
    for( int i = prevHistLen ; i < currentLen ; i++ )
      history_list.append( command_history::get_entry(i) );
    prevHistLen = currentLen;
  }

  return 0;
}

//*************************************************************************
status_t octave_server::set_breakpoint_list( void )
{
  // Set the list of breakpoints
  current_breakpoints = std::vector<bp_info_t>();

  octave_value_list zz;
  
  bp_table::fname_line_map bp_list = bp_table::get_breakpoint_list(zz);
  for (bp_table::fname_line_map_iterator it = bp_list.begin () ; it != bp_list.end () ; it++)
  {	  
    bp_table::intmap m = it->second;
    size_t nel = m.size ();
    for (size_t j = 0; j < nel; j++)

    {
      bp_info_t tmp;
      tmp.filename = it->first;
      tmp.line_number = m[j];

      current_breakpoints.push_back (tmp);
    }
  }

  // If in debug mode, set the location of the break
  breakpoint_reached = std::vector<bp_info_t>();

  octave_user_code *dbg_fcn = get_user_code ();

  if (dbg_fcn)
    {
      bool have_file = true;

      std::string name = dbg_fcn->fcn_file_name ();

      if (name.empty ())
        {
          have_file = false;
	  
          name = dbg_fcn->name ();
        }

      //octave_stdout << "stopped in " << name << " at ";

      int l = octave_call_stack::caller_user_code_line ();

      if (l > 0)
        {

	  bp_info_t tmp;
	  QFileInfo pathInfo;
	  QString qFilePath (name.c_str());
	  pathInfo.setFile (qFilePath);
	  //(QString (name));
	  QString fileName (pathInfo.fileName ());
	  //std::string shortName = Glib::path_get_basename(name);
	  std::string shortName = fileName.toStdString ();
	  std::string funcName;
	  int dot = shortName.find_first_of(".");
	  if (dot!=std::string::npos)
	    {
	      funcName = shortName.substr (0,dot);
	    }
	  tmp.filename = funcName;
	  tmp.line_number = l;
	  
	  //	  octave_stdout << "BP reached at " << tmp.filename << ":" << tmp.line_number << std::endl;

	  breakpoint_reached.push_back (tmp);
        }
      else
        octave_stdout << " <unknown line>" << std::endl;
    }

  

  return 0;
}

//*************************************************************************
status_t octave_server::process_breakpoint_action (void)
{

  if (Vdebugging)
  {
    if (bp_action==BP_ACTION_STEP_INTO)
    {
      Vdebugging = false;
      tree_evaluator::dbstep_flag = -1;

      rl_line_buffer[0] = '\0';	
      rl_point = rl_end = 0;
      rl_done = 1;
      rl_forced_update_display ();
    }
    else if (bp_action==BP_ACTION_STEP_OVER)
    {
      Vdebugging = false;
      tree_evaluator::dbstep_flag = 1;

      rl_line_buffer[0] = '\0';	
      rl_point = rl_end = 0;
      rl_done = 1;
      rl_forced_update_display ();
    }
    else if (bp_action==BP_ACTION_STEP_OUT)
    {
      Vdebugging = false;
      tree_evaluator::dbstep_flag = -2;

      rl_line_buffer[0] = '\0';	
      rl_point = rl_end = 0;
      rl_done = 1;
      rl_forced_update_display ();

      
    }
    else if (bp_action==BP_ACTION_CONTINUE)
    {
      Vdebugging = false;
      tree_evaluator::dbstep_flag = 0;

      rl_line_buffer[0] = '\0';	
      rl_point = rl_end = 0;
      rl_done = 1;
      rl_forced_update_display ();
    }
    else if (bp_action==BP_ACTION_BREAK)
    {
      tree_evaluator::dbstep_flag = 0;
      octave_throw_interrupt_exception ();

      rl_line_buffer[0] = '\0';	
      rl_point = rl_end = 0;
      rl_done = 1;
      rl_forced_update_display ();
    }
    bp_action = BP_ACTION_NONE;
  }

  return 0;
}

//*************************************************************************
status_t octave_server::process_breakpoint_add_remove_modify(void)
{
  //octave_stdout << "Processing breakpoints changes" << std::endl;
  // Process added breakpoints
  for (int i = 0 ; i < added_breakpoints.size() ; i++)
  {
    std::string funcName = added_breakpoints[i].filename;
    bp_table::intmap lines;
    lines[0] = added_breakpoints[i].line_number;
    bp_table::add_breakpoint (funcName,lines);
    octave_stdout << "Adding breakpoint: " << funcName << " : " << lines[0] << std::endl; 
  }
  added_breakpoints = std::vector<bp_info_t>();

  // Process removed breakpoints
  for (int i = 0 ; i < removed_breakpoints.size() ; i++)
  {
    std::string funcName = removed_breakpoints[i].filename;
    bp_table::intmap lines;
    lines[0] = removed_breakpoints[i].line_number;
    bp_table::remove_breakpoint (funcName,lines);
    //octave_stdout << "Removing breakpoint: " << funcName << " : " << lines[0] << std::endl; 
  }
  removed_breakpoints = std::vector<bp_info_t>();

  // Process modified breakpoints
  // TODO:



  return 0;
}
