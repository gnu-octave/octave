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
#include <QMutexLocker>

OctaveLink OctaveLink::m_singleton;

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
int OctaveLink::readlineEventHook() {
  // TODO: No need to run too quickly.  The documentation says it will run
  // at most 10 times per second.  This may be too fast and we will need to
  // artificially slow it down somehow.  Not sure at this time how.
  OctaveLink::instance()->processOctaveServerData();
  return 0;
}

//*************************************************************************
OctaveLink::OctaveLink()
    : m_previousHistoryLength(0) {
}

OctaveLink::~OctaveLink() {
}

/*******************************************************************************
 *******************************************************************************
 * CLIENT SIDE FUNCTIONS
 *******************************************************************************
 *******************************************************************************/

//*************************************************************************
std::vector<OctaveLink::VariableMetaData> OctaveLink::variableInfoList(void) {
    QMutexLocker mutexLocker(&m_internalAccessMutex);

    // Copy the list of variable information
    std::vector<VariableMetaData> retval( m_variableSymbolTableList.size() );
    std::copy( m_variableSymbolTableList.begin(), m_variableSymbolTableList.end(), retval.begin() );
    m_variableSymbolTableList = std::vector<VariableMetaData>();

    return retval;
}


//*************************************************************************
std::vector<OctaveLink::RequestedVariable> OctaveLink::requestedVariables(void)
{
    QMutexLocker mutexLocker(&m_internalAccessMutex);

    // Copy the list of requested variables
    std::vector<RequestedVariable> retval( m_requestedVariables.size() );
    std::copy( m_requestedVariables.begin(), m_requestedVariables.end(), retval.begin() );
    m_requestedVariables = std::vector<RequestedVariable>();

    return retval;
}

//*************************************************************************
int OctaveLink::setRequestedVariableNames( std::vector<std::string> variables_names )
{
    QMutexLocker mutexLocker(&m_internalAccessMutex);

    // Set the list of requested variables
    m_variablesRequestList = std::vector<std::string>( variables_names.size() );
    std::copy( variables_names.begin(), variables_names.end(), m_variablesRequestList.begin() );

    return 0;
}

//*************************************************************************
string_vector OctaveLink::getHistoryList(void)
{
    QMutexLocker mutexLocker(&m_internalAccessMutex);

    // Copy the list of command history items
    string_vector retval( m_historyList );
    m_historyList = string_vector();

    return retval;
}

std::vector<OctaveLink::BreakPoint> OctaveLink::breakPointList(int& status)
{
    QMutexLocker mutexLocker(&m_internalAccessMutex);

    // Copy the list of variable information
    std::vector<BreakPoint> retval (m_currentBreakpoints.size());
    std::copy( m_currentBreakpoints.begin(), m_currentBreakpoints.end(), retval.begin() );

    status = 0;
    return retval;
}

bool OctaveLink::isBreakpointReached (int& status)
{
    QMutexLocker mutexLocker(&m_internalAccessMutex);

    // Copy the list of variable information
    bool retval = (m_reachedBreakpoints.size()>0);
    return retval;
}

std::vector<OctaveLink::BreakPoint> OctaveLink::reachedBreakpoint()
{
    QMutexLocker mutexLocker(&m_internalAccessMutex);

    // Copy the list of variable information
    std::vector<BreakPoint> retval (m_reachedBreakpoints.size());
    std::copy (m_reachedBreakpoints.begin(), m_reachedBreakpoints.end(), retval.begin() );

    return retval;
}

int OctaveLink::addBreakpoint( BreakPoint bp_info )
{
    QMutexLocker mutexLocker(&m_internalAccessMutex);

    // Copy the list of variable information
    m_addedBreakpoints.push_back (bp_info);

    return 0;
}

int OctaveLink::removeBreakpoint( BreakPoint bp_info )
{
    QMutexLocker mutexLocker(&m_internalAccessMutex);
    // Copy the list of variable information
    m_removedBreakpoints.push_back (bp_info);
    return 0;
}

int OctaveLink::setBreakpointAction (BreakPointAction action)
{
    QMutexLocker mutexLocker(&m_internalAccessMutex);
    m_breakPointAction = action;
    return 0;
}

/*******************************************************************************
 *******************************************************************************
 * SERVER SIDE FUNCTIONS
 *******************************************************************************
 *******************************************************************************/

//*************************************************************************
int OctaveLink::processOctaveServerData(void)
{
  struct timeval start, stop;
#ifndef __WIN32__
  gettimeofday(&start, NULL);
#endif

  QMutexLocker mutexLocker(&m_internalAccessMutex);

  process_breakpoint_action();
  processBreakpointAndRemoveModify();
  processRequestedVariables();
  setVariableInfoList();
  setHistoryList();
  setBreakPointList();

#ifndef __WIN32__
  gettimeofday(&stop, NULL);
  double elapsed = stop.tv_sec - start.tv_sec + 1E-6 * (stop.tv_usec - start.tv_usec);
  //octave_stdout << "SERVER ELAPSED: " << elapsed << std::endl;
#endif
  return 0;
}


//*************************************************************************
int OctaveLink::setVariableInfoList( void )
{
  static std::vector<VariableMetaData> lastVars;
  std::vector<VariableMetaData> currVars;
  std::list<symbol_table::symbol_record> lvars = symbol_table::all_variables();
  std::list<symbol_table::symbol_record>::iterator it;

  for ( it = lvars.begin() ; it != lvars.end() ; it++ )
  {
    octave_value varval( it->varval() );
    std::string nm = (*it).name();

    dim_vector dims = varval.dims ();

    VariableMetaData tempVar;
    tempVar.variableName = it->name();
    tempVar.dimensionalSize.push_back( varval.rows() );
    tempVar.dimensionalSize.push_back( varval.columns() );
    tempVar.byteSize = varval.byte_size();
    tempVar.typeName = varval.type_name();

    currVars.push_back(tempVar);
  }

  if ( lastVars != currVars )
  {
    lastVars = currVars;
    
    // Copy currVars into octave_server::variable_symtab_list
    m_variableSymbolTableList = std::vector<VariableMetaData>( currVars.size() );

    std::copy( currVars.begin(), currVars.end(), m_variableSymbolTableList.begin() );
  }
  return 0;
}


//*************************************************************************
int OctaveLink::processRequestedVariables( void )
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
int OctaveLink::setHistoryList( void )
{
  // Build up the current list
  int currentLen = command_history::length();
  if ( currentLen != m_previousHistoryLength )
  {
    for( int i = m_previousHistoryLength ; i < currentLen ; i++ )
      m_historyList.append( command_history::get_entry(i) );
    m_previousHistoryLength = currentLen;
  }

  return 0;
}

//*************************************************************************
int OctaveLink::setBreakPointList( void )
{
  // Set the list of breakpoints
  m_currentBreakpoints = std::vector<BreakPoint>();

  octave_value_list zz;
  
  bp_table::fname_line_map bp_list = bp_table::get_breakpoint_list(zz);
  for (bp_table::fname_line_map_iterator it = bp_list.begin () ; it != bp_list.end () ; it++)
  {	  
    bp_table::intmap m = it->second;
    size_t nel = m.size ();
    for (size_t j = 0; j < nel; j++)

    {
      BreakPoint tmp;
      tmp.fileName = it->first;
      tmp.lineNumber = m[j];

      m_currentBreakpoints.push_back (tmp);
    }
  }

  // If in debug mode, set the location of the break
  m_reachedBreakpoints = std::vector<BreakPoint>();

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

	  BreakPoint tmp;
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
          tmp.fileName = funcName;
          tmp.lineNumber = l;
	  
	  //	  octave_stdout << "BP reached at " << tmp.filename << ":" << tmp.line_number << std::endl;

          m_reachedBreakpoints.push_back (tmp);
        }
      else
        octave_stdout << " <unknown line>" << std::endl;
    }
  return 0;
}

//*************************************************************************
int OctaveLink::process_breakpoint_action (void)
{

  if (Vdebugging)
  {
    if (m_breakPointAction==StepInto)
    {
      Vdebugging = false;
      tree_evaluator::dbstep_flag = -1;

      rl_line_buffer[0] = '\0';	
      rl_point = rl_end = 0;
      rl_done = 1;
      rl_forced_update_display ();
    }
    else if (m_breakPointAction==StepOver)
    {
      Vdebugging = false;
      tree_evaluator::dbstep_flag = 1;

      rl_line_buffer[0] = '\0';	
      rl_point = rl_end = 0;
      rl_done = 1;
      rl_forced_update_display ();
    }
    else if (m_breakPointAction==StepOut)
    {
      Vdebugging = false;
      tree_evaluator::dbstep_flag = -2;

      rl_line_buffer[0] = '\0';	
      rl_point = rl_end = 0;
      rl_done = 1;
      rl_forced_update_display ();

      
    }
    else if (m_breakPointAction==Continue)
    {
      Vdebugging = false;
      tree_evaluator::dbstep_flag = 0;

      rl_line_buffer[0] = '\0';	
      rl_point = rl_end = 0;
      rl_done = 1;
      rl_forced_update_display ();
    }
    else if (m_breakPointAction==Break)
    {
      tree_evaluator::dbstep_flag = 0;
      octave_throw_interrupt_exception ();

      rl_line_buffer[0] = '\0';	
      rl_point = rl_end = 0;
      rl_done = 1;
      rl_forced_update_display ();
    }
    m_breakPointAction = None;
  }

  return 0;
}

//*************************************************************************
int OctaveLink::processBreakpointAndRemoveModify(void)
{
  //octave_stdout << "Processing breakpoints changes" << std::endl;
  // Process added breakpoints
  for (int i = 0 ; i < m_addedBreakpoints.size() ; i++)
  {
    std::string funcName = m_addedBreakpoints[i].fileName;
    bp_table::intmap lines;
    lines[0] = m_addedBreakpoints[i].lineNumber;
    bp_table::add_breakpoint (funcName,lines);
    octave_stdout << "Adding breakpoint: " << funcName << " : " << lines[0] << std::endl; 
  }
  m_addedBreakpoints = std::vector<BreakPoint>();

  // Process removed breakpoints
  for (int i = 0 ; i < m_removedBreakpoints.size() ; i++)
  {
    std::string funcName = m_removedBreakpoints[i].fileName;
    bp_table::intmap lines;
    lines[0] = m_removedBreakpoints[i].lineNumber;
    bp_table::remove_breakpoint (funcName,lines);
    //octave_stdout << "Removing breakpoint: " << funcName << " : " << lines[0] << std::endl; 
  }
  m_removedBreakpoints = std::vector<BreakPoint>();
  return 0;
}
