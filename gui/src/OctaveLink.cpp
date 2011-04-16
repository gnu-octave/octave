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

#include "OctaveLink.h"

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

QString OctaveLink::octaveValueAsQString(OctaveValue octaveValue) {
    // Convert string.
    if(octaveValue.is_string()) {
        return QString("\"%1\"").arg(octaveValue.string_value().c_str());

    // Convert real scalar.
    } else if(octaveValue.is_real_scalar()) {
        return QString("%1").arg(octaveValue.scalar_value());

    // Convert complex scalar.
    } else if(octaveValue.is_complex_scalar()) {
        return QString("%1 + %2i").arg(octaveValue.scalar_value()).arg(octaveValue.complex_value().imag());

    // Convert real matrix.
    } else if(octaveValue.is_real_matrix()) {
        // TODO: Convert real matrix into a string.
        return QString("{matrix}");

    // Convert complex matrix.
    } else if(octaveValue.is_complex_matrix()) {
        // TODO: Convert complex matrix into a string.
        return QString("{complex matrix}");

    // If everything else does not fit, we could not recognize the type.
    } else {
        return QString("<Type not recognized>");
    }
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
QList<SymbolRecord> OctaveLink::workspace() {
    QMutexLocker mutexLocker(&m_internalAccessMutex);
    return m_variableSymbolTableList;
}


//*************************************************************************
QList<OctaveLink::RequestedVariable> OctaveLink::requestedVariables() {
    QMutexLocker mutexLocker(&m_internalAccessMutex);
    return m_requestedVariables;
}

//*************************************************************************
int OctaveLink::setRequestedVariableNames(QList<QString> variablesNames) {
    QMutexLocker mutexLocker(&m_internalAccessMutex);
    m_variablesRequestList = variablesNames;
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

bool OctaveLink::isBreakpointReached()
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
int OctaveLink::processOctaveServerData(void) {
    QMutexLocker mutexLocker(&m_internalAccessMutex);

    process_breakpoint_action();
    processBreakpointAndRemoveModify();
    processRequestedVariables();
    retrieveVariables();
    setHistoryList();
    setBreakPointList();
    return 0;
}

//*************************************************************************
void OctaveLink::retrieveVariables() {
    m_variableSymbolTableList.clear();
    std::list<SymbolRecord> allVariables = symbol_table::all_variables();
    std::list<SymbolRecord>::iterator iterator;
    for(iterator = allVariables.begin(); iterator != allVariables.end(); iterator++)
        m_variableSymbolTableList.append(*iterator);
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
          if (dot!=(signed)std::string::npos)
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
  for (int i = 0 ; i < (signed)m_addedBreakpoints.size() ; i++)
  {
    std::string funcName = m_addedBreakpoints[i].fileName;
    bp_table::intmap lines;
    lines[0] = m_addedBreakpoints[i].lineNumber;
    bp_table::add_breakpoint (funcName,lines);
    octave_stdout << "Adding breakpoint: " << funcName << " : " << lines[0] << std::endl; 
  }
  m_addedBreakpoints = std::vector<BreakPoint>();

  // Process removed breakpoints
  for (int i = 0 ; i < (signed)m_removedBreakpoints.size() ; i++)
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
