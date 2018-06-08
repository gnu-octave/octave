/*

Copyright (C) 2002-2018 John W. Eaton

This file is part of Octave.

Octave is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<https://www.gnu.org/licenses/>.

*/

#if ! defined (octave_interpreter_h)
#define octave_interpreter_h 1

#include "octave-config.h"

#include <string>

#include "child-list.h"
#include "quit.h"
#include "str-vec.h"

#include "dynamic-ld.h"
#include "environment.h"
#include "gtk-manager.h"
#include "help.h"
#include "input.h"
#include "installation-data.h"
#include "load-path.h"
#include "oct-stream.h"
#include "ov-classdef.h"
#include "ov-typeinfo.h"
#include "pager.h"
#include "pt-eval.h"
#include "symtab.h"
#include "url-handle-manager.h"

extern OCTINTERP_API bool quit_allowed;

// TRUE means we are ready to interpret commands, but not everything
// is ready for interactive use.
extern OCTINTERP_API bool octave_interpreter_ready;

// TRUE means we've processed all the init code and we are good to go.
extern OCTINTERP_API bool octave_initialized;

namespace octave
{
  class profiler;
  class call_stack;
  class child_list;

  // The application object contains a pointer to the current
  // interpreter and the interpreter contains a pointer back to the
  // application context so we need a forward declaration for one (or
  // both) of them...

  class application;

  class OCTINTERP_API interpreter
  {
  public:

    // Create an interpreter object and perform basic initialization.

    interpreter (application *app_context = nullptr);

    // No copying, at least not yet...

    interpreter (const interpreter&) = delete;

    interpreter& operator = (const interpreter&) = delete;

    // Clean up the interpreter object.

    ~interpreter (void);

    void initialize_version_info (void);

    void intern_nargin (octave_idx_type nargs);

    // If creating an embedded interpreter, you may inhibit reading
    // the command history file by calling initialize_history with
    // read_history_file = false prior to calling initialize.

    void initialize_history (bool read_history_file = false);

    // If creating an embedded interpreter, you may inhibit setting
    // the default compiled-in path by calling intialize_load_path
    // with set_initial_path = false prior calling initialize.  After
    // that, you can add directories to the load path to set up a
    // custom path.

    void initialize_load_path (bool set_initial_path = true);

    // Load command line history, set the load path.

    void initialize (void);

    // Initialize the interpreter (if not already done by an explicit
    // call to intialize), execute startup files, --eval option code,
    // script files, and/or interactive commands.

    int execute (void);

    bool interactive (void) const
    {
      return m_interactive;
    }

    void interactive (bool arg)
    {
      m_interactive = arg;
    }

    void read_site_files (bool flag)
    {
      m_read_site_files = flag;
    }

    void read_init_files (bool flag)
    {
      m_read_init_files = flag;
    }

    void verbose (bool flag)
    {
      m_verbose = flag;
    }

    void inhibit_startup_message (bool flag)
    {
      m_inhibit_startup_message = flag;
    }

    bool initialized (void) const
    {
      return m_initialized;
    }

    installation_data& get_installation_data (void)
    {
      return  m_installation_data;
    }

    environment& get_environment (void)
    {
      return m_environment;
    }

    help_system& get_help_system (void)
    {
      return m_help_system;
    }

    input_system& get_input_system (void)
    {
      return m_input_system;
    }

    output_system& get_output_system (void)
    {
      return m_output_system;
    }

    dynamic_loader& get_dynamic_loader (void)
    {
      return m_dynamic_loader;
    }

    load_path& get_load_path (void)
    {
      return m_load_path;
    }

    symbol_table& get_symbol_table (void)
    {
      return m_symbol_table;
    }

    type_info& get_type_info (void)
    {
      return m_type_info;
    }

    symbol_scope get_current_scope (void);
    symbol_scope require_current_scope (const std::string& who);

    call_stack& get_call_stack (void);

    profiler& get_profiler (void);

    tree_evaluator& get_evaluator (void);

    stream_list& get_stream_list (void);

    child_list& get_child_list (void)
    {
      return m_child_list;
    }

    url_handle_manager& get_url_handle_manager (void);

    cdef_manager& get_cdef_manager (void)
    {
      return m_cdef_manager;
    }

    gtk_manager& get_gtk_manager (void)
    {
      return m_gtk_manager;
    }

    void mlock (void);

    void munlock (const std::string& nm);

    bool mislocked (const std::string& nm);

    static void recover_from_exception (void);

    static void add_atexit_function (const std::string& fname);

    static bool remove_atexit_function (const std::string& fname);

    static interpreter * the_interpreter (void) { return instance; }

  private:

    // The interpreter instance;  Currently it is only possible to
    // have one, so OCTAVE_THREAD_LOCAL will normally be defined to be
    // empty.  Eventually we would like to allow multiple interpreters
    // to be active at once, but they will still be limited to one per
    // thread.  When that is possible, OCTAVE_THREAD_LOCAL can be
    // replaced by the C++ thread_local keyword.  For now, use a macro
    // to allow experimenting with thread_local storage.

    OCTAVE_THREAD_LOCAL static interpreter *instance;

    static std::list<std::string> atexit_functions;

    void display_startup_message (void) const;

    int execute_startup_files (void) const;

    int execute_eval_option_code (void);

    int execute_command_line_file (void);

    int main_loop (void);

    void cleanup (void);

    application *m_app_context;

    installation_data m_installation_data;

    environment m_environment;

    help_system m_help_system;

    input_system m_input_system;

    output_system m_output_system;

    dynamic_loader m_dynamic_loader;

    load_path m_load_path;

    type_info m_type_info;

    symbol_table m_symbol_table;

    tree_evaluator m_evaluator;

    stream_list m_stream_list;

    child_list m_child_list;

    url_handle_manager m_url_handle_manager;

    cdef_manager m_cdef_manager;

    gtk_manager m_gtk_manager;

    // TRUE means this is an interactive interpreter (forced or not).
    bool m_interactive;

    bool m_read_site_files;

    bool m_read_init_files;

    bool m_verbose;

    bool m_inhibit_startup_message;

    bool m_load_path_initialized;

    bool m_history_initialized;

    bool m_initialized;

    void maximum_braindamage (void);

    void execute_pkg_add (const std::string& dir);
  };
}

#if defined (OCTAVE_USE_DEPRECATED_FUNCTIONS)

OCTAVE_DEPRECATED (4.4, "use 'octave::interpreter::recover_from_exception' instead")
static inline void
recover_from_exception (void)
{
  octave::interpreter::recover_from_exception ();
}

OCTAVE_DEPRECATED (4.4, "use 'octave::interpreter::add_atexit_function' instead")
static inline void
add_atexit_function (const std::string& fname)
{
  octave::interpreter::add_atexit_function (fname);
}

OCTAVE_DEPRECATED (4.4, "use 'octave::interpreter::remove_atexit_function' instead")
static inline bool
remove_atexit_function (const std::string& fname)
{
  return octave::interpreter::remove_atexit_function (fname);
}

#endif

#endif
