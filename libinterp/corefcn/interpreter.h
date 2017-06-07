/*

Copyright (C) 2002-2017 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or
(at your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#if ! defined (octave_interpreter_h)
#define octave_interpreter_h 1

#include "octave-config.h"

#include <string>

#include "quit.h"
#include "str-vec.h"

#include "load-path.h"

extern OCTINTERP_API bool quit_allowed;

// TRUE means we are ready to interpret commands, but not everything
// is ready for interactive use.
extern OCTINTERP_API bool octave_interpreter_ready;

// TRUE means we've processed all the init code and we are good to go.
extern OCTINTERP_API bool octave_initialized;

namespace octave
{
  class call_stack;
  class tree_evaluator;

  // The application object contains a pointer to the current
  // interpreter and the interpreter contains a pointer back to the
  // application context so we need a forward declaration for one (or
  // both) of them...

  class application;

  class OCTINTERP_API interpreter
  {
  public:

    // Create an interpreter object and perform basic initialization
    // up to the point of reading history and setting the load path.

    interpreter (application *app_context = nullptr);

    // No copying, at least not yet...

    interpreter (const interpreter&) = delete;

    interpreter& operator = (const interpreter&) = delete;

    // Clean up the interpreter object.

    ~interpreter (void);

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

    // Load command line history, set the load path and execute
    // startup files.  May throw an exit_exception.

    int initialize (void);

    // Initialize the interpreter and execute --eval option code,
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

    load_path& get_load_path (void)
    {
      return m_load_path;
    }

    call_stack& get_call_stack (void);

    tree_evaluator& get_evaluator (void);

    static void recover_from_exception (void);

    static void add_atexit_function (const std::string& fname);

    static bool remove_atexit_function (const std::string& fname);

    static interpreter * the_interpreter (void) { return instance; }

  private:

    // The interpreter instance;  Currently it is only possible to
    // have one.
    static interpreter *instance;

    static std::list<std::string> atexit_functions;

    void display_startup_message (void) const;

    int execute_startup_files (void) const;

    int execute_eval_option_code (void);

    int execute_command_line_file (void);

    int main_loop (void);

    void cleanup (void);

    application *m_app_context;

    tree_evaluator *m_evaluator;

    load_path m_load_path;

    // TRUE means this is an interactive interpreter (forced or not).
    bool m_interactive;

    bool m_read_site_files;

    bool m_read_init_files;

    bool m_verbose;

    bool m_inhibit_startup_message;

    bool m_load_path_initialized;

    bool m_history_initialized;

    bool m_initialized;
  };
}

#if defined (OCTAVE_USE_DEPRECATED_FUNCTIONS)

OCTAVE_DEPRECATED ("use 'octave::interpreter::recover_from_exception' instead")
static inline void
recover_from_exception (void)
{
  octave::interpreter::recover_from_exception ();
}

OCTAVE_DEPRECATED ("use 'octave::interpreter::add_atexit_function' instead")
static inline void
add_atexit_function (const std::string& fname)
{
  octave::interpreter::add_atexit_function (fname);
}

OCTAVE_DEPRECATED ("use 'octave::interpreter::remove_atexit_function' instead")
static inline bool
remove_atexit_function (const std::string& fname)
{
  return octave::interpreter::remove_atexit_function (fname);
}

#endif

#endif
