/*

Copyright (C) 2002-2016 John W. Eaton

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

#include "pt-eval.h"

typedef void (*octave_exit_func) (int);
extern OCTINTERP_API octave_exit_func octave_exit;

extern OCTINTERP_API bool quit_allowed;

extern OCTINTERP_API bool quitting_gracefully;

extern OCTINTERP_API void recover_from_exception (void);

extern OCTINTERP_API void
octave_add_atexit_function (const std::string& fname);

extern OCTINTERP_API bool
octave_remove_atexit_function (const std::string& fname);

// TRUE means we are ready to interpret commands, but not everything
// is ready for interactive use.
extern OCTINTERP_API bool octave_interpreter_ready;

// TRUE means we've processed all the init code and we are good to go.
extern OCTINTERP_API bool octave_initialized;

// Call a function with exceptions handled to avoid problems with
// errors while shutting down.

#define OCTAVE_IGNORE_EXCEPTION(E)                                      \
  catch (E)                                                             \
    {                                                                   \
      std::cerr << "error: ignoring " #E " while preparing to exit" << std::endl; \
      recover_from_exception ();                                        \
    }

#define OCTAVE_SAFE_CALL(F, ARGS)                                       \
  do                                                                    \
    {                                                                   \
      try                                                               \
        {                                                               \
          octave::unwind_protect frame;                                 \
                                                                        \
          frame.protect_var (Vdebug_on_error);                          \
          frame.protect_var (Vdebug_on_warning);                        \
                                                                        \
          Vdebug_on_error = false;                                      \
          Vdebug_on_warning = false;                                    \
                                                                        \
          F ARGS;                                                       \
        }                                                               \
      OCTAVE_IGNORE_EXCEPTION (const octave::interrupt_exception&)       \
        OCTAVE_IGNORE_EXCEPTION (const octave::execution_exception&)     \
        OCTAVE_IGNORE_EXCEPTION (const std::bad_alloc&)                 \
        }                                                               \
  while (0)

namespace octave
{
  extern tree_evaluator *current_evaluator;

  // The application object contains a pointer to the current
  // interpreter and the interpreter contains a pointer back to the
  // application context so we need a forward declaration for one (or
  // both) of them...

  class application;

  class OCTINTERP_API interpreter
  {
  public:

    interpreter (application *app_context = 0, bool embedded = false);

    // No copying, at least not yet...

    interpreter (const interpreter&) = delete;

    interpreter& operator = (const interpreter&) = delete;

    ~interpreter (void);

    int execute (void);

    int execute_eval_option_code (const std::string& code);

    void execute_command_line_file (const std::string& fname);

    bool interactive (void) const { return m_interactive; }
    void interactive (bool arg) { m_interactive = arg; }

  private:

    int main_loop (void);

    void clean_up_and_exit (int status, bool safe_to_return = false);

    application *m_app_context;

    tree_evaluator *m_evaluator;

    bool m_embedded;

    // TRUE means this is an interactive interpreter (forced or not).
    bool m_interactive;

    bool m_quitting_gracefully;
  };
}

#endif

