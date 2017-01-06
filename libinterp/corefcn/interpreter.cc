/*

Copyright (C) 1993-2016 John W. Eaton

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

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <string>
#include <iostream>

#include "cmd-edit.h"
#include "file-ops.h"
#include "file-stat.h"
#include "fpucw-wrappers.h"
#include "lo-blas-proto.h"
#include "lo-error.h"
#include "oct-env.h"
#include "str-vec.h"
#include "signal-wrappers.h"
#include "unistd-wrappers.h"

#include "builtins.h"
#include "defaults.h"
#include "Cell.h"
#include "defun.h"
#include "display.h"
#include "error.h"
#include "file-io.h"
#include "graphics.h"
#include "interpreter.h"
#include "load-path.h"
#include "load-save.h"
#include "octave-link.h"
#include "octave.h"
#include "oct-hist.h"
#include "oct-map.h"
#include "oct-mutex.h"
#include "ops.h"
#include "ovl.h"
#include "ov.h"
#include "ov-classdef.h"
#include "parse.h"
#include "pt-eval.h"
#include "pt-jump.h"
#include "pt-stmt.h"
#include "sighandlers.h"
#include "sysdep.h"
#include "unwind-prot.h"
#include "utils.h"
#include "variables.h"
#include <version.h>

void (*octave_exit) (int) = ::exit;

// TRUE means the quit() call is allowed.
bool quit_allowed = true;

// TRUE means we are ready to interpret commands, but not everything
// is ready for interactive use.
bool octave_interpreter_ready = false;

// TRUE means we've processed all the init code and we are good to go.
bool octave_initialized = false;

DEFUN (__version_info__, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {retval =} __version_info__ (@var{name}, @var{version}, @var{release}, @var{date})
Undocumented internal function.
@end deftypefn */)
{
  static octave_map vinfo;

  int nargin = args.length ();

  if (nargin != 0 && nargin != 4)
    print_usage ();

  octave_value retval;

  if (nargin == 0)
    retval = vinfo;
  else if (nargin == 4)
    {
      if (vinfo.nfields () == 0)
        {
          vinfo.assign ("Name", args(0));
          vinfo.assign ("Version", args(1));
          vinfo.assign ("Release", args(2));
          vinfo.assign ("Date", args(3));
        }
      else
        {
          octave_idx_type n = vinfo.numel () + 1;

          vinfo.resize (dim_vector (n, 1));

          octave_value idx (n);

          vinfo.assign (idx, "Name", Cell (octave_value (args(0))));
          vinfo.assign (idx, "Version", Cell (octave_value (args(1))));
          vinfo.assign (idx, "Release", Cell (octave_value (args(2))));
          vinfo.assign (idx, "Date", Cell (octave_value (args(3))));
        }
    }

  return retval;
}

static void
initialize_version_info (void)
{
  octave_value_list args;

  args(3) = OCTAVE_RELEASE_DATE;
  args(2) = OCTAVE_RELEASE;
  args(1) = OCTAVE_VERSION;
  args(0) = "GNU Octave";

  F__version_info__ (args, 0);
}

OCTAVE_NORETURN static void
lo_error_handler (const char *fmt, ...)
{
  va_list args;
  va_start (args, fmt);
  verror_with_cfn (fmt, args);
  va_end (args);

  octave_throw_execution_exception ();
}

OCTAVE_NORETURN static void
lo_error_with_id_handler (const char *id, const char *fmt, ...)
{
  va_list args;
  va_start (args, fmt);
  verror_with_id_cfn (id, fmt, args);
  va_end (args);

  octave_throw_execution_exception ();
}

static void
initialize_error_handlers ()
{
  set_liboctave_error_handler (lo_error_handler);
  set_liboctave_error_with_id_handler (lo_error_with_id_handler);
  set_liboctave_warning_handler (warning);
  set_liboctave_warning_with_id_handler (warning_with_id);
}

// What internal options get configured by --traditional.

static void
maximum_braindamage (void)
{
  FPS1 (octave_value (">> "));
  FPS2 (octave_value (""));
  FPS4 (octave_value (""));
  Fbeep_on_error (octave_value (true));
  Fconfirm_recursive_rmdir (octave_value (false));
  Fcrash_dumps_octave_core (octave_value (false));
  Fdisable_diagonal_matrix (octave_value (true));
  Fdisable_permutation_matrix (octave_value (true));
  Fdisable_range (octave_value (true));
  Ffixed_point_format (octave_value (true));
  Fhistory_timestamp_format_string (octave_value ("%%-- %D %I:%M %p --%%"));
  Fpage_screen_output (octave_value (false));
  Fprint_empty_dimensions (octave_value (false));
  Fsave_default_options (octave_value ("-mat-binary"));
  Fstruct_levels_to_print (octave_value (0));

  disable_warning ("Octave:abbreviated-property-match");
  disable_warning ("Octave:data-file-in-path");
  disable_warning ("Octave:function-name-clash");
  disable_warning ("Octave:possible-matlab-short-circuit-operator");
}

void
recover_from_exception (void)
{
  octave::can_interrupt = true;
  octave_interrupt_immediately = 0;
  octave_interrupt_state = 0;
  octave_signal_caught = 0;
  octave_exception_state = octave_no_exception;
  octave_restore_signal_mask ();
  octave::catch_interrupts ();
}

// Fix up things before exiting.

static std::list<std::string> octave_atexit_functions;

DEFUN (quit, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {} exit
@deftypefnx {} {} exit (@var{status})
@deftypefnx {} {} quit
@deftypefnx {} {} quit (@var{status})
Exit the current Octave session.

If the optional integer value @var{status} is supplied, pass that value to
the operating system as Octave's exit status.  The default value is zero.

When exiting, Octave will attempt to run the m-file @file{finish.m} if it
exists.  User commands to save the workspace or clean up temporary files
may be placed in that file.  Alternatively, another m-file may be scheduled
to run using @code{atexit}.
@seealso{atexit}
@end deftypefn */)
{
  // Confirm OK to shutdown.  Note: A dynamic function installation similar
  // to overriding polymorphism for which the GUI can install its own "quit"
  // yet call this base "quit" could be nice.  No link would be needed here.
  if (! octave_link::confirm_shutdown ())
    return ovl ();

  if (! quit_allowed)
    error ("quit: not supported in embedded mode");

  int exit_status = 0;

  if (args.length () > 0)
    exit_status = args(0).nint_value ();

  // Instead of simply calling exit, we thrown an exception so that no
  // matter where the call to quit occurs, we will run the
  // unwind_protect stack, clear the OCTAVE_LOCAL_BUFFER allocations,
  // etc. before exiting.

  clean_up_and_exit (exit_status);

  return ovl ();
}

DEFALIAS (exit, quit);

void
octave_add_atexit_function (const std::string& fname)
{
  octave_atexit_functions.push_front (fname);
}

bool
octave_remove_atexit_function (const std::string& fname)
{
  bool found = false;

  for (auto it = octave_atexit_functions.begin ();
       it != octave_atexit_functions.end (); it++)
    {
      if (*it == fname)
        {
          octave_atexit_functions.erase (it);
          found = true;
          break;
        }
    }

  return found;
}

DEFUN (atexit, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {} atexit (@var{fcn})
@deftypefnx {} {} atexit (@var{fcn}, @var{flag})
Register a function to be called when Octave exits.

For example,

@example
@group
function last_words ()
  disp ("Bye bye");
endfunction
atexit ("last_words");
@end group
@end example

@noindent
will print the message @qcode{"Bye bye"} when Octave exits.

The additional argument @var{flag} will register or unregister @var{fcn}
from the list of functions to be called when Octave exits.  If @var{flag} is
true, the function is registered, and if @var{flag} is false, it is
unregistered.  For example, after registering the function @code{last_words}
above,

@example
atexit ("last_words", false);
@end example

@noindent
will remove the function from the list and Octave will not call
@code{last_words} when it exits.

Note that @code{atexit} only removes the first occurrence of a function
from the list, so if a function was placed in the list multiple times with
@code{atexit}, it must also be removed from the list multiple times.
@seealso{quit}
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin < 1 || nargin > 2)
    print_usage ();

  std::string arg = args(0).xstring_value ("atexit: FCN argument must be a string");

  bool add_mode = (nargin == 2)
    ? args(1).xbool_value ("atexit: FLAG argument must be a logical value")
    : true;

  octave_value_list retval;

  if (add_mode)
    octave_add_atexit_function (arg);
  else
    {
      bool found = octave_remove_atexit_function (arg);

      if (nargout > 0)
        retval = ovl (found);
    }

  return retval;
}

// Execute commands from a file and catch potential exceptions in a consistent
// way.  This function should be called anywhere we might parse and execute
// commands from a file before we have entered the main loop in
// toplev.cc.

static void
safe_source_file (const std::string& file_name,
                  const std::string& context = "",
                  bool verbose = false, bool require_file = true,
                  const std::string& warn_for = "")
{
  try
    {
      source_file (file_name, context, verbose, require_file, warn_for);
    }
  catch (const octave::index_exception& e)
    {
      recover_from_exception ();

      std::cerr << "error: index exception in " << file_name << ": "
                << e.message () << std::endl;
    }
  catch (const octave::exit_exception& ex)
    {
      recover_from_exception ();

      clean_up_and_exit (ex.exit_status (), ex.safe_to_return ());
    }
  catch (const octave::interrupt_exception&)
    {
      recover_from_exception ();
    }
  catch (const octave::execution_exception&)
    {
      recover_from_exception ();

      std::cerr << "error: execution exception in " << file_name << std::endl;
    }
}

static void
execute_pkg_add (const std::string& dir)
{
  std::string file_name = octave::sys::file_ops::concat (dir, "PKG_ADD");

  try
    {
      load_path::execute_pkg_add (dir);
    }
  catch (const octave::index_exception& e)
    {
      recover_from_exception ();

      std::cerr << "error: index exception in " << file_name << ": "
                << e.message () << std::endl;
    }
  catch (const octave::exit_exception& ex)
    {
      recover_from_exception ();

      clean_up_and_exit (ex.exit_status (), ex.safe_to_return ());
    }
  catch (const octave::interrupt_exception&)
    {
      recover_from_exception ();
    }
  catch (const octave::execution_exception&)
    {
      recover_from_exception ();

      std::cerr << "error: execution exception in " << file_name << std::endl;
    }
}

static void
initialize_load_path (bool set_initial_path)
{
  // Temporarily set the execute_pkg_add function to one that catches
  // exceptions.  This is better than wrapping load_path::initialize in
  // a try-catch block because it will not stop executing PKG_ADD files
  // at the first exception.  It's also better than changing the default
  // execute_pkg_add function to use safe_source file because that will
  // normally be evaluated from the normal intepreter loop where
  // exceptions are already handled.

  octave::unwind_protect frame;

  frame.add_fcn (load_path::set_add_hook, load_path::get_add_hook ());

  load_path::set_add_hook (execute_pkg_add);

  load_path::initialize (set_initial_path);
}

// Initialize by reading startup files.

static void
execute_startup_files (bool read_site_files, bool read_init_files,
                       bool verbose_flag, bool inhibit_startup_message)
{
  octave::unwind_protect frame;

  std::string context;

  bool verbose = (verbose_flag && ! inhibit_startup_message);

  bool require_file = false;

  if (read_site_files)
    {
      // Execute commands from the site-wide configuration file.
      // First from the file $(prefix)/lib/octave/site/m/octaverc
      // (if it exists), then from the file
      // $(prefix)/share/octave/$(version)/m/octaverc (if it exists).

      safe_source_file (Vlocal_site_defaults_file, context, verbose,
                        require_file);

      safe_source_file (Vsite_defaults_file, context, verbose, require_file);
    }

  if (read_init_files)
    {
      // Try to execute commands from $HOME/$OCTAVE_INITFILE and
      // $OCTAVE_INITFILE.  If $OCTAVE_INITFILE is not set,
      // .octaverc is assumed.

      bool home_rc_already_executed = false;

      std::string initfile = octave::sys::env::getenv ("OCTAVE_INITFILE");

      if (initfile.empty ())
        initfile = ".octaverc";

      std::string home_dir = octave::sys::env::get_home_directory ();

      std::string home_rc = octave::sys::env::make_absolute (initfile, home_dir);

      std::string local_rc;

      if (! home_rc.empty ())
        {
          safe_source_file (home_rc, context, verbose, require_file);

          // Names alone are not enough.

          octave::sys::file_stat fs_home_rc (home_rc);

          if (fs_home_rc)
            {
              // We want to check for curr_dir after executing home_rc
              // because doing that may change the working directory.

              local_rc = octave::sys::env::make_absolute (initfile);

              home_rc_already_executed = same_file (home_rc, local_rc);
            }
        }

      if (! home_rc_already_executed)
        {
          if (local_rc.empty ())
            local_rc = octave::sys::env::make_absolute (initfile);

          safe_source_file (local_rc, context, verbose, require_file);
        }
    }
}

namespace octave
{
  tree_evaluator *current_evaluator = 0;

  interpreter::interpreter (application *app_context, bool embedded)
    : m_app_context (app_context), m_evaluator (new tree_evaluator (this)),
      m_embedded (embedded), m_interactive (false),
      m_quitting_gracefully (false)
  {
    current_evaluator = m_evaluator;

    cmdline_options options = m_app_context->options ();

    // Matlab uses "C" locale for LC_NUMERIC class regardless of local setting
    setlocale (LC_NUMERIC, "C");
    setlocale (LC_TIME, "C");
    octave::sys::env::putenv ("LC_NUMERIC", "C");
    octave::sys::env::putenv ("LC_TIME", "C");

    // Initialize the default floating point unit control state.
    octave_set_default_fpucw ();

    string_vector all_args = options.all_args ();

    octave_thread::init ();

    set_default_prompts ();

    // Initialize default warning state before --traditional option
    // that may reset them.

    initialize_default_warning_state ();

    if (options.traditional ())
      maximum_braindamage ();

    octave_ieee_init ();

    // The idea here is to force xerbla to be referenced so that we will link to
    // our own version instead of the one provided by the BLAS library.  But
    // octave::numeric_limits<double>::NaN () should never be -1, so we
    // should never actually call xerbla.  FIXME (again!):  If this
    // becomes a constant expression the test might be optimized away and
    // then the reference to the function might also disappear.

    if (octave::numeric_limits<double>::NaN () == -1)
      F77_FUNC (xerbla, XERBLA) ("octave", 13 F77_CHAR_ARG_LEN (6));

    initialize_error_handlers ();

    if (! m_embedded)
      octave::install_signal_handlers ();
    else
      quit_allowed = false;

    initialize_file_io ();

    install_types ();

    install_ops ();

    install_builtins ();

    install_classdef ();

    std::list<std::string> command_line_path = options.command_line_path ();

    for (const auto& pth : command_line_path)
      load_path::set_command_line_path (pth);

    std::string exec_path = options.exec_path ();
    if (! exec_path.empty ())
      set_exec_path (exec_path);

    std::string image_path = options.image_path ();
    if (! image_path.empty ())
      set_image_path (image_path);

    if (options.no_window_system ())
      display_info::no_window_system ();

    // Is input coming from a terminal?  If so, we are probably interactive.

    // If stdin is not a tty, then we are reading commands from a pipe or
    // a redirected file.
    bool stdin_is_tty = octave_isatty_wrapper (fileno (stdin));

    m_interactive = (! m_embedded
                     && ! m_app_context->is_octave_program ()
                     && stdin_is_tty
                     && octave_isatty_wrapper (fileno (stdout)));

    // Check if the user forced an interactive session.
    if (options.forced_interactive ())
      m_interactive = true;

    bool line_editing = options.line_editing ();
    if ((! m_interactive || options.forced_interactive ())
        && ! options.forced_line_editing ())
      line_editing = false;

    // Force default line editor if we don't want readline editing.
    if (! line_editing)
      octave::command_editor::force_default_editor ();

    // These can come after command line args since none of them set any
    // defaults that might be changed by command line options.

    if (line_editing)
      initialize_command_input ();

    octave_interpreter_ready = true;

    initialize_version_info ();

    // Make all command-line arguments available to startup files,
    // including PKG_ADD files.

    app_context->intern_argv (options.all_args ());

    initialize_load_path (options.set_initial_path ());

    initialize_history (options.read_history_file ());
  }

  interpreter::~interpreter (void)
  {
    current_evaluator = 0;

    delete m_evaluator;
  }

  int interpreter::execute (void)
  {
    cmdline_options options = m_app_context->options ();

    if (m_interactive && ! options.inhibit_startup_message ())
      std::cout << octave_startup_message () << "\n" << std::endl;

    octave_prepare_hdf5 ();

    execute_startup_files (options.read_site_files (),
                           options.read_init_files (),
                           options.verbose_flag (),
                           options.inhibit_startup_message ());

    if (m_interactive && ! options.inhibit_startup_message ()
        && reading_startup_message_printed)
      std::cout << std::endl;

    // Execute any code specified with --eval 'CODE'
    std::string code_to_eval = options.code_to_eval ();

    if (! code_to_eval.empty ())
      {
        int parse_status = 0;

        try
          {
            parse_status = execute_eval_option_code (code_to_eval);
          }
        catch (const octave::execution_exception&)
          {
            recover_from_exception ();

            parse_status = 1;
          }

        if (! options.persist ())
          {
            m_quitting_gracefully = true;

            clean_up_and_exit (parse_status);
          }
      }

    // If there is an extra argument, see if it names a file to read.
    // Additional arguments are taken as command line options for the script.

    if (m_app_context->have_script_file ())
      {
        // If we are running an executable script (#! /bin/octave) then
        // we should only see the args passed to the script.

        int exit_status = 0;

        try
          {
            string_vector script_args = options.remaining_args ();

            m_app_context->intern_argv (script_args);

            execute_command_line_file (script_args[0]);
          }
        catch (const octave::execution_exception&)
          {
            recover_from_exception ();

            exit_status = 1;
          }

        // Restore full set of args.
        m_app_context->intern_argv (options.all_args ());

        if (! options.persist ())
          {
            m_quitting_gracefully = true;

            clean_up_and_exit (exit_status);
          }
      }

    // Avoid counting commands executed from startup or script files.

    octave::command_editor::reset_current_command_number (1);

    // Force input to be echoed if not really interactive,
    // but the user has forced interactive behavior.

    if (options.forced_interactive ())
      {
        octave::command_editor::blink_matching_paren (false);

        // FIXME: is this the right thing to do?
        Fecho_executing_commands (octave_value (ECHO_CMD_LINE));
      }

    if (m_embedded)
      {
        // FIXME: Do we need to do any cleanup here before returning?
        // If we don't, what will happen to Octave functions that have been
        // registered to execute with atexit, for example?

        return 1;
      }

    int retval = main_loop ();

    m_quitting_gracefully = true;

    clean_up_and_exit (retval, true);

    return retval;
  }

  int interpreter::execute_eval_option_code (const std::string& code)
  {
    octave::unwind_protect frame;

    octave_save_signal_mask ();

    octave::can_interrupt = true;

    octave_signal_hook = octave::signal_handler;
    octave_interrupt_hook = 0;
    octave_bad_alloc_hook = 0;

    octave::catch_interrupts ();

    octave_initialized = true;

    frame.add_method (this, &interpreter::interactive, m_interactive);

    m_interactive = false;

    int parse_status = 0;

    try
      {
        eval_string (code, false, parse_status, 0);
      }
    catch (const octave::exit_exception& ex)
      {
        recover_from_exception ();

        clean_up_and_exit (ex.exit_status (), ex.safe_to_return ());
      }
    catch (const octave::interrupt_exception&)
      {
        recover_from_exception ();
      }
    catch (const octave::execution_exception&)
      {
        recover_from_exception ();

        parse_status = 1;
      }

    return parse_status;
  }

  void interpreter::execute_command_line_file (const std::string& fname)
  {
    octave::unwind_protect frame;

    octave_save_signal_mask ();

    octave::can_interrupt = true;

    octave_signal_hook = octave::signal_handler;
    octave_interrupt_hook = 0;
    octave_bad_alloc_hook = 0;

    octave::catch_interrupts ();

    octave_initialized = true;

    frame.add_method (this, &interpreter::interactive, m_interactive);

    frame.add_method (m_app_context,
                      &application::program_invocation_name,
                      application::program_invocation_name ());

    frame.add_method (m_app_context,
                      &application::program_name,
                      application::program_name ());

    m_interactive = false;

    m_app_context->set_program_names (fname);

    std::string context;
    bool verbose = false;
    bool require_file = true;

    safe_source_file (fname, context, verbose, require_file, "octave");
  }

  int interpreter::main_loop (void)
  {
    octave_save_signal_mask ();

    octave::can_interrupt = true;

    octave_signal_hook = octave::signal_handler;
    octave_interrupt_hook = 0;
    octave_bad_alloc_hook = 0;

    octave::catch_interrupts ();

    octave_initialized = true;

    // The big loop.

    octave::lexer *lxr = (octave::application::interactive ()
                          ? new octave::lexer ()
                          : new octave::lexer (stdin));

    octave::parser parser (*lxr);

    int retval = 0;
    do
      {
        try
          {
            reset_error_handler ();

            parser.reset ();

            if (symbol_table::at_top_level ())
              octave::tree_evaluator::reset_debug_state ();

            retval = parser.run ();

            if (retval == 0)
              {
                if (parser.stmt_list)
                  {
                    parser.stmt_list->accept (*current_evaluator);

                    octave_quit ();

                    if (! octave::application::interactive ())
                      {
                        bool quit = (tree_return_command::returning
                                     || tree_break_command::breaking);

                        if (tree_return_command::returning)
                          tree_return_command::returning = 0;

                        if (tree_break_command::breaking)
                          tree_break_command::breaking--;

                        if (quit)
                          break;
                      }

                    if (octave_completion_matches_called)
                      octave_completion_matches_called = false;
                    else
                      octave::command_editor::increment_current_command_number ();
                  }
                else if (parser.lexer.end_of_input)
                  break;
              }
          }
        catch (const octave::exit_exception& ex)
          {
            recover_from_exception ();

            clean_up_and_exit (ex.exit_status (), ex.safe_to_return ());
          }
        catch (const octave::interrupt_exception&)
          {
            recover_from_exception ();

            // Required newline when the user does Ctrl+C at the prompt.
            if (octave::application::interactive ())
              octave_stdout << "\n";
          }
        catch (const octave::index_exception& e)
          {
            recover_from_exception ();

            std::cerr << "error: unhandled index exception: "
                      << e.message () << " -- trying to return to prompt"
                      << std::endl;
          }
        catch (const octave::execution_exception& e)
          {
            std::string stack_trace = e.info ();

            if (! stack_trace.empty ())
              std::cerr << stack_trace;

            if (octave::application::interactive ())
              recover_from_exception ();
            else
              {
                // We should exit with a nonzero status.
                retval = 1;
                break;
              }
          }
        catch (const std::bad_alloc&)
          {
            recover_from_exception ();

            std::cerr << "error: out of memory -- trying to return to prompt"
                      << std::endl;
          }

#if defined (DBSTOP_NANINF)
        if (Vdebug_on_naninf)
          {
            if (setjump (naninf_jump) != 0)
              debug_or_throw_exception (true);  // true = stack trace
          }
#endif
      }
    while (retval == 0);

    if (octave::application::interactive ())
      octave_stdout << "\n";

    if (retval == EOF)
      retval = 0;

    return retval;
  }

  void interpreter::clean_up_and_exit (int status, bool safe_to_return)
  {
    static bool deja_vu = false;

    OCTAVE_SAFE_CALL (remove_input_event_hook_functions, ());

    while (! octave_atexit_functions.empty ())
      {
        std::string fcn = octave_atexit_functions.front ();

        octave_atexit_functions.pop_front ();

        OCTAVE_SAFE_CALL (reset_error_handler, ());

        OCTAVE_SAFE_CALL (feval, (fcn, octave_value_list (), 0));

        OCTAVE_SAFE_CALL (flush_octave_stdout, ());
      }

    if (! deja_vu)
      {
        deja_vu = true;

        // Process pending events and disasble octave_link event
        // processing with this call.

        octave_link::process_events (true);

        // Do this explicitly so that destructors for mex file objects
        // are called, so that functions registered with mexAtExit are
        // called.
        OCTAVE_SAFE_CALL (clear_mex_functions, ());

        OCTAVE_SAFE_CALL (octave::command_editor::restore_terminal_state, ());

        // FIXME: is this needed?  Can it cause any trouble?
        OCTAVE_SAFE_CALL (raw_mode, (0));

        OCTAVE_SAFE_CALL (octave_history_write_timestamp, ());

        if (! octave::command_history::ignoring_entries ())
          OCTAVE_SAFE_CALL (octave::command_history::clean_up_and_save, ());

        OCTAVE_SAFE_CALL (gh_manager::close_all_figures, ());

        OCTAVE_SAFE_CALL (gtk_manager::unload_all_toolkits, ());

        OCTAVE_SAFE_CALL (close_files, ());

        OCTAVE_SAFE_CALL (cleanup_tmp_files, ());

        OCTAVE_SAFE_CALL (symbol_table::cleanup, ());

        OCTAVE_SAFE_CALL (sysdep_cleanup, ());

        OCTAVE_SAFE_CALL (octave_finalize_hdf5, ());

        OCTAVE_SAFE_CALL (flush_octave_stdout, ());

        if (! m_quitting_gracefully && octave::application::interactive ())
          {
            octave_stdout << "\n";

            // Yes, we want this to be separate from the call to
            // flush_octave_stdout above.

            OCTAVE_SAFE_CALL (flush_octave_stdout, ());
          }

        // Don't call singleton_cleanup_list::cleanup until we have the
        // problems with registering/unregistering types worked out.  For
        // example, uncomment the following line, then use the make_int
        // function from the examples directory to create an integer
        // object and then exit Octave.  Octave should crash with a
        // segfault when cleaning up the typinfo singleton.  We need some
        // way to force new octave_value_X types that are created in
        // .oct files to be unregistered when the .oct file shared library
        // is unloaded.
        //
        // OCTAVE_SAFE_CALL (singleton_cleanup_list::cleanup, ());

        OCTAVE_SAFE_CALL (octave::chunk_buffer::clear, ());
      }

    if (octave_link::exit (status))
      {
        if (safe_to_return)
          return;
        else
          {
            // What should we do here?  We might be called from some
            // location other than the end of octave::interpreter::execute
            // so it might not be safe to return.

            // We have nothing else to do at this point, and the
            // octave_link::exit function is supposed to take care of
            // exiting for us.  Hang here forever so we never return.

            while (true)
              {
                octave_sleep (1);
              }
          }
      }
    else
      {
        if (octave_exit)
          (*octave_exit) (status);
      }
  }
}

