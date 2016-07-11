/*

Copyright (C) 1993-2015 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

// Born February 20, 1992.

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <cassert>
#include <clocale>
#include <cstdlib>
#include <cstring>
#include <ctime>

#include <iostream>

#include "cmd-edit.h"
#include "f77-fcn.h"
#include "file-ops.h"
#include "file-stat.h"
#include "fpucw-wrappers.h"
#include "getopt-wrapper.h"
#include "lo-error.h"
#include "oct-env.h"
#include "str-vec.h"
#include "signal-wrappers.h"
#include "unistd-wrappers.h"

#include "build-env.h"
#include "builtins.h"
#include "defaults.h"
#include "Cell.h"
#include "defun.h"
#include "display.h"
#include "error.h"
#include "file-io.h"
#include "help.h"
#include "input.h"
#include "lex.h"
#include "load-path.h"
#include "load-save.h"
#include "octave.h"
#include "oct-hist.h"
#include "oct-map.h"
#include "oct-mutex.h"
#include "ovl.h"
#include "ops.h"
#include "options-usage.h"
#include "ov.h"
#include "ov-classdef.h"
#include "ov-range.h"
#include "toplev.h"
#include "parse.h"
#include "procstream.h"
#include "sighandlers.h"
#include "sysdep.h"
#include "unwind-prot.h"
#include "utils.h"
#include "variables.h"
#include <version.h>

// Kluge.
extern "C" F77_RET_T
F77_FUNC (xerbla, XERBLA) (F77_CONST_CHAR_ARG_DECL,
                           const octave_idx_type&
                           F77_CHAR_ARG_LEN_DECL);

extern void install_builtins (void);

// Store the command-line options for later use.

static void
execute_pkg_add (const std::string& dir)
{
  std::string file_name = octave::sys::file_ops::concat (dir, "PKG_ADD");

  try
    {
      load_path::execute_pkg_add (dir);
    }
  catch (const index_exception& e)
    {
      recover_from_exception ();

      std::cerr << "error: index exception in " << file_name << ": "
                << e.message () << std::endl;
    }
  catch (const octave_interrupt_exception&)
    {
      recover_from_exception ();

      if (quitting_gracefully)
        clean_up_and_exit (exit_status);
    }
  catch (const octave_execution_exception&)
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

// Execute commands from a file and catch potential exceptions in a consistent
// way.  This function should be called anywhere we might parse and execute
// commands from a file before before we have entered the main loop in
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
  catch (const index_exception& e)
    {
      recover_from_exception ();

      std::cerr << "error: index exception in " << file_name << ": "
                << e.message () << std::endl;
    }
  catch (const octave_interrupt_exception&)
    {
      recover_from_exception ();

      if (quitting_gracefully)
        clean_up_and_exit (exit_status);
    }
  catch (const octave_execution_exception&)
    {
      recover_from_exception ();

      std::cerr << "error: execution exception in " << file_name << std::endl;
    }
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

namespace octave
{
  cmdline_options::cmdline_options (void)
  {
    m_all_args.resize (1);
    m_all_args[0] = "";
  }

  cmdline_options::cmdline_options (int argc, char **argv)
  {
    // Save raw program arguments.
    m_all_args = string_vector (argv, argc);

    while (true)
      {
        int long_idx;

        int optc = octave_getopt_long_wrapper (argc, argv, short_opts,
                                               long_opts, &long_idx);

        if (optc < 0)
          break;

        switch (optc)
          {
          case '?':
            // Unrecognized option.  getopt_long already printed a message about
            // it, so we will just print the usage string and exit.
            octave_print_terse_usage_and_exit ();
            break;

          case 'H':
            Fhistory_save (octave_value (false));
            m_read_history_file = false;
            break;

          case 'W':
            m_no_window_system = true;
            break;

          case 'V':
            m_verbose_flag = true;
            break;

          case 'd':
            // This is the same as yydebug in parse.y.
            octave_debug++;
            break;

          case 'f':
            m_read_init_files = false;
            m_read_site_files = false;
            break;

          case 'h':
            octave_print_verbose_usage_and_exit ();
            break;

          case 'i':
            m_forced_interactive = true;
            break;

          case 'p':
            if (octave_optarg_wrapper ())
              m_command_line_path.push_back (octave_optarg_wrapper ());
            break;

          case 'q':
            m_inhibit_startup_message = true;
            break;

          case 'x':
            {
              int val = ECHO_SCRIPTS | ECHO_FUNCTIONS | ECHO_CMD_LINE;
              Fecho_executing_commands (octave_value (val));
            }
            break;

          case 'v':
            octave_print_version_and_exit ();
            break;

          case BUILT_IN_DOCSTRINGS_FILE_OPTION:
            if (octave_optarg_wrapper ())
              Fbuilt_in_docstrings_file (octave_value (octave_optarg_wrapper ()));
            break;

          case DOC_CACHE_FILE_OPTION:
            if (octave_optarg_wrapper ())
              Fdoc_cache_file (octave_value (octave_optarg_wrapper ()));
            break;

          case EVAL_OPTION:
            if (octave_optarg_wrapper ())
              {
                if (m_code_to_eval.empty ())
                  m_code_to_eval = octave_optarg_wrapper ();
                else
                  m_code_to_eval += std::string (" ") + octave_optarg_wrapper ();
              }
            break;

          case EXEC_PATH_OPTION:
            if (octave_optarg_wrapper ())
              m_exec_path = octave_optarg_wrapper ();
            break;

          case FORCE_GUI_OPTION:
            m_force_gui = true;
            break;

          case IMAGE_PATH_OPTION:
            if (octave_optarg_wrapper ())
              m_image_path = octave_optarg_wrapper ();
            break;

          case INFO_FILE_OPTION:
            if (octave_optarg_wrapper ())
              Finfo_file (octave_value (octave_optarg_wrapper ()));
            break;

          case INFO_PROG_OPTION:
            if (octave_optarg_wrapper ())
              Finfo_program (octave_value (octave_optarg_wrapper ()));
            break;

          case DEBUG_JIT_OPTION:
            Fdebug_jit (octave_value (true));
            break;

          case JIT_COMPILER_OPTION:
            Fjit_enable (octave_value (true));
            break;

          case LINE_EDITING_OPTION:
            m_forced_line_editing = m_line_editing = true;
            break;

          case NO_GUI_OPTION:
            m_no_gui = true;
            break;

          case NO_INIT_FILE_OPTION:
            m_read_init_files = false;
            break;

          case NO_INIT_PATH_OPTION:
            m_set_initial_path = false;
            break;

          case NO_LINE_EDITING_OPTION:
            m_line_editing = false;
            break;

          case NO_SITE_FILE_OPTION:
            m_read_site_files = 0;
            break;

          case PERSIST_OPTION:
            m_persist = true;
            break;

          case TEXI_MACROS_FILE_OPTION:
            if (octave_optarg_wrapper ())
              Ftexi_macros_file (octave_value (octave_optarg_wrapper ()));
            break;

          case TRADITIONAL_OPTION:
            m_traditional = true;
            m_persist = true;
            break;

          default:
            // getopt_long should print a message about unrecognized options and
            // return '?', which is handled above.  If we end up here, it is
            // because there was an option but we forgot to handle it.
            // That should be fatal.
            panic_impossible ();
            break;
          }
      }

    // Check for various incompatible argument pairs
    if (m_force_gui && m_no_gui)
      {
        warning ("only one of --force-gui and --no-gui may be used");

        octave_print_terse_usage_and_exit ();
      }

    m_remaining_args = string_vector (argv+octave_optind_wrapper (),
                                      argc-octave_optind_wrapper ());
  }

  cmdline_options::cmdline_options (const cmdline_options& opts)
    : m_force_gui (opts.m_force_gui),
      m_forced_interactive (opts.m_forced_interactive),
      m_forced_line_editing (opts.m_forced_line_editing),
      m_inhibit_startup_message (opts.m_inhibit_startup_message),
      m_line_editing (opts.m_line_editing),
      m_no_gui (opts.m_no_gui),
      m_no_window_system (opts.m_no_window_system),
      m_persist (opts.m_persist),
      m_read_history_file (opts.m_read_history_file),
      m_read_init_files (opts.m_read_init_files),
      m_read_site_files (opts.m_read_site_files),
      m_set_initial_path (opts.m_set_initial_path),
      m_traditional (opts.m_traditional),
      m_verbose_flag (opts.m_verbose_flag),
      m_code_to_eval (opts.m_code_to_eval),
      m_command_line_path (opts.m_command_line_path),
      m_exec_path (opts.m_exec_path),
      m_image_path (opts.m_image_path),
      m_all_args (opts.m_all_args),
      m_remaining_args (opts.m_remaining_args)
  { }

  cmdline_options&
  cmdline_options::operator = (const cmdline_options& opts)
  {
    if (this != &opts)
      {
        m_force_gui = opts.m_force_gui;
        m_forced_interactive = opts.m_forced_interactive;
        m_forced_line_editing = opts.m_forced_line_editing;
        m_inhibit_startup_message = opts.m_inhibit_startup_message;
        m_line_editing = opts.m_line_editing;
        m_no_gui = opts.m_no_gui;
        m_no_window_system = opts.m_no_window_system;
        m_persist = opts.m_persist;
        m_read_history_file = opts.m_read_history_file;
        m_read_init_files = opts.m_read_init_files;
        m_read_site_files = opts.m_read_site_files;
        m_set_initial_path = opts.m_set_initial_path;
        m_traditional = opts.m_traditional;
        m_verbose_flag = opts.m_verbose_flag;
        m_code_to_eval = opts.m_code_to_eval;
        m_command_line_path = opts.m_command_line_path;
        m_exec_path = opts.m_exec_path;
        m_image_path = opts.m_image_path;
        m_all_args = opts.m_all_args;
        m_remaining_args = opts.m_remaining_args;
      }

    return *this;
  }

  application *application::instance = 0;

  application::application (int argc, char **argv)
    : m_options (argc, argv)
  {
    init ();
  }

  application::application (const cmdline_options& opts)
    : m_options (opts)
  {
    init ();
  }

  void
  application::set_program_names (const std::string& pname)
  {
    m_program_invocation_name = pname;

    size_t pos = pname.find_last_of (octave::sys::file_ops::dir_sep_chars ());

    m_program_name = (pos != std::string::npos) ? pname.substr (pos+1) : pname;
  }

  void
  application::intern_argv (const string_vector& args)
  {
    assert (symbol_table::at_top_level ());

    octave_idx_type nargs = args.numel ();

    if (nargs > 0)
      {
        // Skip first argument (program name).
        nargs--;

        m_argv.resize (nargs);

        for (octave_idx_type i = 0; i < nargs; i++)
          m_argv[i] = args[i+1];
      }

    symbol_table::assign (".nargin.", nargs);
    symbol_table::mark_hidden (".nargin.");
  }

  void application::interactive (bool arg)
  {
    m_interpreter->interactive (arg);
  }

  bool application::forced_interactive (void)
  {
    return instance->m_options.forced_interactive ();
  }

  bool application::interactive (void)
  {
    return instance->m_interpreter->interactive ();
  }

  application::~application (void)
  {
    instance = 0;

    delete m_interpreter;
  }

  void application::create_interpreter (void)
  {
    if (! m_interpreter)
      m_interpreter = new interpreter (this);
  }

  int application::execute_interpreter (void)
  {
    return m_interpreter ? m_interpreter->execute () : -1;
  }

  void application::init (void)
  {
    if (instance)
      {
        // FIXME: Should this be an error?
      }
    else
      instance = this;

    string_vector all_args = m_options.all_args ();

    set_program_names (all_args[0]);

    string_vector remaining_args = m_options.remaining_args ();

    std::string code_to_eval = m_options.code_to_eval ();

    m_have_script_file = ! remaining_args.empty ();

    if (! code_to_eval.empty () && m_have_script_file)
      {
        warning ("--eval \"CODE\" and script file are mutually exclusive options");

        octave_print_terse_usage_and_exit ();
      }

    m_is_octave_program = ((m_have_script_file || ! code_to_eval.empty ())
                           && ! m_options.persist ()
                           && ! m_options.traditional ());
  }

  int cli_application::execute (void)
  {
    create_interpreter ();

    return execute_interpreter ();
  }

  void embedded_application::create_interpreter (void)
  {
    if (! m_interpreter)
      m_interpreter = new interpreter (this, true);
  }

  int embedded_application::execute (void)
  {
    create_interpreter ();

    return execute_interpreter ();
  }

  interpreter::interpreter (application *app_context, bool embedded)
    : m_app_context (app_context), m_embedded (embedded),
      m_interactive (false)
  {
    cmdline_options options = m_app_context->options ();

    sysdep_init ();

    install_defaults ();

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

    for (std::list<std::string>::const_iterator it = command_line_path.begin ();
         it != command_line_path.end (); it++)
      load_path::set_command_line_path (*it);

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

    // Check if the user forced an interactive session.  If he
    // unnecessarily did so, reset forced_interactive to false.
    if (options.forced_interactive ())
      {
        if (m_interactive)
          options.forced_interactive (false);
        else
          m_interactive = true;
      }

    if ((! m_interactive || options.forced_interactive ())
        && ! options.forced_line_editing ())
      options.line_editing (false);

    // Also skip start-up message unless session is interactive.
    if (! m_interactive)
      options.inhibit_startup_message (true);

    // Force default line editor if we don't want readline editing.
    if (! options.line_editing ())
      octave::command_editor::force_default_editor ();

    // These can come after command line args since none of them set any
    // defaults that might be changed by command line options.

    if (options.line_editing ())
      initialize_command_input ();

    octave_interpreter_ready = true;

    initialize_version_info ();

    // Make all command-line arguments available to startup files,
    // including PKG_ADD files.

    app_context->intern_argv (options.all_args ());

    initialize_load_path (options.set_initial_path ());

    initialize_history (options.read_history_file ());
  }

  int interpreter::execute (void)
  {
    cmdline_options options = m_app_context->options ();

    if (! options.inhibit_startup_message ())
      std::cout << octave_startup_message () << "\n" << std::endl;

    octave_prepare_hdf5 ();

    execute_startup_files (options.read_site_files (),
                           options.read_init_files (),
                           options.verbose_flag (),
                           options.inhibit_startup_message ());

    if (! options.inhibit_startup_message ()
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
        catch (const octave_execution_exception&)
          {
            recover_from_exception ();

            parse_status = 1;
          }

        if (! options.persist ())
          {
            quitting_gracefully = true;

            clean_up_and_exit (parse_status);
          }
      }

    // If there is an extra argument, see if it names a file to read.
    // Additional arguments are taken as command line options for the script.

    if (m_app_context->have_script_file ())
      {
        // If we are running an executable script (#! /bin/octave) then
        // we should only see the args passed to the script.

        exit_status = 0;

        try
          {
            string_vector script_args = options.remaining_args ();

            m_app_context->intern_argv (script_args);

            execute_command_line_file (script_args[0]);
          }
        catch (const octave_execution_exception&)
          {
            recover_from_exception ();

            exit_status = 1;
          }

        // Restore full set of args.
        m_app_context->intern_argv (options.all_args ());

        if (! options.persist ())
          {
            quitting_gracefully = true;

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

    quitting_gracefully = true;

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
    catch (const octave_interrupt_exception&)
      {
        recover_from_exception ();

        if (quitting_gracefully)
          clean_up_and_exit (exit_status);
      }
    catch (const octave_execution_exception&)
      {
        recover_from_exception ();

        std::cerr << "error: unhandled execution exception -- eval failed"
                  << std::endl;
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
}

// embedded is int here because octave_main is extern "C".

int
octave_main (int argc, char **argv, int embedded)
{
  octave::sys::env::set_program_name (argv[0]);

  if (embedded)
    {
      octave::embedded_application app (argc, argv);
      return app.execute ();
    }
  else
    {
      octave::cli_application app (argc, argv);
      return app.execute ();
    }
}

DEFUN (isguirunning, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {} isguirunning ()
Return true if Octave is running in GUI mode and false otherwise.
@seealso{have_window_system}
@end deftypefn */)
{
  if (args.length () != 0)
    print_usage ();

  // FIXME: This isn't quite right, it just says that we intended to
  // start the GUI, not that it is actually running.

  return ovl (octave::application::is_gui_running ());
}

/*
%!assert (islogical (isguirunning ()))
%!error isguirunning (1)
*/

DEFUN (argv, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {} argv ()
Return the command line arguments passed to Octave.

For example, if you invoked Octave using the command

@example
octave --no-line-editing --silent
@end example

@noindent
@code{argv} would return a cell array of strings with the elements
@option{--no-line-editing} and @option{--silent}.

If you write an executable Octave script, @code{argv} will return the list
of arguments passed to the script.  @xref{Executable Octave Programs}, for
an example of how to create an executable Octave script.
@end deftypefn */)
{
  if (args.length () != 0)
    print_usage ();

  return ovl (Cell (octave::application::argv ()));
}

/*
%!assert (iscellstr (argv ()))
%!error argv (1)
*/

DEFUN (program_invocation_name, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {} program_invocation_name ()
Return the name that was typed at the shell prompt to run Octave.

If executing a script from the command line (e.g., @code{octave foo.m})
or using an executable Octave script, the program name is set to the
name of the script.  @xref{Executable Octave Programs}, for an example of
how to create an executable Octave script.
@seealso{program_name}
@end deftypefn */)
{
  if (args.length () != 0)
    print_usage ();

  return ovl (octave::application::program_invocation_name ());
}

/*
%!assert (ischar (program_invocation_name ()))
%!error program_invocation_name (1)
*/

DEFUN (program_name, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {} program_name ()
Return the last component of the value returned by
@code{program_invocation_name}.
@seealso{program_invocation_name}
@end deftypefn */)
{
  if (args.length () != 0)
    print_usage ();

  return ovl (octave::application::program_name ());
}

/*
%!assert (ischar (program_name ()))
%!error program_name (1)
*/
