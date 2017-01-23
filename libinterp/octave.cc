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

// Born February 20, 1992.

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <string>

#include "file-ops.h"
#include "getopt-wrapper.h"
#include "lo-error.h"
#include "oct-env.h"
#include "str-vec.h"

#include "builtin-defun-decls.h"
#include "Cell.h"
#include "defaults.h"
#include "defun.h"
#include "display.h"
#include "error.h"
#include "input.h"
#include "interpreter.h"
#include "octave.h"
#include "oct-hist.h"
#include "oct-map.h"
#include "ovl.h"
#include "options-usage.h"
#include "ov.h"
#include "parse.h"
#include "sysdep.h"

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
    interpreter *interp = instance->m_interpreter;

    if (interp)
      interp->interactive (arg);
  }

  bool application::forced_interactive (void)
  {
    return instance->m_options.forced_interactive ();
  }

  bool application::interactive (void)
  {
    interpreter *interp = instance->m_interpreter;

    return interp ? interp->interactive () : false;
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

    // This should probably happen early.
    sysdep_init ();

    // Need to have global Vfoo variables defined early.
    install_defaults ();
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
