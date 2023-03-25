////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1993-2023 The Octave Project Developers
//
// See the file COPYRIGHT.md in the top-level directory of this
// distribution or <https://octave.org/copyright/>.
//
// This file is part of Octave.
//
// Octave is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// Octave is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Octave; see the file COPYING.  If not, see
// <https://www.gnu.org/licenses/>.
//
////////////////////////////////////////////////////////////////////////

// Born February 20, 1992.

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <iostream>
#include <string>

#include "file-ops.h"
#include "getopt-wrapper.h"
#include "lo-error.h"
#include "oct-env.h"
#include "str-vec.h"

#include "Cell.h"
#include "defun.h"
#include "display.h"
#include "error.h"
#include "input.h"
#include "interpreter.h"
#include "octave.h"
#include "oct-hist.h"
#include "oct-map.h"
#include "ovl.h"
#include "options.h"
#include "ov.h"
#include "parse.h"
#include "sysdep.h"
#include "usage.h"

OCTAVE_BEGIN_NAMESPACE(octave)

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
          m_echo_commands = true;
          break;

        case 'v':
          octave_print_version_and_exit ();
          break;

        case BUILT_IN_DOCSTRINGS_FILE_OPTION:
          if (octave_optarg_wrapper ())
            m_docstrings_file = octave_optarg_wrapper ();
          break;

        case DOC_CACHE_FILE_OPTION:
          if (octave_optarg_wrapper ())
            m_doc_cache_file = octave_optarg_wrapper ();
          break;

        case EVAL_OPTION:
          if (octave_optarg_wrapper ())
            {
              if (m_code_to_eval.empty ())
                m_code_to_eval = octave_optarg_wrapper ();
              else
                m_code_to_eval += (std::string (" ")
                                   + octave_optarg_wrapper ());
            }
          break;

        case EXEC_PATH_OPTION:
          if (octave_optarg_wrapper ())
            m_exec_path = octave_optarg_wrapper ();
          break;

        case EXPERIMENTAL_TERMINAL_WIDGET_OPTION:
#if defined (HAVE_QSCINTILLA)
          m_experimental_terminal_widget = true;
#endif
          break;

        case GUI_OPTION:
          m_gui = true;
          break;

        case IMAGE_PATH_OPTION:
          if (octave_optarg_wrapper ())
            m_image_path = octave_optarg_wrapper ();
          break;

        case INFO_FILE_OPTION:
          if (octave_optarg_wrapper ())
            m_info_file = octave_optarg_wrapper ();
          break;

        case INFO_PROG_OPTION:
          if (octave_optarg_wrapper ())
            m_info_program = octave_optarg_wrapper ();
          break;

        case LINE_EDITING_OPTION:
          m_forced_line_editing = m_line_editing = true;
          break;

        case NO_GUI_OPTION:
          m_gui = false;
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
          m_read_site_files = false;
          break;

        case PERSIST_OPTION:
          m_persist = true;
          break;

        case SERVER_OPTION:
          m_server = true;
          break;

        case TEXI_MACROS_FILE_OPTION:
          if (octave_optarg_wrapper ())
            m_texi_macros_file = octave_optarg_wrapper ();
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

  m_remaining_args = string_vector (argv+octave_optind_wrapper (),
                                    argc-octave_optind_wrapper ());
}

octave_value cmdline_options::as_octave_value (void) const
{
  octave_scalar_map m;

  m.assign ("sys_argc", sys_argc ());
  m.assign ("sys_argv", Cell (string_vector (sys_argv ())));
  m.assign ("echo_commands", echo_commands ());
  m.assign ("forced_interactive", forced_interactive ());
  m.assign ("forced_line_editing", forced_line_editing ());
  m.assign ("gui", gui ());
  m.assign ("inhibit_startup_message", inhibit_startup_message ());
  m.assign ("line_editing", line_editing ());
  m.assign ("no_window_system", no_window_system ());
  m.assign ("persist", persist ());
  m.assign ("read_history_file", read_history_file ());
  m.assign ("read_init_files", read_init_files ());
  m.assign ("read_site_files", read_site_files ());
  m.assign ("server", server ());
  m.assign ("set_initial_path", set_initial_path ());
  m.assign ("traditional", traditional ());
  m.assign ("verbose_flag", verbose_flag ());
  m.assign ("code_to_eval", code_to_eval ());
  m.assign ("command_line_path", string_vector (command_line_path ()));
  m.assign ("docstrings_file", docstrings_file ());
  m.assign ("doc_cache_file", doc_cache_file ());
  m.assign ("exec_path", exec_path ());
  m.assign ("image_path", image_path ());
  m.assign ("info_file", info_file ());
  m.assign ("info_program", info_program ());
  m.assign ("texi_macros_file", texi_macros_file ());
  m.assign ("all_args", Cell (all_args ()));
  m.assign ("remaining_args", Cell (remaining_args ()));

  return m;
}

application *application::s_instance = nullptr;

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

// Note: Although the application destructor doesn't explicitly
// perform any actions, it can't be declared "default" in the header
// file if the interpreter is an incomplete type.  Providing
// an explicit definition of the destructor here is much simpler than
// including the full declaration of interpreter in the
// octave.h header file.
application::~application (void) { }

void
application::set_program_names (const std::string& pname)
{
  m_program_invocation_name = pname;

  std::size_t pos = pname.find_last_of (sys::file_ops::dir_sep_chars ());

  m_program_name = (pos != std::string::npos) ? pname.substr (pos+1) : pname;
}

void
application::intern_argv (const string_vector& args)
{
  octave_idx_type nargs = args.numel ();

  if (nargs > 0)
    {
      // Skip first argument (program name).
      nargs--;

      m_argv.resize (nargs);

      for (octave_idx_type i = 0; i < nargs; i++)
        m_argv[i] = args[i+1];
    }
}

bool application::forced_interactive (void)
{
  return s_instance ? s_instance->m_options.forced_interactive () : false;
}

// Provided for convenience.  Will be removed once we eliminate the
// old terminal widget.
bool application::experimental_terminal_widget (void) const
{
  return (s_instance
          ? s_instance->m_options.experimental_terminal_widget () : false);
}

bool application::interpreter_initialized (void)
{
  return m_interpreter ? m_interpreter->initialized () : false;
}

interpreter& application::create_interpreter (void)
{
  if (! m_interpreter)
    m_interpreter = std::unique_ptr<interpreter> (new interpreter (this));

  return *m_interpreter;
}

void application::initialize_interpreter (void)
{
  if (m_interpreter)
    m_interpreter->initialize ();
}

int application::execute_interpreter (void)
{
  return m_interpreter ? m_interpreter->execute () : -1;
}

void application::delete_interpreter (void)
{
  m_interpreter.reset ();
}

void application::init (void)
{
  if (s_instance)
    throw std::runtime_error
    ("only one Octave application object may be active");

  s_instance = this;

  string_vector all_args = m_options.all_args ();

  set_program_names (all_args[0]);

  string_vector remaining_args = m_options.remaining_args ();

  std::string code_to_eval = m_options.code_to_eval ();

  m_have_script_file = ! remaining_args.empty ();

  m_have_eval_option_code = ! code_to_eval.empty ();

  if (m_have_eval_option_code && m_have_script_file)
    {
      std::cerr << R"(error: --eval "CODE" and script file are mutually exclusive options)" << std::endl;

      octave_print_terse_usage_and_exit ();
    }

  if (m_options.gui ())
    {
      if (m_options.no_window_system ())
        {
          std::cerr << "error: --gui and --no-window-system are mutually exclusive options" << std::endl;
          octave_print_terse_usage_and_exit ();
        }
      if (! m_options.line_editing ())
        {
          std::cerr << "error: --gui and --no-line-editing are mutually exclusive options" << std::endl;
          octave_print_terse_usage_and_exit ();
        }
      if (m_options.server ())
        {
          std::cerr << "error: --gui and --server are mutually exclusive options" << std::endl;
          octave_print_terse_usage_and_exit ();
        }
    }

  m_is_octave_program = ((m_have_script_file || m_have_eval_option_code)
                         && ! m_options.persist ()
                         && ! m_options.traditional ());

  // This should probably happen early.
  sysdep_init ();
}

int cli_application::execute (void)
{
  interpreter& interp = create_interpreter ();

  int status = interp.execute ();

  return status;
}

DEFUN (isguirunning, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{tf} =} isguirunning ()
Return true if Octave is running in GUI mode and false otherwise.
@seealso{have_window_system}
@end deftypefn */)
{
  if (args.length () != 0)
    print_usage ();

  // FIXME: This isn't quite right, it just says that we intended to
  // start the GUI, not that it is actually running.

  return ovl (application::is_gui_running ());
}

/*
%!assert (islogical (isguirunning ()))
%!error <Invalid call> isguirunning (1)
*/

DEFUN (argv, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{args} =} argv ()
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
@seealso{program_name, cmdline_options}
@end deftypefn */)
{
  if (args.length () != 0)
    print_usage ();

  return ovl (Cell (application::argv ()));
}

/*
%!assert (iscellstr (argv ()))
%!error <Invalid call> argv (1)
*/

DEFUN (cmdline_options, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{opt_struct} =} cmdline_options ()
Return a structure containing detailed information about the command line
arguments passed to Octave.

Programming Note: This function provides copious amounts of information about
Octave's parsing of command line options and may be more useful for debugging
Octave rather than for general use.
@seealso{argv, program_name}
@end deftypefn */)
{
  if (args.length () != 0)
    print_usage ();

  application *app = application::app ();

  if (! app)
    error ("invalid application context!");

  cmdline_options opts = app->options ();

  return ovl (opts.as_octave_value ());
}

/*
%!assert (isstruct (cmdline_options ()))
%!error <Invalid call> cmdline_options (1)
*/

DEFUN (program_invocation_name, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{name} =} program_invocation_name ()
Return the string that was typed at the shell prompt to run Octave.

The string may include path components as well as the program filename.

If executing a script from the command line (e.g., @code{octave foo.m}) or
using an executable Octave script, the program name is set to the name of the
script.  @xref{Executable Octave Programs}, for an example of how to create an
executable Octave script.
@seealso{program_name, argv}
@end deftypefn */)
{
  if (args.length () != 0)
    print_usage ();

  return ovl (application::program_invocation_name ());
}

/*
%!assert (ischar (program_invocation_name ()))
%!error <Invalid call> program_invocation_name (1)
*/

DEFUN (program_name, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{name} =} program_name ()
Return the filename component of the value returned by
@code{program_invocation_name}.

@seealso{program_invocation_name, argv}
@end deftypefn */)
{
  if (args.length () != 0)
    print_usage ();

  return ovl (application::program_name ());
}

/*
%!assert (ischar (program_name ()))
%!error <Invalid call> program_name (1)
*/

OCTAVE_END_NAMESPACE(octave)
