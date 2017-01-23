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

#if ! defined (octave_octave_h)
#define octave_octave_h 1

#include "octave-config.h"

#if defined  (__cplusplus)

#include <list>
#include <string>

#include <str-vec.h>

namespace octave
{
  // Command line arguments.  See also options-usage.h.

  class OCTINTERP_API cmdline_options
  {
  public:

    cmdline_options (void);

    cmdline_options (int argc, char **argv);

    cmdline_options (const cmdline_options& opts);

    cmdline_options& operator = (const cmdline_options& opts);

    bool force_gui (void) const { return m_force_gui; }
    bool forced_interactive (void) const { return m_forced_interactive; }
    bool forced_line_editing (void) const { return m_forced_line_editing; }
    bool inhibit_startup_message (void) const { return m_inhibit_startup_message; }
    bool line_editing (void) const { return m_line_editing; }
    bool no_gui (void) const { return m_no_gui; }
    bool no_window_system (void) const { return m_no_window_system; }
    bool persist (void) const { return m_persist; }
    bool read_history_file (void) const { return m_read_history_file; }
    bool read_init_files (void) const { return m_read_init_files; }
    bool read_site_files (void) const { return m_read_site_files; }
    bool set_initial_path (void) const { return m_set_initial_path; }
    bool traditional (void) const { return m_traditional; }
    bool verbose_flag (void) const { return m_verbose_flag; }
    std::string code_to_eval (void) const { return m_code_to_eval; }
    std::list<std::string> command_line_path (void) const { return m_command_line_path; }
    std::string exec_path (void) const { return m_exec_path; }
    std::string image_path (void) const { return m_image_path; }
    string_vector all_args (void) const { return m_all_args; }
    string_vector remaining_args (void) const { return m_remaining_args; }

    void force_gui (bool arg) { m_force_gui = arg; }
    void forced_line_editing (bool arg) { m_forced_line_editing = arg; }
    void forced_interactive (bool arg) { m_forced_interactive = arg; }
    void inhibit_startup_message (bool arg) { m_inhibit_startup_message = arg; }
    void line_editing (bool arg) { m_line_editing = arg; }
    void no_gui (bool arg) { m_no_gui = arg; }
    void no_window_system (bool arg) { m_no_window_system = arg; }
    void persist (bool arg) { m_persist = arg; }
    void read_history_file (bool arg) { m_read_history_file = arg; }
    void read_init_files (bool arg) { m_read_init_files = arg; }
    void read_site_files (bool arg) { m_read_site_files = arg; }
    void set_initial_path (bool arg) { m_set_initial_path = arg; }
    void traditional (bool arg) { m_traditional = arg; }
    void verbose_flag (bool arg) { m_verbose_flag = arg; }
    void code_to_eval (const std::string& arg) { m_code_to_eval = arg; }
    void command_line_path (const std::list<std::string>& arg) { m_command_line_path = arg; }
    void exec_path (const std::string& arg) { m_exec_path = arg; }
    void image_path (const std::string& arg) { m_image_path = arg; }
    void all_args (const string_vector& arg) { m_all_args = arg; }
    void remaining_args (const string_vector& arg) { m_remaining_args = arg; }

  private:

    // If TRUE, force the GUI to start.
    // (--force-gui)
    bool m_force_gui = false;

    // TRUE means the user forced this shell to be interactive.
    // (--interactive, -i)
    bool m_forced_interactive = false;

    // If TRUE, force readline command line editing.
    // (--line-editing)
    bool m_forced_line_editing = false;

    // TRUE means we don't print the usual startup message.
    // (--quiet; --silent; -q)
    bool m_inhibit_startup_message = false;

    // TRUE means we are using readline.
    // (--no-line-editing)
    bool m_line_editing = true;

    // If TRUE don't start the GUI.
    // (--no-gui)
    bool m_no_gui = false;

    // If TRUE, ignore the window system even if it is available.
    // (--no-window-system, -W)
    bool m_no_window_system = false;

    // If TRUE, don't exit after evaluating code given by --eval option.
    // (--persist)
    bool m_persist = false;

    // If TRUE, initialize history list from saved history file.
    // (--no-history; -H)
    bool m_read_history_file = true;

    // TRUE means we read ~/.octaverc and ./.octaverc.
    // (--norc; --no-init-file; -f)
    bool m_read_init_files = true;

    // TRUE means we read the site-wide octaverc files.
    // (--norc; --no-site-file; -f)
    bool m_read_site_files = true;

    // TRUE means we set the initial path to configured defaults.
    // (--no-init-path)
    bool m_set_initial_path = true;

    // If TRUE use traditional (maximally MATLAB compatible) settings
    // (--traditional)
    bool m_traditional = false;

    // If TRUE, print verbose info in some cases.
    // (--verbose; -V)
    bool m_verbose_flag = false;

    // The code to evaluate at startup
    // (--eval CODE)
    std::string m_code_to_eval;

    // The value of "path" specified on the command line.
    // (--path; -p)
    std::list<std::string> m_command_line_path;

    // The value for "EXEC_PATH" specified on the command line.
    // (--exec-path)
    std::string m_exec_path;

    // The value for "IMAGE_PATH" specified on the command line.
    // (--image-path)
    std::string m_image_path;

    // All arguments passed to the argc, argv constructor.
    string_vector m_all_args;

    // Arguments remaining after parsing.
    string_vector m_remaining_args;
  };

  // The application object contains a pointer to the current
  // interpreter and the interpreter contains a pointer back to the
  // application context so we need a forward declaration for one (or
  // both) of them...

  class interpreter;

  // Base class for an Octave application.

  class OCTINTERP_API application
  {
  public:

    application (const cmdline_options& opts = cmdline_options ());

    application (int argc, char **argv);

    // No copying, at least not yet...

    application (const application&) = delete;

    application& operator = (const application&) = delete;

    virtual ~application (void);

    void set_program_names (const std::string& pname);

    void intern_argv (const string_vector& args);

    cmdline_options options (void) const { return m_options; }

    bool have_script_file (void) const { return m_have_script_file; }

    bool is_octave_program (void) const { return m_is_octave_program; }

    virtual void create_interpreter (void);

    virtual int execute_interpreter (void);

    virtual int execute (void) = 0;

    virtual bool gui_running (void) const { return false; }
    virtual void gui_running (bool) { }

    void program_invocation_name (const std::string& nm) { m_program_invocation_name = nm; }

    void program_name (const std::string& nm) { m_program_name = nm; }

    void forced_interactive (bool arg) { m_options.forced_interactive (arg); }

    void interactive (bool arg);

    // Should be an error if instance is 0.
    static application *app (void) { return instance; }

    static std::string program_invocation_name (void) { return instance->m_program_invocation_name; }

    static std::string program_name (void) { return instance->m_program_name; }

    static string_vector argv (void) { return instance->m_argv; }

    static bool is_gui_running (void) { return instance->gui_running (); }

    static interpreter *the_interpreter (void) { return instance->m_interpreter; }

    // Convenience functions.

    static bool forced_interactive (void);
    static bool interactive (void);

  private:

    // The application instance;  There should be only one.
    static application *instance;

    void init (void);

  protected:

    // The name used to invoke Octave.
    std::string m_program_invocation_name;

    // The last component of octave_program_invocation_name.
    std::string m_program_name;

    // The current argument vector (may change if we are running a
    // script with --persist because after the script is done, the
    // arguments revert to the full list given to the octave
    // interpreter at startup.
    string_vector m_argv;

    cmdline_options m_options;

    // TRUE if there is a command line argument that looks like the
    // name of a file to execute.
    bool m_have_script_file = false;

    // TRUE if this is a program and no interpreter and interaction is
    // needed.  For example, an octave program with shebang line, or code
    // from eval without persist.
    bool m_is_octave_program = false;

    // If TRUE, the GUI should be started.
    bool m_gui_running = false;

    interpreter *m_interpreter = 0;
  };

  class OCTINTERP_API cli_application : public application
  {
  public:

    cli_application (const cmdline_options& opts = cmdline_options ())
      : application (opts)
    { }

    cli_application (int argc, char **argv)
      : application (argc, argv)
    { }

    // No copying, at least not yet...

    cli_application (const cli_application&) = delete;

    cli_application& operator = (const cli_application&) = delete;

    ~cli_application (void) = default;

    int execute (void);
  };

  class OCTINTERP_API embedded_application : public application
  {
  public:

    embedded_application (const cmdline_options& opts = cmdline_options ())
      : application (opts)
    { }

    embedded_application (int argc, char **argv)
      : application (argc, argv)
    { }

    // No copying, at least not yet...

    embedded_application (const embedded_application&) = delete;

    embedded_application& operator = (const embedded_application&) = delete;

    ~embedded_application (void) = default;

    void create_interpreter (void);

    int execute (void);
  };
}

#endif

#if defined  (__cplusplus)
extern "C" {
#endif

extern OCTINTERP_API int octave_main (int argc, char **argv, int embedded);

#if defined  (__cplusplus)
}
#endif

#endif
