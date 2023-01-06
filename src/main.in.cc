// %NO_EDIT_WARNING%

////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2012-2023 The Octave Project Developers
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

// NOTE: This program is supposed to be a small wrapper that exists
// primarily to give up the controlling TTY and then exec Octave with
// its GUI.  It may also execute Octave without the GUI or the command
// line version of Octave that is not linked with GUI libraries.  So
// that it remains small, it should NOT depend on or be linked with
// liboctave or libinterp.

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <cstdlib>
#include <cstring>

#include <algorithm>
#include <iostream>
#include <string>

#if defined (OCTAVE_USE_WINDOWS_API) && defined (_UNICODE)
#  include <vector>
#  include <locale>
#  include <codecvt>
#endif

// We are linking against static libs so do not decorate with dllimport.
// FIXME: This should be done by the build system.
#undef OCTAVE_API
#define OCTAVE_API
#include "fcntl-wrappers.h"
#include "getopt-wrapper.h"
#include "signal-wrappers.h"
#include "unistd-wrappers.h"
#include "wait-wrappers.h"

#if ! defined (OCTAVE_VERSION)
#  define OCTAVE_VERSION %OCTAVE_VERSION%
#endif

#if ! defined (OCTAVE_ARCHLIBDIR)
#  define OCTAVE_ARCHLIBDIR %OCTAVE_ARCHLIBDIR%
#endif

#if ! defined (OCTAVE_BINDIR)
#  define OCTAVE_BINDIR %OCTAVE_BINDIR%
#endif

#if ! defined (OCTAVE_PREFIX)
#  define OCTAVE_PREFIX %OCTAVE_PREFIX%
#endif

#if ! defined (OCTAVE_EXEC_PREFIX)
#  define OCTAVE_EXEC_PREFIX %OCTAVE_EXEC_PREFIX%
#endif

#include "display-available.h"
#include "options.h"
#include "shared-fcns.h"

#if defined (HAVE_OCTAVE_QT_GUI) && ! defined (OCTAVE_USE_WINDOWS_API)
static bool fork_and_exec = true;
#else
static bool fork_and_exec = false;
#endif

// If we fork and exec, we'll need the following signal handling code to
// forward signals to the GUI process.

static pid_t gui_pid = 0;

static int caught_signal = -1;

static void
gui_driver_sig_handler (int sig)
{
  if (gui_pid > 0)
    caught_signal = sig;
}

static void
gui_driver_set_signal_handler (const char *signame,
                               octave_sig_handler *handler)
{
  octave_set_signal_handler_by_name (signame, handler, false);
}

static void
install_signal_handlers (void)
{
  // FIXME: do we need to handle and forward all the signals that Octave
  // handles, or is it sufficient to only forward things like SIGINT,
  // SIGBREAK, SIGABRT, SIGQUIT, and possibly a few others?

  gui_driver_set_signal_handler ("SIGINT", gui_driver_sig_handler);
  gui_driver_set_signal_handler ("SIGBREAK", gui_driver_sig_handler);
  gui_driver_set_signal_handler ("SIGABRT", gui_driver_sig_handler);
  gui_driver_set_signal_handler ("SIGALRM", gui_driver_sig_handler);
  gui_driver_set_signal_handler ("SIGBUS", gui_driver_sig_handler);

  // SIGCHLD
  // SIGCLD
  // SIGCONT

  gui_driver_set_signal_handler ("SIGEMT", gui_driver_sig_handler);
  gui_driver_set_signal_handler ("SIGFPE", gui_driver_sig_handler);
  gui_driver_set_signal_handler ("SIGHUP", gui_driver_sig_handler);
  gui_driver_set_signal_handler ("SIGILL", gui_driver_sig_handler);

  // SIGINFO
  // SIGINT

  gui_driver_set_signal_handler ("SIGIOT", gui_driver_sig_handler);
  gui_driver_set_signal_handler ("SIGLOST", gui_driver_sig_handler);
  gui_driver_set_signal_handler ("SIGPIPE", gui_driver_sig_handler);
  gui_driver_set_signal_handler ("SIGPOLL", gui_driver_sig_handler);

  // SIGPROF
  // SIGPWR

  gui_driver_set_signal_handler ("SIGQUIT", gui_driver_sig_handler);
  gui_driver_set_signal_handler ("SIGSEGV", gui_driver_sig_handler);

  // SIGSTOP

  gui_driver_set_signal_handler ("SIGSYS", gui_driver_sig_handler);
  gui_driver_set_signal_handler ("SIGTERM", gui_driver_sig_handler);
  gui_driver_set_signal_handler ("SIGTRAP", gui_driver_sig_handler);

  // SIGTSTP
  // SIGTTIN
  // SIGTTOU
  // SIGURG

  gui_driver_set_signal_handler ("SIGUSR1", gui_driver_sig_handler);
  gui_driver_set_signal_handler ("SIGUSR2", gui_driver_sig_handler);
  gui_driver_set_signal_handler ("SIGVTALRM", gui_driver_sig_handler);
  gui_driver_set_signal_handler ("SIGIO", gui_driver_sig_handler);

  // SIGWINCH

  gui_driver_set_signal_handler ("SIGXCPU", gui_driver_sig_handler);
  gui_driver_set_signal_handler ("SIGXFSZ", gui_driver_sig_handler);
}

static std::string
get_octave_bindir (void)
{
  // Accept value from the environment literally, but substitute
  // OCTAVE_HOME in the configuration value OCTAVE_BINDIR in case Octave
  // has been relocated to some installation directory other than the
  // one originally configured.

  std::string obd = octave_getenv ("OCTAVE_BINDIR");

  return obd.empty () ? prepend_octave_exec_home (std::string (OCTAVE_BINDIR))
                      : obd;
}

static std::string
get_octave_archlibdir (void)
{
  // Accept value from the environment literally, but substitute
  // OCTAVE_HOME in the configuration value OCTAVE_ARCHLIBDIR in case
  // Octave has been relocated to some installation directory other than
  // the one originally configured.

  std::string dir = octave_getenv ("OCTAVE_ARCHLIBDIR");

  return dir.empty () ? prepend_octave_exec_home (std::string (OCTAVE_ARCHLIBDIR))
                      : dir;
}

static int
octave_exec (const std::string& file, char **argv)
{
  int status = octave_execv_wrapper (file.c_str (), argv);

#if defined (OCTAVE_USE_WINDOWS_API)
  // The above wrapper uses spawn(P_WAIT,...) instead of exec on Windows.
  if (status == -1)
#endif
  std::cerr << argv[0] << ": failed to exec '" << file << "'" << std::endl;

  return status;
}

static char *
strsave (const char *s)
{
  if (! s)
    return nullptr;

  int len = strlen (s);
  char *tmp = new char [len+1];
  tmp = strcpy (tmp, s);
  return tmp;
}

#if defined (OCTAVE_USE_WINDOWS_API) && defined (_UNICODE)
extern "C"
int
wmain (int argc, wchar_t **wargv)
{
  static char **argv = new char * [argc + 1];
  std::vector<std::string> argv_str;

  // convert wide character strings to multibyte UTF-8 strings
  std::wstring_convert<std::codecvt_utf8<wchar_t>, wchar_t> wchar_conv;
  for (int i_arg = 0; i_arg < argc; i_arg++)
    argv_str.push_back (wchar_conv.to_bytes (wargv[i_arg]));

  // Get pointers to C strings not before vector is stable.
  for (int i_arg = 0; i_arg < argc; i_arg++)
    argv[i_arg] = &argv_str[i_arg][0];
  argv[argc] = nullptr;

#else
int
main (int argc, char **argv)
{
#endif
  int retval = 0;

  int idx_gui = -1;
  bool server = false;
  bool start_gui = false;
  bool gui_libs = true;

  bool eval_code = false;
  bool persist_octave = false;

  set_octave_home ();

  std::string octave_bindir = get_octave_bindir ();
  std::string octave_archlibdir = get_octave_archlibdir ();
  std::string octave_cli
    = octave_bindir + dir_sep_char + "octave-cli-" OCTAVE_VERSION;
  std::string octave_gui = octave_archlibdir + dir_sep_char + "octave-gui";

#if defined (HAVE_OCTAVE_QT_GUI)
  // The Octave version number is already embedded in the
  // octave_archlibdir directory name so we don't need to append it to
  // the octave-gui filename.

  std::string file = octave_gui;
#else
  std::string file = octave_cli;
#endif

  // Declaring new_argv static avoids leak warnings when using GCC's
  // --address-sanitizer option.
  static char **new_argv = new char * [argc + 2];

  int next_optind = 1;
  int k = 1;

  bool warn_display = true;
  bool no_display = false;

  // Disable error reporting in getopt.  We want to silently recognize
  // and process a few special arguments here and pass everything on to
  // the real Octave program where incorrect usage errors may be
  // reported.

  octave_set_opterr_wrapper (0);

  while (true)
    {
      int long_idx;

      int optc = octave_getopt_long_wrapper (argc, argv, short_opts, long_opts,
                                             &long_idx);
      int old_optind = next_optind;
      next_optind = octave_optind_wrapper ();

      if (optc < 0)
        break;

      switch (optc)
        {
          case NO_GUI_LIBS_OPTION:
            // Run the version of Octave that is not linked with any GUI
            // libraries.  It may not be possible to do plotting or any ui*
            // calls, but it will be a little faster to start and require less
            // memory.  Don't pass the --no-gui-libs option on as that option
            // is not recognized by Octave.
            gui_libs = false;
            file = octave_cli;
            break;

          case NO_GUI_OPTION:
            // If we see this option, then we can just exec octave; we don't
            // have to create a child process and wait for it to exit.  But do
            // exec "octave-gui", not "octave-cli", because even if the
            // --no-gui option is given, we may be asked to do some plotting or
            // ui* calls.
            start_gui = false;
            new_argv[k++] = argv[old_optind];
            break;

          case GUI_OPTION:
            // If we see this option, then we fork and exec octave with the
            // --gui option, while continuing to handle signals in the
            // terminal.
            // Do not copy the arg now, since we still not know if the gui
            // should really be launched.  Just store the index.
            start_gui = true;
            idx_gui = old_optind;
            break;

          case EXPERIMENTAL_TERMINAL_WIDGET_OPTION:
            // If we see this option, then we don't fork and exec.
            fork_and_exec = false;
            new_argv[k++] = argv[old_optind];
            break;

          case PERSIST_OPTION:
            // FIXME: How can we reliably detect if this option appears after
            //        a FILE argument.  In this case octave ignores the option,
            //        but the GUI might still be launched if --gui is also
            //        given.
            persist_octave = true;
            new_argv[k++] = argv[old_optind];
            break;

          case SERVER_OPTION:
            server = true;
            new_argv[k++] = argv[old_optind];
            break;

          case EVAL_OPTION:
            eval_code = true;
            for (int i = old_optind; i < next_optind; i++)
              new_argv[k++] = argv[i];
            break;

          case 'q':
            // options "--silent" or "--quiet"
            warn_display = false;
            new_argv[k++] = argv[old_optind];
            break;

          case 'W':
            // option "--no-window-system"
            no_display = true;
            new_argv[k++] = argv[old_optind];
            break;

          default:
            for (int i = old_optind; i < next_optind; i++)
              new_argv[k++] = argv[i];
            break;
        }
    }

  // Treat trailing arguments as commands to be executed
  if (next_optind < argc)
    {
      eval_code = true;
      for (int i = next_optind; i < argc; i++)
        new_argv[k++] = argv[i];
    }

  if (start_gui && eval_code && ! persist_octave)
    start_gui = false;

  // At this point, we definitely know whether the gui has to
  // be launched or not.
  // gui_libs and start_gui are just about options, not
  // the environment.  Exit if they don't make sense.
  if (start_gui)
    {
      // GUI should be started
      if (! gui_libs)
        {
          std::cerr << "octave: conflicting options: --no-gui-libs and --gui"
                    << std::endl;
          return 1;
        }

      if (server)
        {
          std::cerr << "octave: conflicting options: --server and --gui"
                    << std::endl;
          return 1;
        }

#if ! defined (HAVE_OCTAVE_QT_GUI)
      std::cerr << "octave: GUI features missing or disabled in this build"
                << std::endl;
      return 1;
#endif

      // Finally, add --gui to the command line options. We can not
      // just append it since options after a given file are ignored.
      for (int j = k; j > 1; j--)
        new_argv[j] = new_argv[j-1];

      new_argv[1] = argv[idx_gui];
      k++;
    }

  new_argv[k] = nullptr;

  if (no_display)
    {
      start_gui = false;
      gui_libs = false;

      file = octave_cli;
    }
  else if (gui_libs || start_gui)
    {
      int dpy_avail;

      const char *display_check_err_msg = display_available (&dpy_avail);

      if (! dpy_avail)
        {
          start_gui = false;
          gui_libs = false;

          file = octave_cli;

          if (warn_display)
            {
              if (! display_check_err_msg)
                display_check_err_msg = "graphical display unavailable";

              std::cerr << "octave: " << display_check_err_msg << std::endl;
              std::cerr << "octave: disabling GUI features" << std::endl;
            }
        }
    }

#if defined (OCTAVE_USE_WINDOWS_API)
  file += ".exe";
#endif

  new_argv[0] = strsave (file.c_str ());

  // The Octave interpreter may be multithreaded.  If so, we attempt to
  // ensure that signals are delivered to the main interpreter thread
  // and no others by blocking signals before we exec the Octave
  // interpreter executable.  When that process starts, it will unblock
  // signals in the main interpreter thread.  When running the GUI as a
  // subprocess, we also unblock signals that the parent process handles
  // so we can forward them to the child.

  octave_block_async_signals ();
  octave_block_signal_by_name ("SIGTSTP");

  if (fork_and_exec && gui_libs && start_gui)
    {
      // Fork and exec when starting the GUI so that we will call
      // setsid to give up the controlling terminal (if any) and so that
      // the GUI process will be in a separate process group.
      //
      // The GUI process must be in a separate process group so that we
      // can send an interrupt signal to all child processes when
      // interrupting the interpreter.  See also bug #49609 and the
      // function pthread_thread_manager::interrupt in the file
      // libgui/src/thread-manager.cc.

      gui_pid = octave_fork_wrapper ();

      if (gui_pid < 0)
        {
          std::cerr << "octave: fork failed!" << std::endl;

          retval = 1;
        }
      else if (gui_pid == 0)
        {
          // Child.

          if (octave_setsid_wrapper () < 0)
            {
              std::cerr << "octave: error calling setsid!" << std::endl;

              retval = 1;
            }
          else
            retval = octave_exec (file, new_argv);
        }
      else
        {
          // Parent.  Forward signals to child while waiting for it to exit.

          install_signal_handlers ();

          octave_unblock_async_signals ();
          octave_unblock_signal_by_name ("SIGTSTP");

          int status;

          while (true)
            {
              octave_waitpid_wrapper (gui_pid, &status, 0);

              if (caught_signal > 0)
                {
                  int sig = caught_signal;

                  caught_signal = -1;

                  octave_kill_wrapper (gui_pid, sig);
                }
              else if (octave_wifexited_wrapper (status))
                {
                  retval = octave_wexitstatus_wrapper (status);
                  break;
                }
              else if (octave_wifsignaled_wrapper (status))
                {
                  std::cerr << "octave exited with signal "
                            << octave_wtermsig_wrapper (status) << std::endl;
                  break;
                }
            }
        }
    }
  else
    {
      retval = octave_exec (file, new_argv);

      if (retval < 0)
        std::cerr << argv[0] << ": " << std::strerror (errno) << std::endl;
    }


  return retval;
}
