// %NO_EDIT_WARNING%
/*

Copyright (C) 2012-2016 John W. Eaton

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

#include "fcntl-wrappers.h"
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

#include "display-available.h"
#include "shared-fcns.h"

#if defined (HAVE_OCTAVE_QT_GUI) && ! defined (OCTAVE_USE_WINDOWS_API)

// Forward signals to the GUI process.

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

#endif

static std::string
get_octave_bindir (void)
{
  // Accept value from the environment literally, but substitute
  // OCTAVE_HOME in the configuration value OCTAVE_BINDIR in case Octave
  // has been relocated to some installation directory other than the
  // one originally configured.

  std::string obd = octave_getenv ("OCTAVE_BINDIR");

  return obd.empty () ? subst_octave_home (std::string (OCTAVE_BINDIR)) : obd;
}

static std::string
get_octave_archlibdir (void)
{
  // Accept value from the environment literally, but substitute
  // OCTAVE_HOME in the configuration value OCTAVE_ARCHLIBDIR in case
  // Octave has been relocated to some installation directory other than
  // the one originally configured.

  std::string dir = octave_getenv ("OCTAVE_ARCHLIBDIR");

  return dir.empty () ? subst_octave_home (std::string (OCTAVE_ARCHLIBDIR))
                      : dir;
}

static int
octave_exec (const std::string& file, char **argv)
{
  int status = octave_execv_wrapper (file.c_str (), argv);

  std::cerr << argv[0] << ": failed to exec '" << file << "'" << std::endl;

  return status;
}

static char *
strsave (const char *s)
{
  if (! s)
    return 0;

  int len = strlen (s);
  char *tmp = new char [len+1];
  tmp = strcpy (tmp, s);
  return tmp;
}

int
main (int argc, char **argv)
{
  int retval = 0;

  bool start_gui = true;
  bool gui_libs = true;

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
  static char **new_argv = new char * [argc + 1];

  int k = 1;

  bool warn_display = true;
  bool no_display = false;

  for (int i = 1; i < argc; i++)
    {
      if (! strcmp (argv[i], "--no-gui-libs"))
        {
          // Run the version of Octave that is not linked with any GUI
          // libraries.  It may not be possible to do plotting or any
          // ui* calls, but it will be a little faster to start and
          // require less memory.  Don't pass the --no-gui-libs option
          // on as that option is not recognized by Octave.

          start_gui = false;
          gui_libs = false;
          file = octave_cli;
        }
      else if (! strcmp (argv[i], "--no-gui"))
        {
          // If we see this option, then we can just exec octave; we
          // don't have to create a child process and wait for it to
          // exit.  But do exec "octave-gui", not "octave-cli", because
          // even if the --no-gui option is given, we may be asked to do
          // some plotting or ui* calls.

          start_gui = false;
          new_argv[k++] = argv[i];
        }
      else if (! strcmp (argv[i], "--silent") || ! strcmp (argv[i], "--quiet")
               || ! strcmp (argv[i], "-q"))
        {
          warn_display = false;
          new_argv[k++] = argv[i];
        }
      else if (! strcmp (argv[i], "--no-window-system")
               || ! strcmp (argv[i], "-W"))
        {
          no_display = true;
          new_argv[k++] = argv[i];
        }
      else
        new_argv[k++] = argv[i];
    }

  new_argv[k] = 0;

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

#if defined (HAVE_OCTAVE_QT_GUI) && ! defined (OCTAVE_USE_WINDOWS_API)

  if (gui_libs && start_gui)
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

      install_signal_handlers ();

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

#else

  retval = octave_exec (file, new_argv);

  if (retval < 0)
    std::cerr << argv[0] << ": " << std::strerror (errno) << std::endl;

#endif

  return retval;
}

/*!
@mainpage Source code documentation for GNU Octave

GNU Octave is a high-level language, primarily intended for numerical
computations.  It provides a convenient interactive command line
interface for solving linear and nonlinear problems numerically, and
for performing other numerical experiments.  It may also be used as a
batch-oriented language for data processing.

GNU Octave is free software. You may redistribute it and/or modify it
under the terms of the <a href="http://www.gnu.org/licenses/">GNU
General Public License</a> as published by the Free Software Foundation.

This is the developer documentation for Octave's own source code. It is
intended to help for hacking Octave. It may also be useful for
understanding the Octave API when writing your own .oct files.
*/
