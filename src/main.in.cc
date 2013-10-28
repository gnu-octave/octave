// %NO_EDIT_WARNING%
/*

Copyright (C) 2012-2013 John W. Eaton

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

// NOTE: This program is supposed to be a small wrapper that exists
// primarily to give up the controlling TTY and then exec Octave with
// its GUI.  It may also execute Octave without the GUI or the command
// line version of Octave that is not linked with GUI libraries.  So
// that it remains small, it should NOT depend on or be linked with
// liboctave or libinterp.

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cstdlib>
#include <cstring>

#include <algorithm>
#include <iostream>
#include <string>

// From gnulib, so OK for Windows too.
#include <unistd.h>

#if defined (__WIN32__) && ! defined (_POSIX_VERSION)

#define WIN32_LEAN_AND_MEAN
#include <tlhelp32.h>

static std::string
w32_get_octave_home (void)
{
  std::string retval;

  std::string bin_dir;

  HANDLE h = CreateToolhelp32Snapshot (TH32CS_SNAPMODULE
#ifdef TH32CS_SNAPMODULE32
                                       | TH32CS_SNAPMODULE32
#endif
                                       , 0);

  if (h != INVALID_HANDLE_VALUE)
    {
      MODULEENTRY32 mod_info;

      ZeroMemory (&mod_info, sizeof (mod_info));
      mod_info.dwSize = sizeof (mod_info);

      if (Module32First (h, &mod_info))
        {
          do
            {
              std::string mod_name (mod_info.szModule);

              if (mod_name.find ("octave") != std::string::npos)
                {
                  bin_dir = mod_info.szExePath;

                  if (bin_dir[bin_dir.length () - 1] != '\\')
                    bin_dir.append (1, '\\');

                  break;
                }
            }
          while (Module32Next (h, &mod_info));
       }

      CloseHandle (h);
    }

  if (! bin_dir.empty ())
    {
      size_t pos = bin_dir.rfind ("\\bin\\");

      if (pos != std::string::npos)
        retval = bin_dir.substr (0, pos);
    }

  return retval;
}

#endif

#if (defined (HAVE_OCTAVE_GUI) \
     && ! defined (__WIN32__) && ! defined (__CYGWIN__))

#include <sys/types.h>
#include <signal.h>
#include <fcntl.h>

// This is a liboctave header, but it doesn't include any other Octave
// headers or declare any functions that are defined in liboctave.
#include "syswait.h"

typedef void sig_handler (int);

// Forward signals to the GUI process.

static pid_t gui_pid = 0;

static void
gui_driver_sig_handler (int sig)
{
  if (gui_pid > 0)
    kill (gui_pid, sig);
}

static sig_handler *
octave_set_signal_handler (int sig, sig_handler *handler)
{
  struct sigaction act, oact;

  act.sa_handler = handler;
  act.sa_flags = 0;

  gnulib::sigemptyset (&act.sa_mask);
  gnulib::sigemptyset (&oact.sa_mask);

  gnulib::sigaction (sig, &act, &oact);

  return oact.sa_handler;
}

static void
install_signal_handlers (void)
{

#ifdef SIGINT
  octave_set_signal_handler (SIGINT, gui_driver_sig_handler);
#endif

#ifdef SIGBREAK
  octave_set_signal_handler (SIGBREAK, gui_driver_sig_handler);
#endif

#ifdef SIGABRT
  octave_set_signal_handler (SIGABRT, gui_driver_sig_handler);
#endif

#ifdef SIGALRM
  octave_set_signal_handler (SIGALRM, gui_driver_sig_handler);
#endif

#ifdef SIGBUS
  octave_set_signal_handler (SIGBUS, gui_driver_sig_handler);
#endif

  // SIGCHLD
  // SIGCLD
  // SIGCONT

#ifdef SIGEMT
  octave_set_signal_handler (SIGEMT, gui_driver_sig_handler);
#endif

#ifdef SIGFPE
  octave_set_signal_handler (SIGFPE, gui_driver_sig_handler);
#endif

#ifdef SIGHUP
  octave_set_signal_handler (SIGHUP, gui_driver_sig_handler);
#endif

#ifdef SIGILL
  octave_set_signal_handler (SIGILL, gui_driver_sig_handler);
#endif

  // SIGINFO
  // SIGINT

#ifdef SIGIOT
  octave_set_signal_handler (SIGIOT, gui_driver_sig_handler);
#endif

#ifdef SIGLOST
  octave_set_signal_handler (SIGLOST, gui_driver_sig_handler);
#endif

#ifdef SIGPIPE
  octave_set_signal_handler (SIGPIPE, gui_driver_sig_handler);
#endif

#ifdef SIGPOLL
  octave_set_signal_handler (SIGPOLL, gui_driver_sig_handler);
#endif

  // SIGPROF
  // SIGPWR

#ifdef SIGQUIT
  octave_set_signal_handler (SIGQUIT, gui_driver_sig_handler);
#endif

#ifdef SIGSEGV
  octave_set_signal_handler (SIGSEGV, gui_driver_sig_handler);
#endif

  // SIGSTOP

#ifdef SIGSYS
  octave_set_signal_handler (SIGSYS, gui_driver_sig_handler);
#endif

#ifdef SIGTERM
  octave_set_signal_handler (SIGTERM, gui_driver_sig_handler);
#endif

#ifdef SIGTRAP
  octave_set_signal_handler (SIGTRAP, gui_driver_sig_handler);
#endif

  // SIGTSTP
  // SIGTTIN
  // SIGTTOU
  // SIGURG

#ifdef SIGUSR1
  octave_set_signal_handler (SIGUSR1, gui_driver_sig_handler);
#endif

#ifdef SIGUSR2
  octave_set_signal_handler (SIGUSR2, gui_driver_sig_handler);
#endif

#ifdef SIGVTALRM
  octave_set_signal_handler (SIGVTALRM, gui_driver_sig_handler);
#endif

#ifdef SIGIO
  octave_set_signal_handler (SIGIO, gui_driver_sig_handler);
#endif

  // SIGWINCH

#ifdef SIGXCPU
  octave_set_signal_handler (SIGXCPU, gui_driver_sig_handler);
#endif

#ifdef SIGXFSZ
  octave_set_signal_handler (SIGXFSZ, gui_driver_sig_handler);
#endif

}

static int
have_controlling_terminal (void)
{
  int retval = 0;

#if ! (defined (__WIN32__) || defined (__APPLE__)) || defined (__CYGWIN__)

#if defined (HAVE_CTERMID)
  const char *ctty = ctermid (0);
#else
  const char *ctty = "/dev/tty";
#endif

  int fd = gnulib::open (ctty, O_RDWR, 0);

  if (fd >= 0)
    {
      gnulib::close (fd);

      retval = 1;
    }

#endif

  return retval;
}

#endif

#ifndef OCTAVE_BINDIR
#define OCTAVE_BINDIR %OCTAVE_BINDIR%
#endif

#ifndef OCTAVE_PREFIX
#define OCTAVE_PREFIX %OCTAVE_PREFIX%
#endif

// Find the directory where the octave binary is supposed to be
// installed.

#if (defined (OCTAVE_HAVE_WINDOWS_FILESYSTEM) \
     && ! defined (OCTAVE_HAVE_POSIX_FILESYSTEM))
static const char dir_sep_char = '\\';
#else
static const char dir_sep_char = '/';
#endif

static std::string
octave_getenv (const std::string& name)
{
  char *value = ::getenv (name.c_str ());

  return value ? value : "";
}

static std::string
get_octave_home (void)
{
  std::string oh = octave_getenv ("OCTAVE_HOME");

#if defined (__WIN32__) && ! defined (_POSIX_VERSION)
  if (oh.empty ())
    oh = w32_get_octave_home ();
#endif

  return oh.empty () ? std::string (OCTAVE_PREFIX) : oh;
}

static std::string
subst_octave_home (const std::string& s)
{
  std::string retval;

  std::string octave_home = get_octave_home ();

  std::string prefix = OCTAVE_PREFIX;

  retval = s;

  if (octave_home != prefix)
    {
      octave_idx_type len = prefix.length ();

      if (s.substr (0, len) == prefix)
        retval.replace (0, len, octave_home);
    }

  if (dir_sep_char != '/')
    std::replace (retval.begin (), retval.end (), '/', dir_sep_char);

  return retval;
}

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

static int
octave_exec (const std::string& file, char **argv)
{
  execv (file.c_str (), argv);

  std::cerr << "octave: failed to exec '" << file << "'" << std::endl;

  return 1;
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

#if defined (HAVE_OCTAVE_GUI)
  bool start_gui = true;
  bool cli_only = false;
#endif

  std::string octave_bindir = get_octave_bindir ();

  std::string file = octave_bindir + dir_sep_char;

#if defined (HAVE_OCTAVE_GUI)
  file += "octave-gui";
#else
  file += "octave-cli";
#endif

  char **new_argv = new char * [argc + 1];

  int k = 0;
  new_argv[k++] = strsave ("octave");

  for (int i = 1; i < argc; i++)
    {
      if (! strcmp (argv[i], "--no-gui-libs"))
        {
          // Run the version of Octave that is not linked with any GUI
          // libraries.  It may not be possible to do plotting or any
          // ui* calls, but it will be a little faster to start and
          // require less memory.  Don't pass the --no-gui-libs option
          // on as that option is not recognized by Octave.

#if defined (HAVE_OCTAVE_GUI)
          cli_only = true;
#endif
          file = octave_bindir + dir_sep_char + "octave-cli";
        }
      else if (! strcmp (argv[i], "--no-gui"))
        {
          // If we see this option, then we can just exec octave; we
          // don't have to create a child process and wait for it to
          // exit.  But do exec "octave", not "octave-cli", because even
          // if the --no-gui option is given, we may be asked to do some
          // plotting or ui* calls.

#if defined (HAVE_OCTAVE_GUI)
          start_gui = false;
#endif
          new_argv[k++] = argv[i];
        }
      else
        new_argv[k++] = argv[i];
    }

  new_argv[k] = 0;

#if ! defined (HAVE_OCTAVE_GUI) || defined (__WIN32__) || defined (__CYGWIN__)

  retval = octave_exec (file, new_argv);

#else

  if (cli_only || (! start_gui && ! have_controlling_terminal ()))
    {
      retval = octave_exec (file, new_argv);
    }
  else
    {
      install_signal_handlers ();

      gui_pid = fork ();

      if (gui_pid < 0)
        {
          std::cerr << "octave: fork failed!" << std::endl;

          retval = 1;
        }
      else if (gui_pid == 0)
        {
          // Child.

          if (setsid () < 0)
            {
              std::cerr << "octave: error calling setsid!" << std::endl;

              retval = 1;
            }

          retval = octave_exec (file, new_argv);
        }
      else
        {
          // Parent.  Forward signals to the child while waiting for it
          // to exit.

          int status;

          while (1)
            {
              WAITPID (gui_pid, &status, 0);

              if (WIFEXITED (status))
                {
                  retval = WIFEXITED (status) ? WEXITSTATUS (status) : 127;

                  break;
                }
            }
        }
    }

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
