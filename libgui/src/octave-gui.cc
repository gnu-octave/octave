/*

Copyright (C) 2011-2012 Jacob Dawid

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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <QtGui/QApplication>
#include <QTranslator>

#include <iostream>

#include <unistd.h>
#include <fcntl.h>

#if defined (HAVE_SYS_IOCTL_H)
#include <sys/ioctl.h>
#endif

#include "lo-utils.h"
#include "oct-env.h"
#include "oct-syscalls.h"
#include "syswait.h"

#include "sighandlers.h"

#include "welcome-wizard.h"
#include "resource-manager.h"
#include "main-window.h"
#include "octave-gui.h"

// Dissociate from the controlling terminal, if any.

static void
dissociate_terminal (void)
{
#if ! (defined (__WIN32__) || defined (__APPLE__)) || defined (__CYGWIN__)
 
  pid_t pid = fork ();

  if (pid < 0)
    {
      std::cerr << "fork failed!" << std::endl;;
      exit (1);
    }
  else if (pid == 0)
    {
      // Child.

      if (setsid () < 0)
        {
          std::cerr << "setsid error" << std::endl;
          exit (1);
        }
    }
  else
    {
      // Parent

      install_gui_driver_signal_handlers (pid);

      int status;

      waitpid (pid, &status, 0);

      exit (octave_wait::ifexited (status)
            ? octave_wait::exitstatus (status) : 127);
    }

#endif
}

int
octave_start_gui (int argc, char *argv[], bool fork)
{
  if (fork)
    dissociate_terminal ();

  QApplication application (argc, argv);

  // install translators for the gui and qt text
  QTranslator gui_tr, qt_tr, qsci_tr;
  resource_manager::config_translators (&qt_tr,&qsci_tr,&gui_tr);
  application.installTranslator (&qt_tr);
  application.installTranslator (&qsci_tr);
  application.installTranslator (&gui_tr);

  while (true)
    {
      if (resource_manager::is_first_run ())
        {
          welcome_wizard welcomeWizard;
          welcomeWizard.exec ();
          resource_manager::reload_settings ();
        }
      else
        {
          // update network-settings
          resource_manager::update_network_settings ();

#if ! defined (__WIN32__) || defined (__CYGWIN__)
          // If we were started from a launcher, TERM might not be
          // defined, but we provide a terminal with xterm
          // capabilities.

          std::string term = octave_env::getenv ("TERM");

          if (term.empty ())
            octave_env::putenv ("TERM", "xterm");
#else
          std::string term = octave_env::getenv ("TERM");

          if (term.empty ())
            octave_env::putenv ("TERM", "cygwin");
#endif

          // create main window, read settings, and show window
          main_window w;
          w.read_settings ();  // get widget settings and window layout
          w.focus_command_window ();
          w.connect_visibility_changed (); // connect signals for changes in
                                           // visibility not before w is shown
          return application.exec ();
        }
    }
}
