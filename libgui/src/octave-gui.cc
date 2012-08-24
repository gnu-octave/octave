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
#include <QSettings>

#include <iostream>

#include <syswait.h>

#include "welcome-wizard.h"
#include "resource-manager.h"
#include "main-window.h"
#include "octave-gui.h"

// Dissociate from the controlling terminal, if any.

static void
dissociate_terminal (void)
{
#if ! defined (Q_OS_WIN32) || defined (Q_OS_CYGWIN)

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

      int status;

      waitpid (pid, &status, 0);

      exit (WIFEXITED (status) ? WEXITSTATUS (status) : 127);
    }

#endif
}

int
octave_start_gui (int argc, char *argv[])
{
  dissociate_terminal ();

  QApplication application (argc, argv);

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
          QSettings *settings = resource_manager::get_settings ();

          // FIXME -- what should happen if settings is 0?

          QString language = settings->value ("language").toString ();

          QString translatorFile = resource_manager::find_translator_file (language);
          QTranslator translator;
          translator.load (translatorFile);
          application.installTranslator (&translator);

          resource_manager::update_network_settings ();

          main_window w;
          w.show ();
          w.focus_command_window ();

          return application.exec ();
        }
    }
}
