/*

Copyright (C) 2011-2013 Jacob Dawid

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

#include <QApplication>
#include <QCoreApplication>
#include <QTextCodec>
#include <QThread>
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

#include "octave.h"
#include "sighandlers.h"

#include "welcome-wizard.h"
#include "resource-manager.h"
#include "main-window.h"
#include "octave-gui.h"
#include "thread-manager.h"

// Allow the Octave interpreter to start as in CLI mode with a
// QApplication context so that it can use Qt for things like plotting
// and UI widgets.

class octave_cli_thread : public QThread
{
public:

  octave_cli_thread (int argc, char **argv)
    : m_argc (argc), m_argv (argv), m_result (0) { }

  int result (void) const { return m_result; }

protected:

  void run (void)
  {
    octave_thread_manager::unblock_interrupt_signal ();

    octave_initialize_interpreter (m_argc, m_argv, 0);

    m_result = octave_execute_interpreter ();

    QCoreApplication::exit (m_result);
  }

private:

  int m_argc;
  char** m_argv;
  int m_result;
};


// Custom message handler for filtering some messages from Qt.

void message_handler (QtMsgType type, const char *msg)
{
  switch (type)
    {
    case QtDebugMsg:
      if (strncmp (msg, "QFileSystemWatcher: skipping native engine",42) != 0)
        std::cerr << "Debug: " << msg << std::endl;
      break;

    case QtWarningMsg:
      std::cerr << "Warning: " << msg << std::endl;
      break;

    case QtCriticalMsg:
      std::cerr << "Critical: " << msg << std::endl;
      break;

    case QtFatalMsg:
      std::cerr << "Fatal: " << msg << std::endl;
      abort ();

    default:
      break;
    }
}

// If START_GUI is false, we still set up the QApplication so that we
// can use Qt widgets for plot windows.

int
octave_start_gui (int argc, char *argv[], bool start_gui)
{
  octave_thread_manager::block_interrupt_signal ();

  qInstallMsgHandler (message_handler);

  if (start_gui)
    {
      QApplication application (argc, argv);

      // Set the codec for all strings
      QTextCodec::setCodecForCStrings (QTextCodec::codecForName ("UTF-8"));

      if (resource_manager::is_first_run ())
        {
          welcome_wizard welcomeWizard;

          if (welcomeWizard.exec () == QDialog::Rejected)
            exit (1);
        }

      resource_manager::reload_settings ();

      // install translators for the gui and qt text
      QTranslator gui_tr, qt_tr, qsci_tr;

      resource_manager::config_translators (&qt_tr, &qsci_tr, &gui_tr);

      application.installTranslator (&qt_tr);
      application.installTranslator (&qsci_tr);
      application.installTranslator (&gui_tr);

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

      // Create and show main window.

      main_window w;

      w.read_settings ();

      w.focus_command_window ();

      // Connect signals for changes in visibility not before w
      // is shown.

      w.connect_visibility_changed ();

      return application.exec ();
    }
  else
    {
      QCoreApplication application (argc, argv);

      octave_cli_thread main_thread (argc, argv);

      main_thread.start ();

      return application.exec ();
    }
}
