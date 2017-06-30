/*

Copyright (C) 2011-2017 Jacob Dawid

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

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <QApplication>
#include <QTextCodec>
#include <QThread>
#include <QTranslator>
#include <QtGlobal>
#include <QStyleFactory>

#include <cstdio>

#include <iostream>

#if defined (HAVE_SYS_IOCTL_H)
#  include <sys/ioctl.h>
#endif

#include "lo-utils.h"
#include "oct-env.h"
#include "oct-syscalls.h"

#include "builtin-defun-decls.h"
#include "defaults.h"
#include "display.h"
#include "octave.h"
#include "sysdep.h"
#include "unistd-wrappers.h"

#include "main-window.h"
#include "octave-gui.h"
#include "resource-manager.h"
#include "shortcut-manager.h"
#include "thread-manager.h"
#include "welcome-wizard.h"

// Disable all Qt messages by default.

static void
#if defined (HAVE_QT4)
message_handler (QtMsgType, const char *)
#else
message_handler (QtMsgType, const QMessageLogContext &, const QString &)
#endif
{ }

namespace octave
{
  gui_application::gui_application (int argc, char **argv)
    : application (argc, argv), m_argc (argc), m_argv (argv),
      m_gui_running (false)
  {
    // This should probably happen early.
    sysdep_init ();
  }

  bool gui_application::start_gui_p (void) const
  {
    if (m_options.no_window_system ())
      return false;

    std::string err_msg;
    if (! display_info::display_available (err_msg))
      {
        if (! (m_options.inhibit_startup_message () || err_msg.empty ()))
          warning (err_msg.c_str ());

        return false;
      }

    if (! m_options.line_editing ())
      {
        if (! (m_options.inhibit_startup_message ()
               || m_options.no_gui ()))
          warning ("--no-line-editing option given, disabling GUI");

        return false;
      }

    if (m_options.force_gui ())
      return true;

    if (m_options.no_gui ())
      return false;

    if (m_options.persist ())
      return true;

    // If stdin is not a tty, then assume we are reading commands from a
    // pipe or a redirected file and the GUI should not start.  If this
    // is not the case (for example, starting from a desktop "launcher"
    // with no terminal) and you want to start the GUI, you may use the
    // --force-gui option to start the GUI.

    if (! octave_isatty_wrapper (fileno (stdin)))
      return false;

    // If we have code to eval or execute from a file, and we are going
    // to exit immediately after executing it, don't start the gui.

    std::string code_to_eval = m_options.code_to_eval ();
    if (! code_to_eval.empty () || m_have_script_file)
      return false;

    return true;
  }

  int gui_application::execute (void)
  {
    octave_thread_manager::block_interrupt_signal ();

    set_application_id ();

    std::string show_gui_msgs =
      sys::env::getenv ("OCTAVE_SHOW_GUI_MESSAGES");

    // Installing our handler suppresses the messages.

    if (show_gui_msgs.empty ())
      {
#if defined (HAVE_QT4)
        qInstallMsgHandler (message_handler);
#else
        qInstallMessageHandler (message_handler);
#endif
      }

    // If START_GUI is false, we still set up the QApplication so that
    // we can use Qt widgets for plot windows.

    QApplication qt_app (m_argc, m_argv);
    QTranslator gui_tr, qt_tr, qsci_tr;

    // Set the codec for all strings (before wizard or any GUI object)
#if ! defined (Q_OS_WIN32)
    QTextCodec::setCodecForLocale (QTextCodec::codecForName ("UTF-8"));
#endif

#if defined (HAVE_QT4)
    QTextCodec::setCodecForCStrings (QTextCodec::codecForName ("UTF-8"));
#endif

    // set windows style for windows
#if defined (Q_OS_WIN32)
    qt_app.setStyle (QStyleFactory::create ("Windows"));
#endif

    bool start_gui = start_gui_p ();

    // Show welcome wizard if this is the first run.

    if (resource_manager::is_first_run () && start_gui)
      {
        // Before wizard.
        resource_manager::config_translators (&qt_tr, &qsci_tr, &gui_tr);

        qt_app.installTranslator (&qt_tr);
        qt_app.installTranslator (&gui_tr);
        qt_app.installTranslator (&qsci_tr);

        welcome_wizard welcomeWizard;

        if (welcomeWizard.exec () == QDialog::Rejected)
          exit (1);

        // Install settings file.
        resource_manager::reload_settings ();
      }
    else
      {
        // Get settings file.
        resource_manager::reload_settings ();

        // After settings.
        resource_manager::config_translators (&qt_tr, &qsci_tr, &gui_tr);

        qt_app.installTranslator (&qt_tr);
        qt_app.installTranslator (&gui_tr);

        if (start_gui)
          qt_app.installTranslator (&qsci_tr);
      }

    if (start_gui)
      {
        resource_manager::update_network_settings ();

        // We provide specific terminal capabilities, so ensure that
        // TERM is always set appropriately.

#if defined (OCTAVE_USE_WINDOWS_API)
        sys::env::putenv ("TERM", "cygwin");
#else
        sys::env::putenv ("TERM", "xterm");
#endif

        shortcut_manager::init_data ();
      }

    // Force left-to-right alignment (see bug #46204)
    qt_app.setLayoutDirection (Qt::LeftToRight);

    // Create and show main window.

    main_window w (0, this);

    if (start_gui)
      {
        w.read_settings ();

        w.init_terminal_size ();

        // Connect signals for changes in visibility not before w
        // is shown.

        w.connect_visibility_changed ();

        w.focus_command_window ();

        gui_running (true);
      }
    else
      qt_app.setQuitOnLastWindowClosed (false);

    return qt_app.exec ();
  }
}
