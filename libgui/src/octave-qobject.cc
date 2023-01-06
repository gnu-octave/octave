////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2011-2023 The Octave Project Developers
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

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <utility>

#include <QApplication>
#include <QClipboard>
#include <QFile>
#include <QTextCodec>
#include <QThread>
#include <QTimer>
#include <QTranslator>

// QTerminal includes
#include "QTerminal.h"

#if defined (HAVE_QSCINTILLA)
#  include "command-widget.h"
#endif
#include "community-news.h"
#include "documentation-dock-widget.h"
#include "files-dock-widget.h"
#include "history-dock-widget.h"
#include "interpreter-qobject.h"
#include "main-window.h"
#include "octave-qobject.h"
#include "qt-application.h"
#include "qt-interpreter-events.h"
#include "release-notes.h"
#include "resource-manager.h"
#include "shortcut-manager.h"
#include "terminal-dock-widget.h"
#include "variable-editor.h"
#include "workspace-model.h"
#include "workspace-view.h"

// Bug #55940 (Disable App Nap on Mac)
#if defined (Q_OS_MAC)
#  include <objc/runtime.h>
#  include <objc/message.h>
#endif

#include "interpreter.h"
#include "oct-env.h"
#include "version.h"

#include "ovl.h"

// Bug #55940 (Disable App Nap on Mac)
#if defined (Q_OS_MAC)
static void disable_app_nap (void)
{
  Class process_info_class;
  SEL process_info_selector;
  SEL begin_activity_with_options_selector;
  id process_info;
  id reason_string;
  id osx_latencycritical_activity;

  // Option codes found at https://stackoverflow.com/questions/22784886/what-can-make-nanosleep-drift-with-exactly-10-sec-on-mac-os-x-10-9/32729281#32729281
  unsigned long long NSActivityUserInitiatedAllowingIdleSystemSleep = 0x00FFFFFFULL;
  unsigned long long NSActivityLatencyCritical = 0xFF00000000ULL;

  // Avoid errors on older versions of OS X
  process_info_class = reinterpret_cast<Class> (objc_getClass ("NSProcessInfo"));
  if (process_info_class == nil)
    return;

  process_info_selector = sel_getUid ("processInfo");
  if (class_getClassMethod (process_info_class, process_info_selector)
      == nullptr)
    return;

  begin_activity_with_options_selector = sel_getUid ("beginActivityWithOptions:reason:");
  if (class_getInstanceMethod (process_info_class,
                               begin_activity_with_options_selector)
      == nullptr)
    return;

  process_info = reinterpret_cast<id (*) (id, SEL)> (objc_msgSend)
                   (reinterpret_cast<id> (process_info_class),
                    process_info_selector);
  if (process_info == nil)
    return;

  reason_string = reinterpret_cast<id (*) (id, SEL)> (objc_msgSend)
                    (reinterpret_cast<id> (objc_getClass ("NSString")),
                     sel_getUid ("alloc"));
  reason_string = reinterpret_cast<id (*) (id, SEL, const char *)> (objc_msgSend)
                    (reason_string, sel_getUid ("initWithUTF8String:"),
                     "App Nap causes pause() malfunction");

  // Start an Activity that suppresses App Nap.  This Activity will run for
  // the entire duration of the Octave process.  This is intentional,
  // not a leak.
  osx_latencycritical_activity =
    reinterpret_cast<id (*) (id, SEL, unsigned long long, id)> (objc_msgSend)
      (process_info,
       begin_activity_with_options_selector,
       NSActivityUserInitiatedAllowingIdleSystemSleep
       | NSActivityLatencyCritical,
       reason_string);
}
#endif

OCTAVE_BEGIN_NAMESPACE(octave)

// Disable all Qt messages by default.

static void
message_handler (QtMsgType, const QMessageLogContext&, const QString&)
{ }

//! Reimplement QApplication::notify.  Octave's own exceptions are
//! caught and rethrown in the interpreter thread.

bool octave_qapplication::notify (QObject *receiver, QEvent *ev)
{
  try
    {
      return QApplication::notify (receiver, ev);
    }
  catch (execution_exception& ee)
    {
      emit interpreter_event
        ([=] (void)
        {
          // INTERPRETER THREAD
          throw ee;
        });
    }

  return false;
}

// We will create a QApplication object, even if START_GUI is false,
// so that we can use Qt widgets for plot windows when running in
// command-line mode.  Note that we are creating an
// octave_qapplication object but handling it as a QApplication object
// because the octave_qapplication should behave identically to a
// QApplication object except that it overrides the notify method so
// we can handle forward Octave interpreter exceptions from the GUI
// thread to the interpreter thread.

base_qobject::base_qobject (qt_application& app_context, bool gui_app)
  : QObject (),
    m_app_context (app_context),
    m_argc (m_app_context.sys_argc ()),
    m_argv (m_app_context.sys_argv ()),
    m_qapplication (new octave_qapplication (m_argc, m_argv)),
    m_resource_manager (),
    m_shortcut_manager (*this),
    m_qt_tr (new QTranslator ()),
    m_gui_tr (new QTranslator ()),
    m_qsci_tr (new QTranslator ()),
    m_translators_installed (false),
    m_qt_interpreter_events (new qt_interpreter_events (*this)),
    m_interpreter_qobj (new interpreter_qobject (*this)),
    m_main_thread (new QThread ()),
    m_gui_app (gui_app),
    m_interpreter_ready (false),
    m_workspace_model (new workspace_model ()),
    m_documentation_widget (),
    m_file_browser_widget (),
    m_history_widget (),
    m_workspace_widget (),
    m_editor_widget (),
    m_variable_editor_widget (),
    m_main_window (nullptr)
{
  std::string show_gui_msgs =
    sys::env::getenv ("OCTAVE_SHOW_GUI_MESSAGES");

  // Installing our handler suppresses the messages.

  if (show_gui_msgs.empty ())
    qInstallMessageHandler (message_handler);

  // Set the codec for all strings (before wizard or any GUI object)
#if ! defined (Q_OS_WIN32)
  QTextCodec::setCodecForLocale (QTextCodec::codecForName ("UTF-8"));
#endif

  // Initialize global Qt application metadata.

  QCoreApplication::setApplicationName ("GNU Octave");
  QCoreApplication::setApplicationVersion (OCTAVE_VERSION);

  // Register octave_value_list for connecting thread crossing signals.

  qRegisterMetaType<octave_value_list> ("octave_value_list");

  // Bug #55940 (Disable App Nap on Mac)
#if defined (Q_OS_MAC)
  // Mac App Nap feature causes pause() and sleep() to misbehave.
  // Disable it for the entire program run.
  disable_app_nap ();
#endif

  // Force left-to-right alignment (see bug #46204)
  m_qapplication->setLayoutDirection (Qt::LeftToRight);

  // Qt docs recommend using Qt::QueuedConnection when connecting to
  // the QCoreApplication::exit slot.
  connect (m_interpreter_qobj, &interpreter_qobject::shutdown_finished,
           m_qapplication, &octave_qapplication::exit,
           Qt::QueuedConnection);

  connect (m_interpreter_qobj, &interpreter_qobject::ready,
           this, &base_qobject::interpreter_ready);

  connect (m_main_thread, &QThread::finished,
           m_main_thread, &QThread::deleteLater);

  // Handle any interpreter_event signal from the octave_qapplication
  // object here.

  connect (m_qapplication, QOverload<const fcn_callback&>::of (&octave_qapplication::interpreter_event),
           this, QOverload<const fcn_callback&>::of (&base_qobject::interpreter_event));

  connect (m_qapplication, QOverload<const meth_callback&>::of (&octave_qapplication::interpreter_event),
           this, QOverload<const meth_callback&>::of (&base_qobject::interpreter_event));

  if (m_app_context.experimental_terminal_widget ())
    {
      connect (qt_link (), &qt_interpreter_events::start_gui_signal,
               this, &base_qobject::start_gui);

      connect (qt_link (), &qt_interpreter_events::show_terminal_window_signal,
               this, &base_qobject::show_terminal_window);
    }

  connect (qt_link (), &qt_interpreter_events::copy_image_to_clipboard_signal,
           this, &base_qobject::copy_image_to_clipboard);

  connect (qt_link (), &qt_interpreter_events::show_documentation_signal,
           this, &base_qobject::show_documentation_window);

  connect (qt_link (), &qt_interpreter_events::show_file_browser_signal,
           this, &base_qobject::show_file_browser_window);

  connect (qt_link (), &qt_interpreter_events::show_command_history_signal,
           this, &base_qobject::show_command_history_window);

  connect (qt_link (), &qt_interpreter_events::show_workspace_signal,
           this, &base_qobject::show_workspace_window);

  connect (qt_link (), &qt_interpreter_events::edit_variable_signal,
           this, &base_qobject::show_variable_editor_window);

  connect (qt_link (), &qt_interpreter_events::show_community_news_signal,
           this, &base_qobject::show_community_news);

  connect (qt_link (), &qt_interpreter_events::show_release_notes_signal,
           this, &base_qobject::show_release_notes);

  if (m_app_context.experimental_terminal_widget ())
    {
      m_qapplication->setQuitOnLastWindowClosed (false);
    }
  else
    {
      if (gui_app)
        {
          m_main_window = new main_window (*this);

          connect (m_main_window, &main_window::show_community_news_signal,
                   this, &base_qobject::show_community_news);

          connect (m_main_window, &main_window::show_release_notes_signal,
                   this, &base_qobject::show_release_notes);

          if (m_interpreter_ready)
            m_main_window->handle_octave_ready ();
          else
            connect (m_interpreter_qobj, &interpreter_qobject::ready,
                     m_main_window, &main_window::handle_octave_ready);

          connect (qt_link (), &qt_interpreter_events::focus_window_signal,
                   m_main_window, &main_window::focus_window);

          m_app_context.gui_running (true);
        }
      else
        {
          // Get settings file.
          m_resource_manager.reload_settings ();

          // After settings.
          config_translators ();
          m_resource_manager.config_icon_theme ();

          // Initilize the shortcut-manager
          m_shortcut_manager.init_data ();

          m_qapplication->setQuitOnLastWindowClosed (false);
        }
    }

  start_main_thread ();
}

base_qobject::~base_qobject (void)
{
  // Note that we don't delete m_main_thread here.  That is handled by
  // deleteLater slot that is called when the m_main_thread issues a
  // finished signal.

  // FIXME: Why are dock widget settings and/or the main window
  // configuration not saved correctly if the main window is deleted
  // after the dock widgets?

  // Calling close will cause settings to be saved.
  // If m_main_window exists, the widgets are closed by the main window

  if (! m_main_window)
    {
      if (m_terminal_widget)
        m_terminal_widget->close ();

      if (m_documentation_widget)
        m_documentation_widget->close ();

      if (m_file_browser_widget)
        m_file_browser_widget->close ();

      if (m_history_widget)
        m_history_widget->close ();

      if (m_workspace_widget)
        m_workspace_widget->close ();

      if (m_editor_widget)
        m_editor_widget->close ();

      if (m_variable_editor_widget)
        m_variable_editor_widget->close ();

      if (m_community_news)
        m_community_news->close ();
    }
  else
    {
      delete m_main_window;
    }

  delete m_terminal_widget;
  delete m_documentation_widget;
  delete m_file_browser_widget;
  delete m_history_widget;
  delete m_workspace_widget;
  delete m_editor_widget;
  delete m_variable_editor_widget;
  delete m_community_news;

  delete m_interpreter_qobj;
  delete m_qsci_tr;
  delete m_gui_tr;
  delete m_qt_tr;
  delete m_qapplication;
  delete m_workspace_model;

  string_vector::delete_c_str_vec (m_argv);
}

void base_qobject::config_translators (void)
{
  if (m_translators_installed)
    return;

  m_resource_manager.config_translators (m_qt_tr, m_qsci_tr, m_gui_tr);

  m_qapplication->installTranslator (m_qt_tr);
  m_qapplication->installTranslator (m_gui_tr);
  m_qapplication->installTranslator (m_qsci_tr);

  m_translators_installed = true;
}

void base_qobject::start_main_thread (void)
{
  // Note: if using the new experimental terminal widget, we defer
  // initializing and executing the interpreter until the main event
  // loop begins executing.

  // With the old terminal widget, we defer initializing and executing
  // the interpreter until after the main window and QApplication are
  // running to prevent race conditions.

  QTimer::singleShot (0, m_interpreter_qobj, SLOT (execute (void)));

  m_interpreter_qobj->moveToThread (m_main_thread);

  m_main_thread->start ();
}

int base_qobject::exec (void)
{
  int status = m_qapplication->exec ();

#if defined (Q_OS_MAC)
  // fprintf to stderr is needed by macOS, for poorly-understood reasons.
  fprintf (stderr, "\n");
#endif

  m_main_thread->quit ();
  m_main_thread->wait ();

  return status;
}

// Provided for convenience.  Will be removed once we eliminate the
// old terminal widget.
bool base_qobject::experimental_terminal_widget (void) const
{
  return m_app_context.experimental_terminal_widget ();
}

bool base_qobject::gui_running (void) const
{
  return m_app_context.gui_running ();
}

QPointer<terminal_dock_widget>
base_qobject::terminal_widget (main_window *mw)
{
  if (m_terminal_widget && mw)
    {
      m_terminal_widget->set_main_window (mw);
      m_terminal_widget->set_adopted (true);
    }
  else if (! m_terminal_widget)
    {
      m_terminal_widget
        = QPointer<terminal_dock_widget> (new terminal_dock_widget (mw, *this));
      if (experimental_terminal_widget ())
        {
#if defined (HAVE_QSCINTILLA)
          command_widget *cmd_widget
            = m_terminal_widget->get_command_widget ();

          connect (cmd_widget, &command_widget::interpreter_pause,
                   this, &base_qobject::interpreter_pause);

          connect (cmd_widget, &command_widget::interpreter_resume,
                   this, &base_qobject::interpreter_resume);

          connect (cmd_widget, &command_widget::interpreter_stop,
                   this, &base_qobject::interpreter_stop);

          connect (qt_link (), &qt_interpreter_events::interpreter_output_signal,
                   m_terminal_widget, &terminal_dock_widget::interpreter_output_signal);

          connect (qt_link (), &qt_interpreter_events::update_prompt_signal,
                   m_terminal_widget, &terminal_dock_widget::update_prompt_signal);

          connect (qt_link (), &qt_interpreter_events::new_command_line_signal,
                   m_terminal_widget, &terminal_dock_widget::new_command_line_signal);

          connect_interpreter_events (cmd_widget);
#endif
        }
      else
        {
          QTerminal *cmd_widget = m_terminal_widget->get_qterminal ();

          // Connect the interrupt signal (emitted by Ctrl-C)
          connect (cmd_widget, &QTerminal::interrupt_signal,
                   this, &base_qobject::interpreter_interrupt);
        }
    }

  return m_terminal_widget;
}

QPointer<documentation_dock_widget>
base_qobject::documentation_widget (main_window *mw)
{
  if (m_documentation_widget && mw)
    {
      m_documentation_widget->set_main_window (mw);
      m_documentation_widget->set_adopted (true);
    }
  else if (! m_documentation_widget)
    {
      m_documentation_widget
        = QPointer<documentation_dock_widget> (new documentation_dock_widget (mw, *this));

      connect (qt_link (),
               &qt_interpreter_events::register_documentation_signal,
               m_documentation_widget,
               &documentation_dock_widget::registerDoc);

      connect (qt_link (),
               &qt_interpreter_events::unregister_documentation_signal,
               m_documentation_widget,
               &documentation_dock_widget::unregisterDoc);
    }

  return m_documentation_widget;
}

QPointer<files_dock_widget>
base_qobject::file_browser_widget (main_window *mw)
{
  if (m_file_browser_widget)
    {
      m_file_browser_widget->set_main_window (mw);
      m_file_browser_widget->set_adopted (true);
    }
  else if (! m_file_browser_widget)
    m_file_browser_widget
      = QPointer<files_dock_widget> (new files_dock_widget (mw, *this));

  connect (qt_link (), &qt_interpreter_events::directory_changed_signal,
           m_file_browser_widget, &files_dock_widget::update_octave_directory);

  return m_file_browser_widget;
}

QPointer<history_dock_widget>
base_qobject::history_widget (main_window *mw)
{
  if (m_history_widget)
    {
      m_history_widget->set_main_window (mw);
      m_history_widget->set_adopted (true);
    }
  else if (! m_history_widget)
    {
      m_history_widget
        = QPointer<history_dock_widget> (new history_dock_widget (mw, *this));

      connect (qt_link (), &qt_interpreter_events::set_history_signal,
               m_history_widget, &history_dock_widget::set_history);

      connect (qt_link (), &qt_interpreter_events::append_history_signal,
               m_history_widget, &history_dock_widget::append_history);

      connect (qt_link (), &qt_interpreter_events::clear_history_signal,
               m_history_widget, &history_dock_widget::clear_history);

      emit interpreter_event
        ([=] (interpreter& interp) {
          // INTERPRETER THREAD

          event_manager& xevmgr = interp.get_event_manager ();

          xevmgr.set_history ();
        });
    }

  return m_history_widget;
}

QPointer<workspace_view>
base_qobject::workspace_widget (main_window *mw)
{
  if (m_workspace_widget)
    {
      m_workspace_widget->set_main_window (mw);
      m_workspace_widget->set_adopted (true);
    }
  else if (! m_workspace_widget)
    {
      m_workspace_widget
        = QPointer<workspace_view> (new workspace_view (mw, *this));

      m_workspace_widget->setModel (m_workspace_model);

      connect (m_workspace_model, &workspace_model::model_changed,
               m_workspace_widget, &workspace_view::handle_model_changed);

      connect (qt_link (), &qt_interpreter_events::set_workspace_signal,
               m_workspace_model, &workspace_model::set_workspace);

      connect (qt_link (), &qt_interpreter_events::clear_workspace_signal,
               m_workspace_model, &workspace_model::clear_workspace);

      connect (m_workspace_widget,
               &workspace_view::copy_variable_value_to_clipboard,
               [=] (const QString& var_name) {
                 emit interpreter_event
                   ([=] (interpreter& interp)
                   {
                     // INTERPRETER THREAD

                     octave_value val = interp.varval (var_name.toStdString ());

                     if (val.is_undefined ())
                       val = 0;

                     std::ostringstream buf;
                     val.print_raw (buf, true);

                     // FIXME: is the following operation thread safe or should
                     // it be done with a signal/slot connection?

                     QClipboard *clipboard = QApplication::clipboard ();
                     clipboard->setText (QString::fromStdString (buf.str ()));
                   });
               });

      connect (m_workspace_widget, &workspace_view::rename_variable_signal,
               [=] (const QString& old_name, const QString& new_name) {
                 emit interpreter_event
                   ([=] (interpreter& interp) {
                     // INTERPRETER THREAD

                     symbol_scope scope = interp.get_current_scope ();

                     if (scope)
                       {
                         scope.rename (old_name.toStdString (),
                                       new_name.toStdString ());

                         tree_evaluator& tw = interp.get_evaluator ();

                         event_manager& xevmgr = interp.get_event_manager ();

                         xevmgr.set_workspace (true, tw.get_symbol_info ());
                       }

                     // FIXME: if this action fails, do we need a way to
                     // display that info in the GUI?
                   });
               });

      connect (m_workspace_widget, &workspace_view::edit_variable_signal,
               [=] (const QString& var_name) {
                 emit interpreter_event
                   ([=] (interpreter& interp) {
                     // INTERPRETER THREAD

                     std::string name = var_name.toStdString ();
                     octave_value val = interp.varval (name);

                     event_manager& xevmgr = interp.get_event_manager ();

                     xevmgr.edit_variable (name, val);
                   });
               });

      emit interpreter_event
        ([=] (interpreter& interp) {
          // INTERPRETER THREAD

          event_manager& xevmgr = interp.get_event_manager ();

          xevmgr.set_workspace ();
        });
    }

  return m_workspace_widget;
}

QPointer<file_editor_interface>
base_qobject::editor_widget (main_window */*mw*/)
{
#if 0
  if (m_editor_widget && mw)
    {
      m_editor_widget->set_main_window (mw);
      m_editor_widget->set_adopted (true);
    }
  else if (! m_editor_widget)
    m_editor_widget = new file_editor (mw, *this);
#endif

  return m_editor_widget;
}

QPointer<variable_editor>
base_qobject::variable_editor_widget (main_window *mw)
{
  if (m_variable_editor_widget && mw)
    {
      m_variable_editor_widget->set_main_window (mw);
      m_variable_editor_widget->set_adopted (true);
    }
  else if (! m_variable_editor_widget)
    {
      m_variable_editor_widget
        = QPointer<variable_editor> (new variable_editor (mw, *this));

      connect (m_variable_editor_widget, &variable_editor::updated,
               this, &base_qobject::handle_variable_editor_update);

      connect (m_variable_editor_widget, &variable_editor::command_signal,
               this, &base_qobject::execute_command);

      connect (qt_link (),
               &qt_interpreter_events::refresh_variable_editor_signal,
               m_variable_editor_widget, &variable_editor::refresh);

      connect_interpreter_events<variable_editor> (m_variable_editor_widget);
    }

  return m_variable_editor_widget;
}

QPointer<community_news> base_qobject::community_news_widget (int serial)
{
  if (! m_community_news)
    m_community_news
      = QPointer<community_news> (new community_news (*this, serial));

  return m_community_news;
}

QPointer<release_notes> base_qobject::release_notes_widget (void)
{
  if (! m_release_notes)
    m_release_notes = QPointer<release_notes> (new release_notes (*this));

  return m_release_notes;
}

bool base_qobject::confirm_shutdown (void)
{
  // Currently, we forward to main_window::confirm_shutdown instead of
  // just displaying a dialog box here because the main_window also
  // knows about and is responsible for notifying the editor.

  return m_main_window ? m_main_window->confirm_shutdown () : true;
}

void base_qobject::start_gui (bool gui_app)
{
  if (m_app_context.experimental_terminal_widget ())
    {
      if (m_main_window)
        return;

      m_gui_app = gui_app;

      m_main_window = new main_window (*this);

      connect (qt_link (), &qt_interpreter_events::focus_window_signal,
               m_main_window, &main_window::focus_window);

      connect (qt_link (), &qt_interpreter_events::close_gui_signal,
               this, &base_qobject::close_gui);

      connect (m_main_window, &main_window::close_gui_signal,
               this, &base_qobject::close_gui);

      connect (m_main_window, &main_window::show_community_news_signal,
               this, &base_qobject::show_community_news);

      connect (m_main_window, &main_window::show_release_notes_signal,
               this, &base_qobject::show_release_notes);

      if (m_interpreter_ready)
        m_main_window->handle_octave_ready ();
      else
        connect (m_interpreter_qobj, &interpreter_qobject::ready,
                 m_main_window, &main_window::handle_octave_ready);

      if (m_gui_app)
        m_qapplication->setQuitOnLastWindowClosed (true);
      else
        {
          // FIXME: Save current values of PS1 and PS2 so they can be
          // restored when we return to the command line?
        }

      m_app_context.gui_running (true);
    }
}

void base_qobject::show_terminal_window (void)
{
  terminal_dock_widget *widget
    = (m_terminal_widget
       ? m_terminal_widget : terminal_widget ());

  if (! widget->isVisible ())
    {
      widget->show ();
      widget->raise ();
    }
}

void base_qobject::show_documentation_window (const QString& file)
{
  documentation_dock_widget *widget
    = (m_documentation_widget
       ? m_documentation_widget : documentation_widget ());

  widget->showDoc (file);

  if (! widget->isVisible ())
    {
      widget->show ();
      widget->raise ();
    }
}

void base_qobject::show_file_browser_window (void)
{
  files_dock_widget *widget
    = m_file_browser_widget ? m_file_browser_widget : file_browser_widget ();

  if (! widget->isVisible ())
    {
      widget->show ();
      widget->raise ();
    }
}

void base_qobject::show_command_history_window (void)
{
  history_dock_widget *widget
    = m_history_widget ? m_history_widget : history_widget ();

  if (! widget->isVisible ())
    {
      widget->show ();
      widget->raise ();
    }
}

void base_qobject::show_workspace_window (void)
{
  workspace_view *widget
    = m_workspace_widget ? m_workspace_widget : workspace_widget ();

  if (! widget->isVisible ())
    {
      widget->show ();
      widget->raise ();
    }
}

void base_qobject::show_variable_editor_window (const QString& name,
                                                const octave_value& value)
{
  variable_editor *widget
    = (m_variable_editor_widget
       ? m_variable_editor_widget : variable_editor_widget ());

  if (! widget->isVisible ())
    {
      widget->show ();
      widget->raise ();
    }

  // FIXME: Should this be done with a signal/slot connection?
  widget->edit_variable (name, value);
}

void base_qobject::handle_variable_editor_update (void)
{
  // Called when the variable editor emits the updated signal.  The size
  // of a variable may have changed, so we refresh the workspace in the
  // interpreter.  That will eventually cause the workspace view in the
  // GUI to be updated.

  interpreter_event
    ([] (interpreter& interp)
    {
      // INTERPRETER THREAD

      tree_evaluator& tw = interp.get_evaluator ();

      event_manager& xevmgr = interp.get_event_manager ();

      xevmgr.set_workspace (true, tw.get_symbol_info (), false);
    });
}

void base_qobject::show_community_news (int serial)
{
  // Ensure widget exists.
  community_news_widget (serial);

  m_community_news->display ();
}

void base_qobject::show_release_notes (void)
{
  // Ensure widget exists.
  release_notes_widget ();

  m_release_notes->display ();
}

void base_qobject::execute_command (const QString& command)
{
  emit interpreter_event
    ([=] (interpreter& interp)
    {
      // INTERPRETER THREAD

      // FIXME: Do we need to do anything special about errors here?
      // Currently the eval function will just call error() in the
      // interpreter event loop and throw an execution error.  It will
      // be caught, so shouldn't crash the interpreter, but the
      // message may not go anywhere useful depending on how the GUI
      // is being used or if Octave running server mode.

      interp.eval (command.toStdString (), 0);
    });
}

void base_qobject::close_gui (void)
{
  if (m_app_context.experimental_terminal_widget ())
    {
      if (! m_main_window)
        return;

      // FIXME: Restore previous values of PS1 and PS2 if we are
      // returning to the command line?

      interpreter_event
        ([=] (interpreter& interp)
        {
          // INTERPRETER THREAD

          application *app = interp.get_app_context ();

          cmdline_options opts = app->options ();

          if (opts.gui ())
            interp.quit (0, false, false);
        });

      m_app_context.gui_running (false);

      if (m_main_window)
        {
          m_main_window->deleteLater ();

          m_main_window = nullptr;
        }
    }
}

void base_qobject::interpreter_ready (void)
{
  m_interpreter_ready = true;
}

void base_qobject::interpreter_event (const fcn_callback& fcn)
{
  // The following is a direct function call across threads.  It works
  // because it is accessing a thread-safe queue of events that
  // are later executed by the Octave interpreter in the other thread.

  // See also the comments in interpreter-qobject.h about
  // interpreter_qobject slots.

  m_interpreter_qobj->interpreter_event (fcn);
}

void base_qobject::interpreter_event (const meth_callback& meth)
{
  // The following is a direct function call across threads.  It works
  // because it is accessing a thread-safe queue of events that
  // are later executed by the Octave interpreter in the other thread.

  // See also the comments in interpreter-qobject.h about
  // interpreter_qobject slots.

  m_interpreter_qobj->interpreter_event (meth);
}

void base_qobject::interpreter_interrupt (void)
{
  m_interpreter_qobj->interrupt ();
}

// FIXME: Should we try to make the pause, stop, and resume actions
// work for both the old and new terminal widget?

void base_qobject::interpreter_pause (void)
{
  if (m_app_context.experimental_terminal_widget ())
    m_interpreter_qobj->pause ();
}

void base_qobject::interpreter_stop (void)
{
  if (m_app_context.experimental_terminal_widget ())
    m_interpreter_qobj->stop ();
}

void base_qobject::interpreter_resume (void)
{
  if (m_app_context.experimental_terminal_widget ())
    m_interpreter_qobj->resume ();
}

void base_qobject::copy_image_to_clipboard (const QString& file,
                                            bool remove_file)
{
  QClipboard *clipboard = QApplication::clipboard ();

  QImage img (file);

  if (img.isNull ())
    {
      // Report error?
      return;
    }

  clipboard->setImage (img);

  if (remove_file)
    QFile::remove (file);
}

OCTAVE_END_NAMESPACE(octave)
