////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2011-2021 The Octave Project Developers
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

#if ! defined (octave_octave_qobject_h)
#define octave_octave_qobject_h 1

#include <memory>

#include <QApplication>
#include <QList>
#include <QObject>
#include <QString>
#include <QStringList>

#include "interpreter-qobject.h"
#include "resource-manager.h"
#include "shortcut-manager.h"
#include "workspace-model.h"

namespace octave
{
  class main_window;
  class qt_application;
  class qt_interpreter_events;

  //! This class is a simple wrapper around QApplication so that we can
  //! reimplement QApplication::notify.  The octave_qapplication object
  //! should behave identically to a QApplication object except that it
  //! overrides the notify method so we can handle forward Octave
  //! execution_exception exceptions from the GUI thread to the
  //! interpreter thread.

  class octave_qapplication : public QApplication
  {
    Q_OBJECT

  public:

    octave_qapplication (int& argc, char **argv)
      : QApplication (argc, argv)
    { }

    virtual bool notify (QObject *receiver, QEvent *e) override;

    ~octave_qapplication (void) { };

  signals:

    void interpreter_event (const fcn_callback& fcn);
    void interpreter_event (const meth_callback& meth);
  };

  //! Base class for Octave interfaces that use Qt.  There are two
  //! classes derived from this one.  One provides a command-line
  //! interface that may use Qt graphics and another provides the
  //! full GUI experience.

  class base_qobject : public QObject
  {
    Q_OBJECT

  public:

    // Note: the GUI_APP argument is not needed with the new
    // experimental terminal widget.
    base_qobject (qt_application& app_context, bool gui_app = false);

    ~base_qobject (void);

    void config_translators (void);

    void start_main_thread (void);

    int exec (void);

    // The Octave application context.
    qt_application& app_context (void) { return m_app_context; }

    // The Qt QApplication.
    QApplication * qapplication (void) { return m_qapplication; };

    // Provided for convenience.  Will be removed once we eliminate the
    // old terminal widget.
    bool experimental_terminal_widget (void) const;

    // Provided for convenience.
    bool gui_running (void) const;

    resource_manager& get_resource_manager (void)
    {
      return m_resource_manager;
    }

    shortcut_manager& get_shortcut_manager (void)
    {
      return m_shortcut_manager;
    }

    workspace_model * get_workspace_model (void)
    {
      return m_workspace_model;
    }

    std::shared_ptr<qt_interpreter_events> get_qt_interpreter_events (void)
    {
      return m_qt_interpreter_events;
    }

    qt_interpreter_events * qt_link (void)
    {
      return m_qt_interpreter_events.get ();
    }

    interpreter_qobject * interpreter_qobj (void)
    {
      return m_interpreter_qobj;
    }

    QThread *main_thread (void) { return m_main_thread; }

    // Declared virtual so that a derived class may redefine this
    // method.

    virtual bool confirm_shutdown (void);

  signals:

    void request_interpreter_shutdown (int);

  public slots:

    // Note: START_GUI and CLOSE_GUI don't currently perform any work
    // with the old terminal widget and
    // HANDLE_INTERPRETER_EXECUTION_FINISHED doesn't perform any action
    // with the new experimental terminal widget.
    void start_gui (bool gui_app);
    void close_gui (void);
    void handle_interpreter_execution_finished (int);

    void interpreter_ready (void);

    void handle_interpreter_shutdown_finished (int);

    void interpreter_event (const fcn_callback& fcn);

    void interpreter_event (const meth_callback& meth);

    void interpreter_interrupt (void);

    // Note: these currently only work with the new experimental
    // terminal widget.
    void interpreter_pause (void);
    void interpreter_stop (void);
    void interpreter_resume (void);

    void copy_image_to_clipboard (const QString& file, bool remove_file);

  protected:

    qt_application& m_app_context;

    // Use these to ensure that argc and argv exist for as long as the
    // QApplication object.

    int m_argc;
    char **m_argv;

    octave_qapplication *m_qapplication;

    resource_manager m_resource_manager;

    shortcut_manager m_shortcut_manager;

    workspace_model *m_workspace_model;

    QTranslator *m_qt_tr;
    QTranslator *m_gui_tr;
    QTranslator *m_qsci_tr;

    bool m_translators_installed;

    std::shared_ptr<qt_interpreter_events> m_qt_interpreter_events;

    interpreter_qobject *m_interpreter_qobj;

    QThread *m_main_thread;

    bool m_gui_app;

    bool m_interpreter_ready;

    main_window *m_main_window;
  };
}

#endif
