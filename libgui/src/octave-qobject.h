/*

Copyright (C) 2013-2019 John W. Eaton
Copyright (C) 2011-2019 Jacob Dawid

This file is part of Octave.

Octave is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<https://www.gnu.org/licenses/>.

*/

#if ! defined (octave_octave_qobject_h)
#define octave_octave_qobject_h 1

#include <QApplication>
#include <QList>
#include <QObject>
#include <QString>
#include <QStringList>

#include "interpreter-qobject.h"

namespace octave
{
  class main_window;
  class qt_application;

  //! This class is a simple wrapper around QApplication so that we can
  //! reimplement QApplication::notify.  The octave_qapplication object
  //! should behave identically to a QApplication object except that it
  //! overrides the notify method so we can handle forward Octave
  //! octave::execution_exception exceptions from the GUI thread to the
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

    base_qobject (qt_application& app_context);

    ~base_qobject (void);

    void config_translators (void);

    void start_main_thread (void);

    int exec (void);

    // The Octave application context.
    qt_application& app_context (void) { return m_app_context; }

    // The Qt QApplication.
    QApplication * qapplication (void) { return m_qapplication; };

    interpreter_qobject * interpreter_qobj (void)
    {
      return m_interpreter_qobj;
    }

    QThread *main_thread (void) { return m_main_thread; }

  private:

#if defined (Q_OS_MAC)
    // FIXME: Does this need to be a private member function of base_qobject?
    void disable_app_nap (void);
#endif

  public slots:

    void handle_octave_finished (int);

    virtual void confirm_shutdown_octave (void);

    void copy_image_to_clipboard (const QString& file, bool remove_file);

    void interpreter_event (const fcn_callback& fcn);

    void interpreter_event (const meth_callback& meth);

  protected:

    qt_application& m_app_context;

    // Use these to ensure that argc and argv exist for as long as the
    // QApplication object.

    int m_argc;
    char **m_argv;

    octave_qapplication *m_qapplication;

    QTranslator *m_qt_tr;
    QTranslator *m_gui_tr;
    QTranslator *m_qsci_tr;

    bool m_translators_installed;

    interpreter_qobject *m_interpreter_qobj;

    QThread *m_main_thread;
  };

  //! This object provides a command-line interface to Octave that may
  //! use Qt graphics.

  class cli_qobject : public base_qobject
  {
    Q_OBJECT

  public:

    cli_qobject (qt_application& app_context);

    ~cli_qobject (void) = default;
  };

  //! This object provides a full GUI interface to Octave that is
  //! implemented Qt.

  class gui_qobject : public base_qobject
  {
    Q_OBJECT

  public:

    gui_qobject (qt_application& app_context);

    ~gui_qobject (void);

  public slots:

    void confirm_shutdown_octave (void);

  private:

    main_window *m_main_window;
  };
}

#endif

