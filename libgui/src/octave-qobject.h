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

// Defined for purposes of sending QList<int> as part of signal.
typedef QList<int> QIntList;

// Defined for purposes of sending QList<float> as part of signal.
typedef QList<float> QFloatList;

namespace octave
{
  class interpreter_qobject;
  class main_window;
  class octave_qt_link;
  class qt_application;

  //! This class is a simple wrapper around QApplication so that we can
  //! reimplement QApplication::notify.  The octave_qapplication object
  //! should behave identically to a QApplication object except that it
  //! overrides the notify method so we can handle forward Octave
  //! octave::execution_exception exceptions from the GUI thread to the
  //! interpreter thread.

  class octave_qapplication : public QApplication
  {
  public:

    octave_qapplication (int& argc, char **argv)
      : QApplication (argc, argv)
    { }

    virtual bool notify (QObject *receiver, QEvent *e) override;

    ~octave_qapplication (void) { };
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

    QApplication *octave_qapp (void) { return m_octave_qapp; };

  public slots:

    void handle_octave_finished (int);

    virtual void confirm_shutdown_octave (void);

    void copy_image_to_clipboard (const QString& file, bool remove_file);

    void handle_create_dialog (const QString& message, const QString& title,
                               const QString& icon, const QStringList& button,
                               const QString& defbutton,
                               const QStringList& role);

    void handle_create_listview (const QStringList& list, const QString& mode,
                                 int width, int height,
                                 const QIntList& initial,
                                 const QString& name,
                                 const QStringList& prompt,
                                 const QString& ok_string,
                                 const QString& cancel_string);

    void handle_create_inputlayout (const QStringList&, const QString&,
                                    const QFloatList&, const QFloatList&,
                                    const QStringList&);

    void handle_create_filedialog (const QStringList& filters,
                                   const QString& title,
                                   const QString& filename,
                                   const QString& dirname,
                                   const QString& multimode);

  protected:

    qt_application& m_app_context;

    // Use these to ensure that argc and argv exist for as long as the
    // QApplication object.

    int m_argc;
    char **m_argv;

    QApplication *m_octave_qapp;

    QTranslator *m_qt_tr;
    QTranslator *m_gui_tr;
    QTranslator *m_qsci_tr;

    bool m_translators_installed;

    octave_qt_link *m_octave_qt_link;

    interpreter_qobject *m_interpreter_qobject;

    QThread *m_main_thread;

    void connect_uiwidget_links (void);

    void confirm_shutdown_octave_internal (bool closenow);
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

