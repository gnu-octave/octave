/*

Copyright (C) 2013-2018 John W. Eaton
Copyright (C) 2011-2018 Jacob Dawid

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

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <QKeySequence>
#include <QApplication>
#include <QInputDialog>
#include <QLabel>
#include <QMenuBar>
#include <QMenu>
#include <QAction>
#include <QSettings>
#include <QStyle>
#include <QToolBar>
#include <QDesktopServices>
#include <QDesktopWidget>
#include <QFileDialog>
#include <QMessageBox>
#include <QIcon>
#include <QTextBrowser>
#include <QTextStream>
#include <QThread>
#include <QDateTime>
#include <QDebug>
#include <QTimer>

#include <utility>

#if defined (HAVE_QSCINTILLA)
#  include "file-editor.h"
#endif
#include "main-window.h"
#include "settings-dialog.h"
#include "shortcut-manager.h"

#include "Array.h"
#include "cmd-edit.h"
#include "url-transfer.h"

#include "builtin-defun-decls.h"
#include "defaults.h"
#if defined (HAVE_QT_GRAPHICS)
#  include "__init_qt__.h"
#endif
#include "interpreter-private.h"
#include "interpreter.h"
#include "oct-map.h"
#include "octave.h"
#include "parse.h"
#include "symscope.h"
#include "utils.h"
#include "version.h"

static octave::file_editor_interface *
create_default_editor (QWidget *p)
{
#if defined (HAVE_QSCINTILLA)
  return new octave::file_editor (p);
#else
  octave_unused_parameter (p);

  return 0;
#endif
}

namespace octave
{
  octave_interpreter::octave_interpreter (gui_application *app_context)
    : QObject (), m_app_context (app_context)
  { }

  void octave_interpreter::execute (void)
  {
    // The application context owns the interpreter.

    interpreter& interp = m_app_context->create_interpreter ();

    int exit_status = 0;

    try
      {
        // Final initialization.

        interp.initialize ();

        if (m_app_context->start_gui_p ())
          {
            input_system& input_sys = interp.get_input_system ();

            input_sys.PS1 (">> ");
            input_sys.PS2 ("");

            tree_evaluator& tw = interp.get_evaluator ();

            tw.PS4 ("");
          }

        if (interp.initialized ())
          {
            // The interpreter should be completely ready at this point so let
            // the GUI know.

            emit octave_ready_signal ();

            // Start executing commands in the command window.

#if defined (HAVE_QT_GRAPHICS)
            // The qt graphics toolkit must be initialized before startup
            // files are executed.

            symbol_table& symtab = interp.get_symbol_table ();

            install___init_qt___functions (symtab);

            Fregister_graphics_toolkit (interp, ovl ("qt"));
#endif

            exit_status = interp.execute ();
          }
      }
    catch (const exit_exception& ex)
      {
        exit_status = ex.exit_status ();
      }

    // Whether or not initialization succeeds we need to clean up the
    // interpreter once we are done with it.

    m_app_context->delete_interpreter ();

    emit octave_finished_signal (exit_status);
  }

  main_window::main_window (QWidget *p, gui_application *app_context)
    : QMainWindow (p), m_app_context (app_context),
      m_interpreter (new octave_interpreter (app_context)),
      m_main_thread (new QThread ()), m_workspace_model (nullptr),
      m_status_bar (nullptr), m_command_window (nullptr),
      m_history_window (nullptr), m_file_browser_window (nullptr),
      m_doc_browser_window (nullptr), m_editor_window (nullptr),
      m_workspace_window (nullptr), m_variable_editor_window (nullptr),
      m_settings_dlg (nullptr), m_find_files_dlg (nullptr),
      m_release_notes_window (nullptr), m_community_news_window (nullptr),
      m_octave_qt_link (nullptr), m_clipboard (QApplication::clipboard ()),
      m_prevent_readline_conflicts (true), m_suppress_dbg_location (true),
      m_start_gui (app_context && app_context->start_gui_p ()),
      m_file_encoding (QString ())
  {
    if (m_start_gui)
      {
        m_workspace_model = new workspace_model ();
        m_status_bar = new QStatusBar ();
        m_command_window = new terminal_dock_widget (this);
        m_history_window = new history_dock_widget (this);
        m_file_browser_window = new files_dock_widget (this);
        m_doc_browser_window = new documentation_dock_widget (this);
        m_editor_window = create_default_editor (this);
        m_variable_editor_window = new variable_editor (this);
        m_workspace_window = new workspace_view (this);
      }

    // Initialize global Qt application metadata
    QCoreApplication::setApplicationName ("GNU Octave");
    QCoreApplication::setApplicationVersion (OCTAVE_VERSION);
#if defined (HAVE_QGUIAPPLICATION_SETDESKTOPFILENAME)
    if (m_start_gui)
      QGuiApplication::setDesktopFileName ("org.octave.Octave.desktop");
#endif

    m_external_editor = new external_editor_interface (this);
    m_active_editor = m_editor_window;  // for connecting signals
    if (! m_editor_window)
      m_active_editor = m_external_editor;

    QSettings *settings = resource_manager::get_settings ();

    bool connect_to_web = true;
    QDateTime last_checked;
    int serial = 0;
    m_active_dock = nullptr;

    if (settings)
      {
        connect_to_web
          = settings->value ("news/allow_web_connection", false).toBool ();

        last_checked
          = settings->value ("news/last_time_checked", QDateTime ()).toDateTime ();

        serial = settings->value ("news/last_news_item", 0).toInt ();
      }

    QDateTime current = QDateTime::currentDateTime ();
    QDateTime one_day_ago = current.addDays (-1);

    if (m_start_gui && connect_to_web
        && (! last_checked.isValid () || one_day_ago > last_checked))
      load_and_display_community_news (serial);

    // We have to set up all our windows, before we finally launch octave.
    construct ();

    connect (m_interpreter, SIGNAL (octave_ready_signal (void)),
             this, SLOT (handle_octave_ready (void)));

    connect (m_interpreter, SIGNAL (octave_finished_signal (int)),
             this, SLOT (handle_octave_finished (int)));

    connect (m_interpreter, SIGNAL (octave_finished_signal (int)),
             m_main_thread, SLOT (quit (void)));

    connect (m_main_thread, SIGNAL (finished (void)),
             m_main_thread, SLOT (deleteLater (void)));

    m_interpreter->moveToThread (m_main_thread);

    m_main_thread->start ();
  }

  main_window::~main_window (void)
  {
    // Note that we don't delete m_main_thread here.  That is handled by
    // deleteLater slot that is called when the m_main_thread issues a
    // finished signal.

    // Destroy the terminal first so that STDERR stream is redirected back
    // to its original pipe to capture error messages at exit.

    delete m_editor_window;     // first one for dialogs of modified editor-tabs
    delete m_external_editor;
    delete m_command_window;
    delete m_workspace_window;
    delete m_doc_browser_window;
    delete m_file_browser_window;
    delete m_history_window;
    delete m_status_bar;
    delete m_workspace_model;
    delete m_variable_editor_window;
    delete m_interpreter;

    if (m_find_files_dlg)
      {
        delete m_find_files_dlg;
        m_find_files_dlg = nullptr;
      }
    if (m_release_notes_window)
      {
        delete m_release_notes_window;
        m_release_notes_window = nullptr;
      }
    if (m_settings_dlg)
      {
        delete m_settings_dlg;
        m_settings_dlg = nullptr;
      }
    if (m_community_news_window)
      {
        delete m_community_news_window;
        m_community_news_window = nullptr;
      }
  }

  bool main_window::command_window_has_focus (void) const
  {
    return m_command_window->has_focus ();
  }

  void main_window::focus_command_window (void)
  {
    m_command_window->focus ();
  }

  // catch focus changes and determine the active dock widget
  void main_window::focus_changed (QWidget *, QWidget *new_widget)
  {
    // If there is no new widget (e.g., when pressing <alt> and the global
    // menu gets active, we can return immediately
    if (! new_widget)
      return;

    octave_dock_widget *dock = nullptr;
    QWidget *w_new = new_widget;  // get a copy of new focus widget
    QWidget *start = w_new;       // Save it as start of our search
    int count = 0;                // fallback to prevent endless loop

    QList<octave_dock_widget *> w_list = dock_widget_list ();

    while (w_new && w_new != m_main_tool_bar && count < 100)
      {
        // Go through all dock widgets and check whether the current widget
        // widget with focus is a child of one of it
        foreach (octave_dock_widget *w, w_list)
          {
            if (w->isAncestorOf (w_new))
              dock = w;
          }

        if (dock)
          break;

        // If not yet found (in case w_new is not a childs of its dock widget),
        // test next widget in the focus chain
        w_new = qobject_cast<QWidget *> (w_new->previousInFocusChain ());

        // Measures preventing an endless loop
        if (w_new == start)
          break;  // We have arrived where we began ==> exit loop
        count++;  // Limited number of trials
      }

    // editor needs extra handling
    octave_dock_widget *edit_dock_widget
      = static_cast<octave_dock_widget *> (m_editor_window);
    // if new dock has focus, emit signal and store active focus
    // except editor changes to a dialog (dock=0)
    if ((dock || m_active_dock != edit_dock_widget) && (dock != m_active_dock))
      {
        // signal to all dock widgets for updating the style
        emit active_dock_changed (m_active_dock, dock);

        QList<QDockWidget *> tabbed = tabifiedDockWidgets (dock);
        if (tabbed.contains (m_active_dock))
          dock->set_predecessor_widget (m_active_dock);

        if (edit_dock_widget == dock)
          emit editor_focus_changed (true);
        else if (edit_dock_widget == m_active_dock)
          emit editor_focus_changed (false);

        m_active_dock = dock;
      }
  }

  void main_window::request_reload_settings (void)
  {
    QSettings *settings = resource_manager::get_settings ();

    if (settings)
      emit settings_changed (settings);
  }

  void main_window::report_status_message (const QString& statusMessage)
  {
    m_status_bar->showMessage (statusMessage, 1000);
  }

  void main_window::handle_save_workspace_request (void)
  {
    QString file
      = QFileDialog::getSaveFileName (this, tr ("Save Workspace As"), ".",
                                      nullptr, nullptr,
                                      QFileDialog::DontUseNativeDialog);

    if (! file.isEmpty ())
      octave_link::post_event (this, &main_window::save_workspace_callback,
                               file.toStdString ());
  }

  void main_window::handle_load_workspace_request (const QString& file_arg)
  {
    QString file = file_arg;

    if (file.isEmpty ())
      file = QFileDialog::getOpenFileName (this, tr ("Load Workspace"), ".",
                                           nullptr, nullptr,
                                           QFileDialog::DontUseNativeDialog);

    if (! file.isEmpty ())
      octave_link::post_event (this, &main_window::load_workspace_callback,
                               file.toStdString ());
  }

  void main_window::handle_open_any_request (const QString& file_arg)
  {
    if (! file_arg.isEmpty ())
      octave_link::post_event (this, &main_window::open_any_callback,
                               file_arg.toStdString ());
  }

  void main_window::handle_clear_workspace_request (void)
  {
    octave_link::post_event (this, &main_window::clear_workspace_callback);
  }

  void main_window::handle_clear_command_window_request (void)
  {
    octave_link::post_event (this, &main_window::clear_command_window_callback);
  }

  void main_window::handle_clear_history_request (void)
  {
    octave_link::post_event (this, &main_window::clear_history_callback);
  }

  void main_window::handle_undo_request (void)
  {
    if (command_window_has_focus ())
      octave_link::post_event (this, &main_window::command_window_undo_callback);
    else
      emit undo_signal ();
  }

  void main_window::handle_rename_variable_request (const QString& old_name,
                                                    const QString& new_name)

  {
    name_pair names (old_name.toStdString (), new_name.toStdString ());

    octave_link::post_event (this, &main_window::rename_variable_callback,
                             names);
  }

  void main_window::new_file (const QString& commands)
  {
    emit new_file_signal (commands);
  }

  void main_window::open_file (const QString& file_name, int line)
  {
    if (line < 0)
      emit open_file_signal (file_name);
    else
      emit open_file_signal (file_name, QString (), line);
  }

  void main_window::edit_mfile (const QString& name, int line)
  {
    handle_edit_mfile_request (name, QString (), QString (), line);
  }

  void main_window::open_online_documentation_page (void)
  {
    QDesktopServices::openUrl (
                               QUrl ("https://octave.org/doc/interpreter/index.html"));
  }

  void main_window::display_release_notes (void)
  {
    if (! m_release_notes_window)
      {
        std::string news_file = config::oct_etc_dir () + "/NEWS";

        QString news;

        QFile *file = new QFile (QString::fromStdString (news_file));
        if (file->open (QFile::ReadOnly))
          {
            QTextStream *stream = new QTextStream (file);
            news = stream->readAll ();
            if (! news.isEmpty ())
              {
                // Convert '<', '>' which would be interpreted as HTML
                news.replace ("<", "&lt;");
                news.replace (">", "&gt;");
                // Add HTML tags for pre-formatted text
                news.prepend ("<pre>");
                news.append ("</pre>");
              }
            else
              news = (tr ("The release notes file '%1' is empty.")
                      . arg (QString::fromStdString (news_file)));
          }
        else
          news = (tr ("The release notes file '%1' cannot be read.")
                  . arg (QString::fromStdString (news_file)));

        m_release_notes_window = new QWidget;

        QTextBrowser *browser = new QTextBrowser (m_release_notes_window);
        browser->setText (news);

        QVBoxLayout *vlayout = new QVBoxLayout;
        vlayout->addWidget (browser);

        m_release_notes_window->setLayout (vlayout);
        m_release_notes_window->setWindowTitle (tr ("Octave Release Notes"));

        browser->document ()->adjustSize ();

        // center the window on the screen where octave is running
        QDesktopWidget *m_desktop = QApplication::desktop ();
        int screen = m_desktop->screenNumber (this);  // screen of the main window
        QRect screen_geo = m_desktop->availableGeometry (screen);
        int win_x = screen_geo.width ();        // width of the screen
        int win_y = screen_geo.height ();       // height of the screen
        int reln_x = std::min (720, win_x-80);  // desired width of release notes
        int reln_y = std::min (740, win_y-80);  // desired height of release notes
        m_release_notes_window->resize (reln_x, reln_y);  // set size
        m_release_notes_window->move (20, 0);     // move to the top left corner
      }

    if (! m_release_notes_window->isVisible ())
      m_release_notes_window->show ();
    else if (m_release_notes_window->isMinimized ())
      m_release_notes_window->showNormal ();

    m_release_notes_window->setWindowIcon (QIcon (m_release_notes_icon));

    m_release_notes_window->raise ();
    m_release_notes_window->activateWindow ();
  }

  void main_window::load_and_display_community_news (int serial)
  {
    QSettings *settings = resource_manager::get_settings ();

    bool connect_to_web
      = (settings
         ? settings->value ("news/allow_web_connection", false).toBool ()
         : true);

    QString base_url = "https://octave.org";
    QString page = "community-news.html";

    QThread *worker_thread = new QThread;

    news_reader *reader = new news_reader (base_url, page, serial,
                                           connect_to_web);

    reader->moveToThread (worker_thread);

    connect (reader, SIGNAL (display_news_signal (const QString&)),
             this, SLOT (display_community_news (const QString&)));

    connect (worker_thread, SIGNAL (started (void)),
             reader, SLOT (process (void)));

    connect (reader, SIGNAL (finished (void)), worker_thread, SLOT (quit (void)));

    connect (reader, SIGNAL (finished (void)), reader, SLOT (deleteLater (void)));

    connect (worker_thread, SIGNAL (finished (void)),
             worker_thread, SLOT (deleteLater (void)));

    worker_thread->start ();
  }

  void main_window::display_community_news (const QString& news)
  {
    if (! m_community_news_window)
      {
        m_community_news_window = new QWidget;

        QTextBrowser *browser = new QTextBrowser (m_community_news_window);

        browser->setHtml (news);
        browser->setObjectName ("OctaveNews");
        browser->setOpenExternalLinks (true);

        QVBoxLayout *vlayout = new QVBoxLayout;

        vlayout->addWidget (browser);

        m_community_news_window->setLayout (vlayout);
        m_community_news_window->setWindowTitle (tr ("Octave Community News"));

        // center the window on the screen where octave is running
        QDesktopWidget *m_desktop = QApplication::desktop ();
        int screen = m_desktop->screenNumber (this);  // screen of the main window
        QRect screen_geo = m_desktop->availableGeometry (screen);
        int win_x = screen_geo.width ();        // width of the screen
        int win_y = screen_geo.height ();       // height of the screen
        int news_x = std::min (640, win_x-80);  // desired width of news window
        int news_y = std::min (480, win_y-80);  // desired height of news window
        m_community_news_window->resize (news_x, news_y);  // set size and center
        m_community_news_window->move ((win_x - m_community_news_window->width ())/2,
                                       (win_y - m_community_news_window->height ())/2);
      }

    if (! m_community_news_window->isVisible ())
      m_community_news_window->show ();
    else if (m_community_news_window->isMinimized ())
      m_community_news_window->showNormal ();

    // same icon as release notes
    m_community_news_window->setWindowIcon (QIcon (m_release_notes_icon));

    m_community_news_window->raise ();
    m_community_news_window->activateWindow ();
  }

  void main_window::open_bug_tracker_page (void)
  {
    QDesktopServices::openUrl (QUrl ("https://octave.org/bugs.html"));
  }

  void main_window::open_octave_packages_page (void)
  {
    QDesktopServices::openUrl (QUrl ("https://octave.org/packages.html"));
  }

  void main_window::open_contribute_page (void)
  {
    QDesktopServices::openUrl (QUrl ("https://octave.org/contribute.html"));
  }

  void main_window::open_donate_page (void)
  {
    QDesktopServices::openUrl (QUrl ("https://octave.org/donate.html"));
  }

  void main_window::process_settings_dialog_request (const QString& desired_tab)
  {
    if (m_settings_dlg)  // m_settings_dlg is a guarded pointer!
      {
        // here the dialog is still open and called once again
        if (! desired_tab.isEmpty ())
          m_settings_dlg->show_tab (desired_tab);
        return;
      }

    m_settings_dlg = new settings_dialog (this, desired_tab);

    connect (m_settings_dlg, SIGNAL (apply_new_settings (void)),
             this, SLOT (request_reload_settings (void)));

    m_settings_dlg->setModal (false);
    m_settings_dlg->setAttribute (Qt::WA_DeleteOnClose);
    m_settings_dlg->show ();
  }

  void main_window::copy_image_to_clipboard (const QString& file,
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

  void main_window::show_about_octave (void)
  {
    std::string message
      = octave_name_version_copyright_copying_warranty_and_bugs (true);

    QMessageBox::about (this, tr ("About Octave"),
                        QString::fromStdString (message));
  }

  void main_window::notice_settings (const QSettings *settings)
  {
    // QSettings pointer is checked before emitting.

    // the widget's icons (when floating)
    QString icon_set
      = settings->value ("DockWidgets/widget_icon_set", "NONE").toString ();

    static struct
    {
      QString name;
      QString path;
    }

    widget_icon_data[] =
    {
      // array of possible icon sets (name, path (complete for NONE))
      // the first entry here is the default!
      {"NONE",    ":/actions/icons/logo.png"},
      {"GRAPHIC", ":/actions/icons/graphic_logo_"},
      {"LETTER",  ":/actions/icons/letter_logo_"},
      {"", ""} // end marker has empty name
    };

    int count = 0;
    int icon_set_found = 0; // default

    while (! widget_icon_data[count].name.isEmpty ())
      {
        // while not end of data
        if (widget_icon_data[count].name == icon_set)
          {
            // data of desired icon set found
            icon_set_found = count;
            break;
          }
        count++;
      }

    QString icon;
    foreach (octave_dock_widget *widget, dock_widget_list ())
      {
        QString name = widget->objectName ();
        if (! name.isEmpty ())
          {
            // if children has a name
            icon = widget_icon_data[icon_set_found].path; // prefix | octave-logo
            if (widget_icon_data[icon_set_found].name != "NONE")
              icon += name + ".png"; // add widget name and ext.
            widget->setWindowIcon (QIcon (icon));
          }
      }
    if (widget_icon_data[icon_set_found].name != "NONE")
      m_release_notes_icon = widget_icon_data[icon_set_found].path
                             + "ReleaseWidget.png";
    else
      m_release_notes_icon = ":/actions/icons/logo.png";

    int icon_size_settings = settings->value ("toolbar_icon_size",0).toInt ();
    QStyle *st = style ();
    int icon_size = st->pixelMetric (QStyle::PM_ToolBarIconSize);

    if (icon_size_settings == 1)
      icon_size = st->pixelMetric (QStyle::PM_LargeIconSize);
    else if (icon_size_settings == -1)
      icon_size = st->pixelMetric (QStyle::PM_SmallIconSize);

    m_main_tool_bar->setIconSize (QSize (icon_size,icon_size));

    if (settings->value ("show_status_bar",true).toBool ())
      m_status_bar->show ();
    else
      m_status_bar->hide ();

    m_prevent_readline_conflicts
      = settings->value ("shortcuts/prevent_readline_conflicts", true).toBool ();

    m_suppress_dbg_location
      = ! settings->value ("terminal/print_debug_location", false).toBool ();

    resource_manager::update_network_settings ();

    emit active_dock_changed (nullptr, m_active_dock); // update dock widget styles

    configure_shortcuts ();
    set_global_shortcuts (m_active_dock == m_command_window);
    disable_menu_shortcuts (m_active_dock == m_editor_window);


    // Set cursor blinking depending on the settings
    // Cursor blinking: consider old terminal related setting if not yet set
    // TODO: This pref. can be deprecated / removed if Qt adds support for
    //       getting the cursor blink preferences from all OS environments
    bool cursor_blinking;

    if (settings->contains ("cursor_blinking"))
      cursor_blinking = settings->value ("cursor_blinking",true).toBool ();
    else
      cursor_blinking = settings->value ("terminal/cursorBlinking",true).toBool ();

    if (cursor_blinking)
      QApplication::setCursorFlashTime (1000);  // 1000 ms flash time
    else
      QApplication::setCursorFlashTime (0);  // no flashing

  }

  void main_window::confirm_shutdown_octave (void)
  {
    bool closenow = true;

    if (m_start_gui)
      {
        QSettings *settings = resource_manager::get_settings ();

        if (settings->value ("prompt_to_exit", false).toBool ())
          {
            int ans = QMessageBox::question (this, tr ("Octave"),
                                             tr ("Are you sure you want to exit Octave?"),
                                             (QMessageBox::Ok
                                              | QMessageBox::Cancel),
                                             QMessageBox::Ok);

            if (ans != QMessageBox::Ok)
              closenow = false;
          }

#if defined (HAVE_QSCINTILLA)
        if (closenow)
          closenow = m_editor_window->check_closing ();
#endif
      }

    // Wait for link thread to go to sleep state.
    m_octave_qt_link->lock ();

    m_octave_qt_link->shutdown_confirmation (closenow);

    m_octave_qt_link->unlock ();

    // Awake the worker thread so that it continues shutting down (or not).
    m_octave_qt_link->wake_all ();
  }

  void main_window::prepare_to_exit (void)
  {
    // Find files dialog is constructed dynamically, not at time of main_window
    // construction.  Connecting it to qApp aboutToQuit signal would have
    // caused it to run after QSettings deleted.
    if (m_find_files_dlg)
      m_find_files_dlg->save_settings ();

    write_settings ();
  }

  void main_window::reset_windows (void)
  {
    QSettings *settings = resource_manager::get_default_settings ();

    set_window_layout (settings);
    showNormal ();  // make sure main window is not minimized
    focus_command_window ();
  }

  void main_window::change_directory (const QString& dir)
  {
    // Remove existing entry, if any, then add new directory at top and
    // mark it as the current directory.  Finally, update the file list
    // widget.

    int index = m_current_directory_combo_box->findText (dir);

    if (index >= 0)
      m_current_directory_combo_box->removeItem (index);

    m_current_directory_combo_box->insertItem (0, dir);
    m_current_directory_combo_box->setCurrentIndex (0);
  }

  void main_window::browse_for_directory (void)
  {
    QString dir
      = QFileDialog::getExistingDirectory (this, tr ("Browse directories"), nullptr,
                                           QFileDialog::ShowDirsOnly |
                                           QFileDialog::DontUseNativeDialog);

    set_current_working_directory (dir);

    // FIXME: on Windows systems, the command window freezes after the
    // previous actions.  Forcing the focus appears to unstick it.

    focus_command_window ();
  }

  void main_window::set_current_working_directory (const QString& dir)
  {
    // Change to dir if it is an existing directory.

    QString xdir = (dir.isEmpty () ? "." : dir);

    QFileInfo fileInfo (xdir);

    if (fileInfo.exists () && fileInfo.isDir ())
      octave_link::post_event (this, &main_window::change_directory_callback,
                               xdir.toStdString ());
  }

  void main_window::change_directory_up (void)
  {
    set_current_working_directory ("..");
  }

  // Slot that is called if return is pressed in the line edit of the
  // combobox to change to a new directory or a directory that is already
  // in the drop down list.

  void main_window::accept_directory_line_edit (void)
  {
    // Get new directory name, and change to it if it is new.  Otherwise,
    // the combo box will triggers the "activated" signal to change to the
    // directory.

    QString dir = m_current_directory_combo_box->currentText ();

    int index = m_current_directory_combo_box->findText (dir);

    if (index < 0)
      set_current_working_directory (dir);
  }

  void main_window::execute_command_in_terminal (const QString& command)
  {
    octave_cmd_exec *cmd = new octave_cmd_exec (command);

    m_cmd_queue.add_cmd (cmd);

    if (focus_console_after_command ())
      focus_command_window ();
  }

  void main_window::run_file_in_terminal (const QFileInfo& info)
  {
    octave_cmd_eval *cmd = new octave_cmd_eval (info);

    m_cmd_queue.add_cmd (cmd);

    if (focus_console_after_command ())
      focus_command_window ();
  }

  void main_window::handle_new_figure_request (void)
  {
    octave_link::post_event (this, &main_window::new_figure_callback);
  }

  void main_window::handle_enter_debugger (void)
  {
    setWindowTitle ("Octave (Debugging)");

    m_debug_continue->setEnabled (true);
    m_debug_step_into->setEnabled (true);
    m_debug_step_over->setEnabled (true);
    m_debug_step_out->setEnabled (true);
    m_debug_quit->setEnabled (true);

#if defined (HAVE_QSCINTILLA)
    m_editor_window->handle_enter_debug_mode ();
#endif
  }

  void main_window::handle_exit_debugger (void)
  {
    setWindowTitle ("Octave");

    m_debug_continue->setEnabled (false);
    m_debug_step_into->setEnabled (false);
    m_debug_step_over->setEnabled (false);
    m_debug_step_out->setEnabled (false);
    m_debug_quit->setEnabled (false);

#if defined (HAVE_QSCINTILLA)
    m_editor_window->handle_exit_debug_mode ();
#endif
  }

  void main_window::debug_continue (void)
  {
    octave_cmd_debug *cmd
      = new octave_cmd_debug ("cont", m_suppress_dbg_location);
    m_cmd_queue.add_cmd (cmd);
  }

  void main_window::debug_step_into (void)
  {
    octave_cmd_debug *cmd = new octave_cmd_debug ("in", m_suppress_dbg_location);
    m_cmd_queue.add_cmd (cmd);
  }

  void main_window::debug_step_over (void)
  {
    octave_cmd_debug *cmd
      = new octave_cmd_debug ("step", m_suppress_dbg_location);
    m_cmd_queue.add_cmd (cmd);
  }

  void main_window::debug_step_out (void)
  {
    octave_cmd_debug *cmd = new octave_cmd_debug ("out", m_suppress_dbg_location);
    m_cmd_queue.add_cmd (cmd);
  }

  void main_window::debug_quit (void)
  {
    octave_cmd_debug *cmd
      = new octave_cmd_debug ("quit", m_suppress_dbg_location);
    m_cmd_queue.add_cmd (cmd);
  }

  //
  // Functions related to file editing
  //
  // These are moved from editor to here for also using them when octave
  // is built without qscintilla
  //
  void main_window::request_open_file (void)
  {
    // Open file isn't a file_editor_tab or editor function since the file
    // might be opened in an external editor. Hence, functionality is here.

    QSettings *settings = resource_manager::get_settings ();
    bool is_internal = m_editor_window
                       && ! settings->value ("useCustomFileEditor",false).toBool ();

    // Create a NonModal message.
    QWidget *p = this;
    if (is_internal)
      p = m_editor_window;
    QFileDialog *fileDialog = new QFileDialog (p);
    fileDialog->setNameFilter (tr ("Octave Files (*.m);;All Files (*)"));

    // Giving trouble under KDE (problem is related to Qt signal handling on unix,
    // see https://bugs.kde.org/show_bug.cgi?id=260719 ,
    // it had/has no effect on Windows, though)
    fileDialog->setOption (QFileDialog::DontUseNativeDialog, true);

    // define a new grid layout with the extra elements
    QGridLayout *extra = new QGridLayout (fileDialog);
    QFrame *separator = new QFrame (fileDialog);
    separator->setFrameShape (QFrame::HLine);   // horizontal line as separator
    separator->setFrameStyle (QFrame::Sunken);

    if (is_internal)
      {
        // combo box for encoding, only when using the internal editor
        QLabel *label_enc = new QLabel (tr ("File Encoding:"));
        QComboBox *combo_enc = new QComboBox ();
        resource_manager::combo_encoding (combo_enc);
        m_file_encoding = QString ();  // default

        // track changes in the combo boxes
        connect (combo_enc, SIGNAL (currentIndexChanged (QString)),
                 this, SLOT (set_file_encoding (QString)));

        // build the extra grid layout
        extra->addWidget (separator,0,0,1,3);
        extra->addWidget (label_enc,1,0);
        extra->addWidget (combo_enc,1,1);
        extra->addItem   (new QSpacerItem (1,20,QSizePolicy::Expanding,
                                           QSizePolicy::Fixed), 1,2);

        // and add the extra grid layout to the dialog's layout
        QGridLayout *dialog_layout = dynamic_cast<QGridLayout *> (
                                                                  fileDialog->layout ());
        dialog_layout->addLayout (extra,dialog_layout->rowCount (),0,
                                  1,dialog_layout->columnCount ());
      }

    fileDialog->setAcceptMode (QFileDialog::AcceptOpen);
    fileDialog->setViewMode (QFileDialog::Detail);
    fileDialog->setFileMode (QFileDialog::ExistingFiles);
    fileDialog->setDirectory (m_current_directory_combo_box->itemText (0));

    connect (fileDialog, SIGNAL (filesSelected (const QStringList&)),
             this, SLOT (request_open_files (const QStringList&)));

    fileDialog->setWindowModality (Qt::NonModal);
    fileDialog->setAttribute (Qt::WA_DeleteOnClose);
    fileDialog->show ();
  }

  // Create a new script
  void main_window::request_new_script (const QString& commands)
  {
    emit new_file_signal (commands);
  }

  // Create a new function and open it
  void main_window::request_new_function (bool)
  {
    bool ok;
    // Get the name of the new function: Parent of the input dialog is the
    // editor window or the main window. The latter is chosen, if a custom
    // editor is used or qscintilla is not available
    QWidget *p = m_editor_window;
    QSettings *settings = resource_manager::get_settings ();
    if (! p || settings->value ("useCustomFileEditor",false).toBool ())
      p = this;
    QString new_name = QInputDialog::getText (p, tr ("New Function"),
                                              tr ("New function name:\n"), QLineEdit::Normal, "", &ok);

    if (ok && new_name.length () > 0)
      {
        // append suffix if it not already exists
        if (new_name.rightRef (2) != ".m")
          new_name.append (".m");
        // check whether new files are created without prompt
        if (! settings->value ("editor/create_new_file",false).toBool ())
          {
            // no, so enable this settings and wait for end of new file loading
            settings->setValue ("editor/create_new_file",true);
            connect (m_editor_window, SIGNAL (file_loaded_signal (void)),
                     this, SLOT (restore_create_file_setting (void)));
          }
        // start the edit command
        execute_command_in_terminal ("edit " + new_name);
      }
  }

  void main_window::handle_edit_mfile_request (const QString& fname,
                                               const QString& ffile,
                                               const QString& curr_dir,
                                               int line)
  {
    interpreter& interp
      = __get_interpreter__ ("main_window::clear_workspace_callback");

    // Is it a regular function within the search path? (Call __which__)
    octave_value_list fct = F__which__ (interp, ovl (fname.toStdString ()),0);
    octave_map map = fct(0).map_value ();

    std::string type = map.contents ("type").data ()[0].string_value ();
    std::string name = map.contents ("name").data ()[0].string_value ();

    QString message = QString ();
    QString filename = QString ();

    if (type == "built-in function")
      {
        // built in function: can't edit
        message = tr ("%1 is a built-in function");
      }
    else if (type == "")
      {
        // function not known to octave -> try directory of edited file
        // get directory
        QDir dir;
        if (ffile.isEmpty ())
          {
            if (curr_dir.isEmpty ())
              dir = QDir (m_current_directory_combo_box->itemText (0));
            else
              dir = QDir (curr_dir);
          }
        else
          dir = QDir (QFileInfo (ffile).canonicalPath ());

        // function not known to octave -> try directory of edited file
        QFileInfo file = QFileInfo (dir, fname + ".m");

        if (file.exists ())
          {
            filename = file.canonicalFilePath (); // local file exists
          }
        else
          {
            // local file does not exist -> try private directory
            file = QFileInfo (ffile);
            file = QFileInfo (QDir (file.canonicalPath () + "/private"),
                              fname + ".m");

            if (file.exists ())
              {
                filename = file.canonicalFilePath ();  // private function exists
              }
            else
              {
                message = tr ("Can not find function %1");  // no file found
              }
          }
      }

    if (! message.isEmpty ())
      {
        QMessageBox *msgBox
          = new QMessageBox (QMessageBox::Critical,
                             tr ("Octave Editor"),
                             message.arg (QString::fromStdString (name)),
                             QMessageBox::Ok, this);

        msgBox->setWindowModality (Qt::NonModal);
        msgBox->setAttribute (Qt::WA_DeleteOnClose);
        msgBox->show ();
        return;
      }

    if (filename.isEmpty ())
      filename = QString::fromStdString (
                                         map.contents ("file").data ()[0].string_value ());

    if (! filename.endsWith (".m"))
      filename.append (".m");

    emit open_file_signal (filename, QString (), line);  // default encoding
  }

  void main_window::handle_insert_debugger_pointer_request (const QString& file,
                                                            int line)
  {
    bool cmd_focus = command_window_has_focus ();

    emit insert_debugger_pointer_signal (file, line);

    if (cmd_focus)
      focus_command_window ();
  }

  void main_window::handle_delete_debugger_pointer_request (const QString& file,
                                                            int line)
  {
    bool cmd_focus = command_window_has_focus ();

    emit delete_debugger_pointer_signal (file, line);

    if (cmd_focus)
      focus_command_window ();
  }

  void main_window::handle_update_breakpoint_marker_request (bool insert,
                                                             const QString& file,
                                                             int line,
                                                             const QString& cond)
  {
    bool cmd_focus = command_window_has_focus ();

    emit update_breakpoint_marker_signal (insert, file, line, cond);

    if (cmd_focus)
      focus_command_window ();
  }

  void main_window::read_settings (void)
  {
    QSettings *settings = resource_manager::get_settings ();

    if (! settings)
      {
        qDebug ("Error: QSettings pointer from resource manager is NULL.");
        return;
      }

    set_window_layout (settings);

    // restore the list of the last directories
    QStringList curr_dirs
      = settings->value ("MainWindow/current_directory_list").toStringList ();
    for (int i=0; i < curr_dirs.size (); i++)
      {
        m_current_directory_combo_box->addItem (curr_dirs.at (i));
      }
    emit settings_changed (settings);
  }

  void main_window::init_terminal_size (void)
  {
    emit init_terminal_size_signal ();
  }

  void main_window::set_window_layout (QSettings *settings)
  {
    restoreState (settings->value ("MainWindow/windowState").toByteArray ());
    restoreGeometry (settings->value ("MainWindow/geometry").toByteArray ());

    // Restore the geometry of all dock-widgets
    foreach (octave_dock_widget *widget, dock_widget_list ())
      {
        QString name = widget->objectName ();

        if (! name.isEmpty ())
          {
            bool floating = settings->value
              ("DockWidgets/" + name + "Floating", false).toBool ();
            bool visible = settings->value
              ("DockWidgets/" + name + "Visible", true).toBool ();

            // If floating, make window from widget.
            if (floating)
              {
                widget->make_window ();

                if (visible)
                  {
                    if (settings->value ("DockWidgets/" + name
                                         + "_minimized").toBool ())
                      widget->showMinimized ();
                    else
                      widget->setVisible (true);
                  }
              }
            else  // not floating
              {
                if (! widget->parent ())        // should not be floating but is
                  widget->make_widget (false);  // no docking, just reparent

                widget->make_widget ();
                widget->setVisible (visible);   // not floating -> show
              }
          }
      }

    show ();
  }

  void main_window::write_settings (void)
  {
    QSettings *settings = resource_manager::get_settings ();
    if (! settings)
      {
        qDebug ("Error: QSettings pointer from resource manager is NULL.");
        return;
      }

    settings->setValue ("MainWindow/geometry", saveGeometry ());
    settings->setValue ("MainWindow/windowState", saveState ());
    // write the list of recent used directories
    QStringList curr_dirs;
    for (int i=0; i<m_current_directory_combo_box->count (); i++)
      {
        curr_dirs.append (m_current_directory_combo_box->itemText (i));
      }
    settings->setValue ("MainWindow/current_directory_list", curr_dirs);
    settings->sync ();
  }

  // Connecting the signals emitted when the visibility of a widget changes.
  // This has to be done after the window is shown (see octave-gui.cc)
  void main_window::connect_visibility_changed (void)
  {
    foreach (octave_dock_widget *widget, dock_widget_list ())
      widget->connect_visibility_changed ();

#if defined (HAVE_QSCINTILLA)
    m_editor_window->enable_menu_shortcuts (false);
#endif
  }

  void main_window::copyClipboard (void)
  {
    if (m_current_directory_combo_box->hasFocus ())
      {
        QLineEdit *edit = m_current_directory_combo_box->lineEdit ();
        if (edit && edit->hasSelectedText ())
          {
            QClipboard *clipboard = QApplication::clipboard ();
            clipboard->setText (edit->selectedText ());
          }
      }
    else
      emit copyClipboard_signal ();
  }

  void main_window::pasteClipboard (void)
  {
    if (m_current_directory_combo_box->hasFocus ())
      {
        QLineEdit *edit = m_current_directory_combo_box->lineEdit ();
        QClipboard *clipboard = QApplication::clipboard ();
        QString str = clipboard->text ();
        if (edit && str.length () > 0)
          {
            edit->insert (str);
          }
      }
    else
      emit pasteClipboard_signal ();
  }

  void main_window::selectAll (void)
  {
    if (m_current_directory_combo_box->hasFocus ())
      {
        QLineEdit *edit = m_current_directory_combo_box->lineEdit ();
        if (edit)
          {
            edit->selectAll ();
          }
      }
    else
      emit selectAll_signal ();
  }

  // Connect the signals emitted when the Octave thread wants to create
  // a dialog box of some sort.  Perhaps a better place for this would be
  // as part of the QUIWidgetCreator class.  However, mainWindow currently
  // is not a global variable and not accessible for connecting.

  void main_window::connect_uiwidget_links (void)
  {
    connect (&uiwidget_creator,
             SIGNAL (create_dialog (const QString&, const QString&,
                                    const QString&, const QStringList&,
                                    const QString&, const QStringList&)),
             this,
             SLOT (handle_create_dialog (const QString&, const QString&,
                                         const QString&, const QStringList&,
                                         const QString&, const QStringList&)));

    // Register QIntList so that list of ints may be part of a signal.
    qRegisterMetaType<QIntList> ("QIntList");
    connect (&uiwidget_creator,
             SIGNAL (create_listview (const QStringList&, const QString&,
                                      int, int, const QIntList&,
                                      const QString&, const QStringList&,
                                      const QString&, const QString&)),
             this,
             SLOT (handle_create_listview (const QStringList&, const QString&,
                                           int, int, const QIntList&,
                                           const QString&, const QStringList&,
                                           const QString&, const QString&)));

    // Register QFloatList so that list of floats may be part of a signal.
    qRegisterMetaType<QFloatList> ("QFloatList");
    connect (&uiwidget_creator,
             SIGNAL (create_inputlayout (const QStringList&, const QString&,
                                         const QFloatList&, const QFloatList&,
                                         const QStringList&)),
             this,
             SLOT (handle_create_inputlayout (const QStringList&, const QString&,
                                              const QFloatList&,
                                              const QFloatList&,
                                              const QStringList&)));

    connect (&uiwidget_creator,
             SIGNAL (create_filedialog (const QStringList &,const QString&,
                                        const QString&, const QString&,
                                        const QString&)),
             this,
             SLOT (handle_create_filedialog (const QStringList &, const QString&,
                                             const QString&, const QString&,
                                             const QString&)));
  }

  // Create a message dialog with specified string, buttons and decorative
  // text.

  void main_window::handle_create_dialog (const QString& message,
                                          const QString& title,
                                          const QString& icon,
                                          const QStringList& button,
                                          const QString& defbutton,
                                          const QStringList& role)
  {
    MessageDialog *message_dialog = new MessageDialog (message, title, icon,
                                                       button, defbutton, role);
    message_dialog->setAttribute (Qt::WA_DeleteOnClose);
    message_dialog->show ();
  }

  // Create a list dialog with specified list, initially selected, mode,
  // view size and decorative text.

  void main_window::handle_create_listview (const QStringList& list,
                                            const QString& mode,
                                            int wd, int ht,
                                            const QIntList& initial,
                                            const QString& name,
                                            const QStringList& prompt,
                                            const QString& ok_string,
                                            const QString& cancel_string)
  {
    ListDialog *list_dialog = new ListDialog (list, mode, wd, ht,
                                              initial, name, prompt,
                                              ok_string, cancel_string);

    list_dialog->setAttribute (Qt::WA_DeleteOnClose);
    list_dialog->show ();
  }

  // Create an input dialog with specified prompts and defaults, title and
  // row/column size specifications.
  void main_window::handle_create_inputlayout (const QStringList& prompt,
                                               const QString& title,
                                               const QFloatList& nr,
                                               const QFloatList& nc,
                                               const QStringList& defaults)
  {
    InputDialog *input_dialog = new InputDialog (prompt, title, nr, nc,
                                                 defaults);

    input_dialog->setAttribute (Qt::WA_DeleteOnClose);
    input_dialog->show ();
  }

  void main_window::handle_create_filedialog (const QStringList& filters,
                                              const QString& title,
                                              const QString& filename,
                                              const QString& dirname,
                                              const QString& multimode)
  {
    FileDialog *file_dialog = new FileDialog (filters, title, filename,
                                              dirname, multimode);

    file_dialog->setAttribute (Qt::WA_DeleteOnClose);
    file_dialog->show ();
  }

  void main_window::handle_show_doc (const QString& file)
  {
    m_doc_browser_window->setVisible (true);
    emit show_doc_signal (file);
  }

  void main_window::handle_register_doc (const QString& file)
  {
    emit register_doc_signal (file);
  }

  void main_window::handle_unregister_doc (const QString& file)
  {
    emit unregister_doc_signal (file);
  }

  void main_window::handle_octave_ready (void)
  {
    // actions after the startup files are executed
    QSettings *settings = resource_manager::get_settings ();

    QDir startup_dir = QDir ();    // current octave dir after startup

    if (settings)
      {
        if (settings->value ("restore_octave_dir").toBool ())
          {
            // restore last dir from previous session
            QStringList curr_dirs
              = settings->value ("MainWindow/current_directory_list").toStringList ();
            startup_dir
              = QDir (curr_dirs.at (0));  // last dir in previous session
          }
        else if (! settings->value ("octave_startup_dir").toString ().isEmpty ())
          {
            // do not restore but there is a startup dir configured
            startup_dir
              = QDir (settings->value ("octave_startup_dir").toString ());
          }
      }

    if (! startup_dir.exists ())
      {
        // the configured startup dir does not exist, take actual one
        startup_dir = QDir ();
      }

    set_current_working_directory (startup_dir.absolutePath ());

    if (m_editor_window)
      {
#if defined (HAVE_QSCINTILLA)
        // Octave ready, determine whether to create an empty script.
        // This can not be done when the editor is created because all functions
        // must be known for the lexer's auto completion informations
        m_editor_window->empty_script (true, false);
        m_editor_window->restore_session (settings);
#endif
      }

    if (m_start_gui)
      focus_command_window ();  // make sure that the command window has focus
  }

  void main_window::handle_octave_finished (int exit_status)
  {
    qApp->exit (exit_status);
  }

  void main_window::find_files (const QString& start_dir)
  {

    if (! m_find_files_dlg)
      {
        m_find_files_dlg = new find_files_dialog (this);

        connect (m_find_files_dlg, SIGNAL (finished (int)),
                 this, SLOT (find_files_finished (int)));

        connect (m_find_files_dlg, SIGNAL (dir_selected (const QString &)),
                 m_file_browser_window,
                 SLOT (set_current_directory (const QString&)));

        connect (m_find_files_dlg, SIGNAL (file_selected (const QString &)),
                 this, SLOT (open_file (const QString &)));

        m_find_files_dlg->setWindowModality (Qt::NonModal);
      }

    if (! m_find_files_dlg->isVisible ())
      {
        m_find_files_dlg->show ();
      }

    m_find_files_dlg->set_search_dir (start_dir);

    m_find_files_dlg->activateWindow ();

  }

  void main_window::set_global_shortcuts (bool set_shortcuts)
  {
    // this slot is called when the terminal gets/loses focus

    // return if the user don't want to use readline shortcuts
    if (! m_prevent_readline_conflicts)
      return;

    if (set_shortcuts)
      {
        // terminal loses focus: set the global shortcuts
        configure_shortcuts ();
      }
    else
      {
        // terminal gets focus: disable some shortcuts
        QKeySequence no_key = QKeySequence ();

        // file menu
        m_open_action->setShortcut (no_key);
        m_new_script_action->setShortcut (no_key);
        m_new_function_action->setShortcut (no_key);
        m_new_function_action->setShortcut (no_key);
        m_load_workspace_action->setShortcut (no_key);
        m_save_workspace_action->setShortcut (no_key);
        m_preferences_action->setShortcut (no_key);
        m_exit_action->setShortcut (no_key);

        // edit menu
        m_select_all_action->setShortcut (no_key);
        m_clear_clipboard_action->setShortcut (no_key);
        m_find_files_action->setShortcut (no_key);
        m_clear_command_history_action->setShortcut (no_key);
        m_clear_command_window_action->setShortcut (no_key);
        m_clear_workspace_action->setShortcut (no_key);

        // window menu
        m_reset_windows_action->setShortcut (no_key);

        // help menu
        m_ondisk_doc_action->setShortcut (no_key);
        m_online_doc_action->setShortcut (no_key);
        m_report_bug_action->setShortcut (no_key);
        m_octave_packages_action->setShortcut (no_key);
        m_contribute_action->setShortcut (no_key);
        m_developer_action->setShortcut (no_key);
        m_about_octave_action->setShortcut (no_key);

        // news menu
        m_release_notes_action->setShortcut (no_key);
        m_current_news_action->setShortcut (no_key);
      }
  }

  void main_window::set_screen_size (int ht, int wd)
  {
    octave_link::post_event (this, &main_window::set_screen_size_callback,
                             int_pair (ht, wd));
  }

  void main_window::clipboard_has_changed (void)
  {
    if (m_clipboard->text ().isEmpty ())
      {
        m_paste_action->setEnabled (false);
        m_clear_clipboard_action->setEnabled (false);
      }
    else
      {
        m_paste_action->setEnabled (true);
        m_clear_clipboard_action->setEnabled (true);
      }
  }

  void main_window::clear_clipboard (void)
  {
    m_clipboard->clear (QClipboard::Clipboard);
  }

  void main_window::disable_menu_shortcuts (bool disable)
  {
    QHash<QMenu*, QStringList>::const_iterator i = m_hash_menu_text.constBegin ();

    while (i != m_hash_menu_text.constEnd ())
      {
        i.key ()->setTitle (i.value ().at (disable));
        ++i;
      }
  }

  void main_window::restore_create_file_setting (void)
  {
    // restore the new files creation setting
    QSettings *settings = resource_manager::get_settings ();
    settings->setValue ("editor/create_new_file",false);
    disconnect (m_editor_window, SIGNAL (file_loaded_signal (void)),
                this, SLOT (restore_create_file_setting (void)));
  }

  void main_window::set_file_encoding (const QString& new_encoding)
  {
    m_file_encoding = new_encoding;
  }

  // The following slot is called after files have been selected in the
  // open file dialog., possibly with a new selected encoding stored in
  // m_file_encoding
  void main_window::request_open_files (const QStringList& open_file_names)
  {
    for (int i = 0; i < open_file_names.count (); i++)
      emit open_file_signal (open_file_names.at (i), m_file_encoding, -1);
  }

  void main_window::edit_variable (const QString &expr, const octave_value& val)
  {
    m_variable_editor_window->edit_variable (expr, val);

    if (! m_variable_editor_window->isVisible ())
      {
        m_variable_editor_window->show ();
        m_variable_editor_window->raise ();
      }

  }

  void main_window::refresh_variable_editor (void)
  {
    m_variable_editor_window->refresh ();
  }

  void main_window::handle_variable_editor_update (void)
  {
    // Called when the variable editor emits the updated signal.  The size
    // of a variable may have changed, so we refresh the workspace in the
    // interpreter.  That will eventually cause the workspace view in the
    // GUI to be updated.

    octave_link::post_event (this, &main_window::refresh_workspace_callback);
  }

  void main_window::closeEvent (QCloseEvent *e)
  {
    e->ignore ();
    octave_cmd_exec *cmd = new octave_cmd_exec ("exit");
    m_cmd_queue.add_cmd (cmd);
  }

  // Main subroutine of the constructor

  void main_window::construct (void)
  {
    m_closing = false;   // flag for editor files when closed

    // Create and set the central widget.  QMainWindow takes ownership of
    // the widget (pointer) so there is no need to delete the object upon
    // destroying this main_window.

    QWidget *dummyWidget = new QWidget ();
    dummyWidget->setObjectName ("CentralDummyWidget");
    dummyWidget->resize (10, 10);
    dummyWidget->setSizePolicy (QSizePolicy::Minimum, QSizePolicy::Minimum);
    dummyWidget->hide ();
    setCentralWidget (dummyWidget);

    connect_uiwidget_links ();

    construct_octave_qt_link ();

    if (m_start_gui)
      {
        setWindowIcon (QIcon (":/actions/icons/logo.png"));

        m_workspace_window->setModel (m_workspace_model);

        connect (m_workspace_model, SIGNAL (model_changed (void)),
                 m_workspace_window, SLOT (handle_model_changed (void)));

        connect (m_octave_qt_link,
                 SIGNAL (edit_variable_signal (const QString&,
                                               const octave_value&)),
                 this,
                 SLOT (edit_variable (const QString&, const octave_value&)));

        connect (m_octave_qt_link, SIGNAL (refresh_variable_editor_signal (void)),
                 this, SLOT (refresh_variable_editor (void)));

        connect (m_workspace_model,
                 SIGNAL (rename_variable (const QString&, const QString&)),
                 this,
                 SLOT (handle_rename_variable_request (const QString&,
                                                       const QString&)));

        connect (m_variable_editor_window, SIGNAL (updated (void)),
                 this, SLOT (handle_variable_editor_update (void)));

        construct_menu_bar ();

        construct_tool_bar ();

        // Order is important.  Deleting QSettings must be last.
        connect (qApp, SIGNAL (aboutToQuit (void)),
                 m_command_window, SLOT (save_settings (void)));

        connect (qApp, SIGNAL (aboutToQuit (void)),
                 m_history_window, SLOT (save_settings (void)));

        connect (qApp, SIGNAL (aboutToQuit (void)),
                 m_file_browser_window, SLOT (save_settings (void)));

        connect (qApp, SIGNAL (aboutToQuit (void)),
                 m_doc_browser_window, SLOT (save_settings (void)));

        connect (qApp, SIGNAL (aboutToQuit (void)),
                 m_workspace_window, SLOT (save_settings (void)));

        connect (qApp, SIGNAL (aboutToQuit (void)),
                 m_editor_window, SLOT (save_settings (void)));

        connect (qApp, SIGNAL (aboutToQuit (void)),
                 m_variable_editor_window, SLOT (save_settings (void)));

        connect (qApp, SIGNAL (aboutToQuit (void)),
                 this, SLOT (prepare_to_exit (void)));

        connect (qApp, SIGNAL (aboutToQuit (void)),
                 shortcut_manager::instance, SLOT (cleanup_instance (void)));

        // QSettings are saved upon deletion (i.e., cleanup_instance)
        connect (qApp, SIGNAL (aboutToQuit (void)),
                 resource_manager::instance, SLOT (cleanup_instance (void)));

        connect (qApp, SIGNAL (focusChanged (QWidget*, QWidget*)),
                 this, SLOT (focus_changed (QWidget*, QWidget*)));

        connect (this, SIGNAL (settings_changed (const QSettings *)),
                 this, SLOT (notice_settings (const QSettings *)));

        connect (this, SIGNAL (editor_focus_changed (bool)),
                 this, SLOT (disable_menu_shortcuts (bool)));

        connect (this, SIGNAL (editor_focus_changed (bool)),
                 m_editor_window, SLOT (enable_menu_shortcuts (bool)));

        connect (m_editor_window,
                 SIGNAL (request_open_file_external (const QString&, int)),
                 m_external_editor,
                 SLOT (call_custom_editor (const QString&, int)));

        connect (m_external_editor,
                 SIGNAL (request_settings_dialog (const QString&)),
                 this, SLOT (process_settings_dialog_request (const QString&)));

        connect (m_file_browser_window, SIGNAL (load_file_signal (const QString&)),
                 this, SLOT (handle_load_workspace_request (const QString&)));

        connect (m_file_browser_window, SIGNAL (open_any_signal (const QString&)),
                 this, SLOT (handle_open_any_request (const QString&)));

        connect (m_file_browser_window, SIGNAL (find_files_signal (const QString&)),
                 this, SLOT (find_files (const QString&)));

        setWindowTitle ("Octave");

// See Octave bug #53409 and https://bugreports.qt.io/browse/QTBUG-55357
#if (QT_VERSION < 0x050601) || (QT_VERSION >= 0x050701)
        setDockOptions (QMainWindow::AnimatedDocks
                        | QMainWindow::AllowNestedDocks
                        | QMainWindow::AllowTabbedDocks);
#else
        setDockNestingEnabled (true);
#endif

        addDockWidget (Qt::RightDockWidgetArea, m_command_window);
        addDockWidget (Qt::RightDockWidgetArea, m_doc_browser_window);
        tabifyDockWidget (m_command_window, m_doc_browser_window);

#if defined (HAVE_QSCINTILLA)
        addDockWidget (Qt::RightDockWidgetArea, m_editor_window);
        tabifyDockWidget (m_command_window, m_editor_window);
#endif
        addDockWidget (Qt::RightDockWidgetArea, m_variable_editor_window);
        tabifyDockWidget (m_command_window, m_variable_editor_window);

        addDockWidget (Qt::LeftDockWidgetArea, m_file_browser_window);
        addDockWidget (Qt::LeftDockWidgetArea, m_workspace_window);
        addDockWidget (Qt::LeftDockWidgetArea, m_history_window);

        int win_x = QApplication::desktop ()->width ();
        int win_y = QApplication::desktop ()->height ();

        if (win_x > 960)
          win_x = 960;

        if (win_y > 720)
          win_y = 720;

        setGeometry (0, 0, win_x, win_y);

        setStatusBar (m_status_bar);

#if defined (HAVE_QSCINTILLA)
        connect (this,
                 SIGNAL (insert_debugger_pointer_signal (const QString&, int)),
                 m_editor_window,
                 SLOT (handle_insert_debugger_pointer_request (const QString&,
                                                               int)));

        connect (this,
                 SIGNAL (delete_debugger_pointer_signal (const QString&, int)),
                 m_editor_window,
                 SLOT (handle_delete_debugger_pointer_request (const QString&,
                                                               int)));

        connect (this,
                 SIGNAL (update_breakpoint_marker_signal (bool, const QString&,
                                                          int, const QString&)),
                 m_editor_window,
                 SLOT (handle_update_breakpoint_marker_request (bool,
                                                                const QString&,
                                                                int,
                                                                const QString&)));

        connect (m_file_browser_window,
                 SIGNAL (file_remove_signal (const QString&, const QString&)),
                 m_editor_window,
                 SLOT (handle_file_remove (const QString&, const QString&)));

        connect (m_file_browser_window, SIGNAL (file_renamed_signal (bool)),
                 m_editor_window, SLOT (handle_file_renamed (bool)));
#endif

        octave_link::post_event (this,
                                 &main_window::resize_command_window_callback);

        configure_shortcuts ();
      }
  }

  void main_window::construct_octave_qt_link (void)
  {
    m_octave_qt_link = new octave_qt_link (this, m_app_context);

    octave_link::connect_link (m_octave_qt_link);

    connect (m_octave_qt_link, SIGNAL (confirm_shutdown_signal (void)),
             this, SLOT (confirm_shutdown_octave (void)));

    connect (m_octave_qt_link,
             SIGNAL (copy_image_to_clipboard_signal (const QString&, bool)),
             this, SLOT (copy_image_to_clipboard (const QString&, bool)));

    if (m_start_gui)
      {
        connect (m_octave_qt_link,
                 SIGNAL (set_workspace_signal (bool, bool,
                                               const symbol_scope&)),
                 m_workspace_model,
                 SLOT (set_workspace (bool, bool, const symbol_scope&)));

        connect (m_octave_qt_link, SIGNAL (clear_workspace_signal (void)),
                 m_workspace_model, SLOT (clear_workspace (void)));

        connect (m_octave_qt_link, SIGNAL (change_directory_signal (QString)),
                 this, SLOT (change_directory (QString)));

        connect (m_octave_qt_link, SIGNAL (change_directory_signal (QString)),
                 m_file_browser_window, SLOT (update_octave_directory (QString)));

        connect (m_octave_qt_link, SIGNAL (change_directory_signal (QString)),
                 m_editor_window, SLOT (update_octave_directory (QString)));

        connect (m_octave_qt_link,
                 SIGNAL (execute_command_in_terminal_signal (QString)),
                 this, SLOT (execute_command_in_terminal (QString)));

        connect (m_octave_qt_link,
                 SIGNAL (set_history_signal (const QStringList&)),
                 m_history_window, SLOT (set_history (const QStringList&)));

        connect (m_octave_qt_link,
                 SIGNAL (append_history_signal (const QString&)),
                 m_history_window, SLOT (append_history (const QString&)));

        connect (m_octave_qt_link,
                 SIGNAL (clear_history_signal (void)),
                 m_history_window, SLOT (clear_history (void)));

        connect (m_octave_qt_link, SIGNAL (enter_debugger_signal (void)),
                 this, SLOT (handle_enter_debugger (void)));

        connect (m_octave_qt_link, SIGNAL (exit_debugger_signal (void)),
                 this, SLOT (handle_exit_debugger (void)));

        connect (m_octave_qt_link,
                 SIGNAL (show_preferences_signal (void)),
                 this, SLOT (process_settings_dialog_request (void)));

        connect (m_octave_qt_link,
                 SIGNAL (gui_preference_signal (const QString&, const QString&,
                                                QString*)),
                 this, SLOT (gui_preference (const QString&, const QString&,
                                             QString*)));

        connect (m_octave_qt_link,
                 SIGNAL (edit_file_signal (const QString&)),
                 m_active_editor,
                 SLOT (handle_edit_file_request (const QString&)));

        connect (m_octave_qt_link,
                 SIGNAL (insert_debugger_pointer_signal (const QString&, int)),
                 this,
                 SLOT (handle_insert_debugger_pointer_request (const QString&,
                                                               int)));

        connect (m_octave_qt_link,
                 SIGNAL (delete_debugger_pointer_signal (const QString&, int)),
                 this,
                 SLOT (handle_delete_debugger_pointer_request (const QString&,
                                                               int)));

        connect (m_octave_qt_link,
                 SIGNAL (update_breakpoint_marker_signal (bool, const QString&,
                                                          int, const QString&)),
                 this,
                 SLOT (handle_update_breakpoint_marker_request (bool, const QString&,
                                                                int, const QString&)));

        connect (m_octave_qt_link,
                 SIGNAL (show_doc_signal (const QString &)),
                 this, SLOT (handle_show_doc (const QString &)));

        connect (m_octave_qt_link,
                 SIGNAL (register_doc_signal (const QString &)),
                 this, SLOT (handle_register_doc (const QString &)));

        connect (m_octave_qt_link,
                 SIGNAL (unregister_doc_signal (const QString &)),
                 this, SLOT (handle_unregister_doc (const QString &)));
      }

    // Defer initializing and executing the interpreter until after the main
    // window and QApplication are running to prevent race conditions
    QTimer::singleShot (0, m_interpreter, SLOT (execute (void)));
  }

  QAction* main_window::add_action (QMenu *menu, const QIcon& icon,
                                    const QString& text, const char *member,
                                    const QWidget *receiver)
  {
    QAction *a;

    if (receiver)
      a = menu->addAction (icon, text, receiver, member);
    else
      a = menu->addAction (icon, text, this, member);

    addAction (a);  // important for shortcut context
    a->setShortcutContext (Qt::ApplicationShortcut);
    return a;
  }

  QMenu* main_window::m_add_menu (QMenuBar *p, QString name)
  {
    QMenu *menu = p->addMenu (name);

    QString base_name = name;  // get a copy
    // replace intended '&' ("&&") by a temp. string
    base_name.replace ("&&", "___octave_amp_replacement___");
    // remove single '&' (shortcut)
    base_name.remove ("&");
    // restore intended '&'
    base_name.replace ("___octave_amp_replacement___", "&&");

    // remember names with and without shortcut
    m_hash_menu_text[menu] = QStringList () << name << base_name;

    return menu;
  }

  void main_window::construct_menu_bar (void)
  {
    QMenuBar *menu_bar = menuBar ();

    construct_file_menu (menu_bar);

    construct_edit_menu (menu_bar);

    construct_debug_menu (menu_bar);

    construct_window_menu (menu_bar);

    construct_help_menu (menu_bar);

    construct_news_menu (menu_bar);

#if defined (HAVE_QSCINTILLA)
    // call the editor to add actions which should also be available in the
    // editor's menu and tool bar
    QList<QAction*> shared_actions;
    shared_actions << m_new_script_action
                   << m_new_function_action
                   << m_open_action
                   << m_find_files_action
                   << m_undo_action
                   << m_copy_action
                   << m_paste_action
                   <<m_select_all_action;
    m_editor_window->insert_global_actions (shared_actions);
#endif
  }

  void main_window::construct_file_menu (QMenuBar *p)
  {
    QMenu *file_menu = m_add_menu (p, tr ("&File"));

    construct_new_menu (file_menu);

    m_open_action
      = file_menu->addAction (resource_manager::icon ("document-open"),
                              tr ("Open..."));
    m_open_action->setShortcutContext (Qt::ApplicationShortcut);
    m_open_action->setToolTip (tr ("Open an existing file in editor"));

#if defined (HAVE_QSCINTILLA)
    file_menu->addMenu (m_editor_window->get_mru_menu ());
#endif

    file_menu->addSeparator ();

    m_load_workspace_action
      = file_menu->addAction (tr ("Load Workspace..."));

    m_save_workspace_action
      = file_menu->addAction (tr ("Save Workspace As..."));

    file_menu->addSeparator ();

    m_exit_action = file_menu->addAction (tr ("Exit"));
    m_exit_action->setShortcutContext (Qt::ApplicationShortcut);

    connect (m_open_action, SIGNAL (triggered (void)),
             this, SLOT (request_open_file (void)));

    connect (m_load_workspace_action, SIGNAL (triggered (void)),
             this, SLOT (handle_load_workspace_request (void)));

    connect (m_save_workspace_action, SIGNAL (triggered (void)),
             this, SLOT (handle_save_workspace_request (void)));

    connect (m_exit_action, SIGNAL (triggered (void)),
             this, SLOT (close (void)));
  }

  void main_window::construct_new_menu (QMenu *p)
  {
    QMenu *new_menu = p->addMenu (tr ("New"));

    m_new_script_action
      = new_menu->addAction (resource_manager::icon ("document-new"),
                             tr ("New Script"));
    m_new_script_action->setShortcutContext (Qt::ApplicationShortcut);

    m_new_function_action = new_menu->addAction (tr ("New Function..."));
    m_new_function_action->setEnabled (true);
    m_new_function_action->setShortcutContext (Qt::ApplicationShortcut);

    m_new_figure_action = new_menu->addAction (tr ("New Figure"));
    m_new_figure_action->setEnabled (true);

    connect (m_new_script_action, SIGNAL (triggered (void)),
             this, SLOT (request_new_script (void)));

    connect (m_new_function_action, SIGNAL (triggered (void)),
             this, SLOT (request_new_function (void)));

    connect (this, SIGNAL (new_file_signal (const QString&)),
             m_active_editor, SLOT (request_new_file (const QString&)));

    connect (this, SIGNAL (open_file_signal (const QString&)),
             m_active_editor, SLOT (request_open_file (const QString&)));

    connect (this,
             SIGNAL (open_file_signal (const QString&, const QString&, int)),
             m_active_editor,
             SLOT (request_open_file (const QString&, const QString&, int)));

    connect (m_new_figure_action, SIGNAL (triggered (void)),
             this, SLOT (handle_new_figure_request (void)));
  }

  void main_window::construct_edit_menu (QMenuBar *p)
  {
    QMenu *edit_menu = m_add_menu (p, tr ("&Edit"));

    QKeySequence ctrl_shift = Qt::ControlModifier + Qt::ShiftModifier;

    m_undo_action
      = edit_menu->addAction (resource_manager::icon ("edit-undo"), tr ("Undo"));
    m_undo_action->setShortcutContext (Qt::ApplicationShortcut);

    edit_menu->addSeparator ();

    m_copy_action
      = edit_menu->addAction (resource_manager::icon ("edit-copy"),
                              tr ("Copy"), this, SLOT (copyClipboard (void)));
    m_copy_action->setShortcutContext (Qt::ApplicationShortcut);

    m_paste_action
      = edit_menu->addAction (resource_manager::icon ("edit-paste"),
                              tr ("Paste"), this, SLOT (pasteClipboard (void)));
    m_paste_action->setShortcutContext (Qt::ApplicationShortcut);

    m_select_all_action
      = edit_menu->addAction (tr ("Select All"), this, SLOT (selectAll (void)));
    m_select_all_action->setShortcutContext (Qt::ApplicationShortcut);

    m_clear_clipboard_action
      = edit_menu->addAction (tr ("Clear Clipboard"), this,
                              SLOT (clear_clipboard (void)));

    edit_menu->addSeparator ();

    m_find_files_action
      = edit_menu->addAction (resource_manager::icon ("edit-find"),
                              tr ("Find Files..."));

    edit_menu->addSeparator ();

    m_clear_command_window_action
      = edit_menu->addAction (tr ("Clear Command Window"));

    m_clear_command_history_action
      = edit_menu->addAction (tr ("Clear Command History"));

    m_clear_workspace_action
      = edit_menu->addAction (tr ("Clear Workspace"));

    edit_menu->addSeparator ();

    m_preferences_action
      = edit_menu->addAction (resource_manager::icon ("preferences-system"),
                              tr ("Preferences..."));

    connect (m_find_files_action, SIGNAL (triggered (void)),
             this, SLOT (find_files (void)));

    connect (m_clear_command_window_action, SIGNAL (triggered (void)),
             this, SLOT (handle_clear_command_window_request (void)));

    connect (m_clear_command_history_action, SIGNAL (triggered (void)),
             this, SLOT (handle_clear_history_request (void)));

    connect (m_clear_workspace_action, SIGNAL (triggered (void)),
             this, SLOT (handle_clear_workspace_request (void)));

    connect (m_clipboard, SIGNAL (dataChanged (void)),
             this, SLOT (clipboard_has_changed (void)));
    clipboard_has_changed ();
#if defined (Q_OS_WIN32)
    // Always enable paste action (unreliable clipboard signals in windows)
    // FIXME: This has to be removed, when the clipboards signals in windows
    //        are working again
    m_paste_action->setEnabled (true);
    m_clear_clipboard_action->setEnabled (true);
#endif

    connect (m_preferences_action, SIGNAL (triggered (void)),
             this, SLOT (process_settings_dialog_request (void)));
  }

  QAction * main_window::construct_debug_menu_item (const char *icon,
                                                    const QString& item,
                                                    const char *member)
  {
    QAction *action = add_action (m_debug_menu,
                                  resource_manager::icon (QString (icon)),
                                  item, member);

    action->setEnabled (false);

#if defined (HAVE_QSCINTILLA)
    m_editor_window->debug_menu ()->addAction (action);
    m_editor_window->toolbar ()->addAction (action);
#endif

    return action;
  }

  void main_window::construct_debug_menu (QMenuBar *p)
  {
    m_debug_menu = m_add_menu (p, tr ("De&bug"));

    m_debug_step_over = construct_debug_menu_item (
                                                   "db-step", tr ("Step"),
                                                   SLOT (debug_step_over (void)));

    m_debug_step_into = construct_debug_menu_item (
                                                   "db-step-in", tr ("Step In"),
                                                   SLOT (debug_step_into (void)));

    m_debug_step_out = construct_debug_menu_item (
                                                  "db-step-out", tr ("Step Out"),
                                                  SLOT (debug_step_out (void)));

    m_debug_continue = construct_debug_menu_item (
                                                  "db-cont", tr ("Continue"),
                                                  SLOT (debug_continue (void)));

    m_debug_menu->addSeparator ();
#if defined (HAVE_QSCINTILLA)
    m_editor_window->debug_menu ()->addSeparator ();
#endif

    m_debug_quit = construct_debug_menu_item (
                                              "db-stop", tr ("Quit Debug Mode"),
                                              SLOT (debug_quit (void)));
  }

  QAction * main_window::construct_window_menu_item (QMenu *p,
                                                     const QString& item,
                                                     bool checkable,
                                                     QWidget *widget)
  {
    QAction *action = p->addAction (QIcon (), item);

    addAction (action);  // important for shortcut context
    action->setCheckable (checkable);
    action->setShortcutContext (Qt::ApplicationShortcut);

    if (widget)  // might be zero for m_editor_window
      {
        if (checkable)
          {
            // action for visibilty of dock widget
            connect (action, SIGNAL (toggled (bool)),
                     widget, SLOT (setVisible (bool)));

            connect (widget, SIGNAL (active_changed (bool)),
                     action, SLOT (setChecked (bool)));
          }
        else
          {
            // action for focus of dock widget
            connect (action, SIGNAL (triggered (void)), widget, SLOT (focus (void)));
          }
      }

    return action;
  }

  void main_window::construct_window_menu (QMenuBar *p)
  {
    QMenu *window_menu = m_add_menu (p, tr ("&Window"));

    m_show_command_window_action = construct_window_menu_item
      (window_menu, tr ("Show Command Window"), true, m_command_window);

    m_show_history_action = construct_window_menu_item
      (window_menu, tr ("Show Command History"), true, m_history_window);

    m_show_file_browser_action = construct_window_menu_item
      (window_menu, tr ("Show File Browser"), true, m_file_browser_window);

    m_show_workspace_action = construct_window_menu_item
      (window_menu, tr ("Show Workspace"), true, m_workspace_window);

    m_show_editor_action = construct_window_menu_item
      (window_menu, tr ("Show Editor"), true, m_editor_window);

    m_show_documentation_action = construct_window_menu_item
      (window_menu, tr ("Show Documentation"), true, m_doc_browser_window);

    m_show_variable_editor_action = construct_window_menu_item
      (window_menu, tr ("Show Variable Editor"), true, m_variable_editor_window);

    window_menu->addSeparator ();

    m_command_window_action = construct_window_menu_item
      (window_menu, tr ("Command Window"), false, m_command_window);

    m_history_action = construct_window_menu_item
      (window_menu, tr ("Command History"), false, m_history_window);

    m_file_browser_action = construct_window_menu_item
      (window_menu, tr ("File Browser"), false, m_file_browser_window);

    m_workspace_action = construct_window_menu_item
      (window_menu, tr ("Workspace"), false, m_workspace_window);

    m_editor_action = construct_window_menu_item
      (window_menu, tr ("Editor"), false, m_editor_window);

    m_documentation_action = construct_window_menu_item
      (window_menu, tr ("Documentation"), false, m_doc_browser_window);

    m_variable_editor_action = construct_window_menu_item
      (window_menu, tr ("Variable Editor"), false, m_variable_editor_window);

    window_menu->addSeparator ();

    m_reset_windows_action = add_action (window_menu, QIcon (),
                                         tr ("Reset Default Window Layout"), SLOT (reset_windows (void)));
  }

  void main_window::construct_help_menu (QMenuBar *p)
  {
    QMenu *help_menu = m_add_menu (p, tr ("&Help"));

    construct_documentation_menu (help_menu);

    help_menu->addSeparator ();

    m_report_bug_action = add_action (help_menu, QIcon (),
                                      tr ("Report Bug"), SLOT (open_bug_tracker_page ()));

    m_octave_packages_action = add_action (help_menu, QIcon (),
                                           tr ("Octave Packages"), SLOT (open_octave_packages_page ()));

    m_contribute_action = add_action (help_menu, QIcon (),
                                      tr ("Contribute"), SLOT (open_contribute_page ()));

    m_developer_action = add_action (help_menu, QIcon (),
                                     tr ("Donate to Octave"), SLOT (open_donate_page ()));

    help_menu->addSeparator ();

    m_about_octave_action = add_action (help_menu, QIcon (),
                                        tr ("About Octave"), SLOT (show_about_octave ()));
  }

  void main_window::construct_documentation_menu (QMenu *p)
  {
    QMenu *doc_menu = p->addMenu (tr ("Documentation"));

    m_ondisk_doc_action = add_action (doc_menu, QIcon (),
                                      tr ("On Disk"), SLOT (focus ()), m_doc_browser_window);

    m_online_doc_action = add_action (doc_menu, QIcon (),
                                      tr ("Online"), SLOT (open_online_documentation_page ()));
  }

  void main_window::construct_news_menu (QMenuBar *p)
  {
    QMenu *news_menu = m_add_menu (p, tr ("&News"));

    m_release_notes_action = add_action (news_menu, QIcon (),
                                         tr ("Release Notes"), SLOT (display_release_notes ()));

    m_current_news_action = add_action (news_menu, QIcon (),
                                        tr ("Community News"), SLOT (load_and_display_community_news ()));
  }

  void main_window::construct_tool_bar (void)
  {
    m_main_tool_bar = addToolBar (tr ("Toolbar"));

    m_main_tool_bar->setObjectName ("MainToolBar");
    m_main_tool_bar->addAction (m_new_script_action);
    m_main_tool_bar->addAction (m_open_action);

    m_main_tool_bar->addSeparator ();

    m_main_tool_bar->addAction (m_copy_action);
    m_main_tool_bar->addAction (m_paste_action);
    m_main_tool_bar->addAction (m_undo_action);

    m_main_tool_bar->addSeparator ();

    m_current_directory_combo_box = new QComboBox (this);
    QFontMetrics fm = m_current_directory_combo_box->fontMetrics ();
    m_current_directory_combo_box->setFixedWidth (48*fm.averageCharWidth ());
    m_current_directory_combo_box->setEditable (true);
    m_current_directory_combo_box->setInsertPolicy (QComboBox::NoInsert);
    m_current_directory_combo_box->setToolTip (tr ("Enter directory name"));
    m_current_directory_combo_box->setMaxVisibleItems (
                                                       current_directory_max_visible);
    m_current_directory_combo_box->setMaxCount (current_directory_max_count);
    QSizePolicy sizePol (QSizePolicy::Preferred, QSizePolicy::Preferred);
    m_current_directory_combo_box->setSizePolicy (sizePol);

    // addWidget takes ownership of the objects so there is no
    // need to delete these upon destroying this main_window.
    m_main_tool_bar->addWidget (new QLabel (tr ("Current Directory: ")));
    m_main_tool_bar->addWidget (m_current_directory_combo_box);
    QAction *current_dir_up = m_main_tool_bar->addAction (
                                                          resource_manager::icon ("go-up"),
                                                          tr ("One directory up"));
    QAction *current_dir_search = m_main_tool_bar->addAction (
                                                              resource_manager::icon ("folder"),
                                                              tr ("Browse directories"));

    connect (m_current_directory_combo_box, SIGNAL (activated (QString)),
             this, SLOT (set_current_working_directory (QString)));

    connect (m_current_directory_combo_box->lineEdit (), SIGNAL (returnPressed (void)),
             this, SLOT (accept_directory_line_edit (void)));

    connect (current_dir_search, SIGNAL (triggered (void)),
             this, SLOT (browse_for_directory (void)));

    connect (current_dir_up, SIGNAL (triggered (void)),
             this, SLOT (change_directory_up (void)));

    connect (m_undo_action, SIGNAL (triggered (void)),
             this, SLOT (handle_undo_request (void)));
  }

  QString main_window::gui_preference_adjust (const QString& key,
                                              const QString& value)
  {
    QString adjusted_value = value;

    // Immediately return if no new value is given
    if (adjusted_value.isEmpty ())
      return adjusted_value;

    // Not all encodings are available. Encodings are uppercase and do not
    // use CPxxx but IBMxxx or WINDOWS-xxx.
    if (key == "editor/default_encoding")
      {
        adjusted_value = adjusted_value.toUpper ();

        QStringList codecs;
        resource_manager::get_codecs (&codecs);

        QRegExp re ("^CP(\\d+)$");
        if (re.indexIn (adjusted_value) > -1)
          {
            if (codecs.contains ("IBM" + re.cap (1)))
              adjusted_value = "IBM" + re.cap (1);
            else if (codecs.contains ("WINDOWS-" + re.cap (1)))
              adjusted_value = "WINDOWS-" + re.cap (1);
            else
              adjusted_value.clear ();
          }
        else if (! codecs.contains (adjusted_value))
          adjusted_value.clear ();
      }

    return adjusted_value;
  }

  void main_window::gui_preference (const QString& key, const QString& value,
                                    QString* read_value)
  {
    QSettings *settings = resource_manager::get_settings ();
    *read_value = settings->value (key).toString ();

    // Wait for worker to suspend
    m_octave_qt_link->lock ();

    // Some preferences need extra handling
    QString adjusted_value = gui_preference_adjust (key, value);

    if (! adjusted_value.isEmpty () && (*read_value != adjusted_value))
      {
        // Change settings only for new, non-empty values
        settings->setValue (key, QVariant (adjusted_value));
        emit settings_changed (settings);
      }

    // We are done: Unlock and wake the worker thread
    m_octave_qt_link->unlock ();
    m_octave_qt_link->wake_all ();
  }

  void main_window::save_workspace_callback (const std::string& file)
  {
    // INTERPRETER THREAD

    Fsave (ovl (file));
  }

  void main_window::load_workspace_callback (const std::string& file)
  {
    // INTERPRETER THREAD

    Fload (ovl (file));

    symbol_scope scope
      = __get_current_scope__ ("main_window::load_workspace_callback");

    if (scope)
      octave_link::set_workspace (true, scope);
  }

  void main_window::rename_variable_callback (const main_window::name_pair& names)
  {
    // INTERPRETER THREAD

    symbol_scope scope
      = __get_current_scope__ ("main_window::rename_variable_callback");

    if (scope)
      {
        scope.rename (names.first, names.second);

        octave_link::set_workspace (true, scope);
      }

    // FIXME: if this action fails, do we need a way to display that info
    // in the GUI?
  }

  void main_window::command_window_undo_callback (void)
  {
    // INTERPRETER THREAD

    command_editor::undo ();
    command_editor::redisplay ();
  }

  void main_window::clear_command_window_callback (void)
  {
    // INTERPRETER THREAD

    command_editor::kill_full_line ();
    command_editor::clear_screen ();
  }

  void main_window::resize_command_window_callback (void)
  {
    // INTERPRETER THREAD

    command_editor::resize_terminal ();
  }

  void main_window::set_screen_size_callback (const int_pair& sz)
  {
    // INTERPRETER THREAD

    command_editor::set_screen_size (sz.first, sz.second);
  }

  void main_window::open_any_callback (const std::string& file)
  {
    // INTERPRETER THREAD

    octave::feval ("open", ovl (file));

    // Update the workspace since open.m may have loaded new variables.
    symbol_scope scope
      = __get_current_scope__ ("main_window::open_any_callback");

    if (scope)
          octave_link::set_workspace (true, scope);
  }

  void main_window::clear_workspace_callback (void)
  {
    // INTERPRETER THREAD

    interpreter& interp
      = __get_interpreter__ ("main_window::clear_workspace_callback");

    Fclear (interp);
  }

  void main_window::clear_history_callback (void)
  {
    // INTERPRETER THREAD

    Fhistory (ovl ("-c"));
  }

  void main_window::refresh_workspace_callback (void)
  {
    // INTERPRETER THREAD

    symbol_scope scope
      = __get_current_scope__ ("main_window::force_refresh_workspace");

    if (scope)
      octave_link::set_workspace (true, scope, false);
  }

  bool main_window::focus_console_after_command (void)
  {
    QSettings *settings = resource_manager::get_settings ();
    return settings->value ("terminal/focus_after_command",false).toBool ();
  }

  void main_window::new_figure_callback (void)
  {
    // INTERPRETER THREAD

    interpreter& interp
      = __get_interpreter__ ("main_window::new_figure_callback");

    Fbuiltin (interp, ovl ("figure"));
    Fdrawnow ();
  }

  void main_window::change_directory_callback (const std::string& directory)
  {
    // INTERPRETER THREAD

    Fcd (ovl (directory));
  }

  void main_window::configure_shortcuts (void)
  {
    // file menu
    shortcut_manager::set_shortcut (m_open_action, "main_file:open_file");
    shortcut_manager::set_shortcut (m_new_script_action, "main_file:new_file");
    shortcut_manager::set_shortcut (m_new_function_action,
                                    "main_file:new_function");
    shortcut_manager::set_shortcut (m_new_function_action, "main_file:new_figure");
    shortcut_manager::set_shortcut (m_load_workspace_action,
                                    "main_file:load_workspace");
    shortcut_manager::set_shortcut (m_save_workspace_action,
                                    "main_file:save_workspace");
    shortcut_manager::set_shortcut (m_preferences_action, "main_file:preferences");
    shortcut_manager::set_shortcut (m_exit_action,"main_file:exit");

    // edit menu
    shortcut_manager::set_shortcut (m_copy_action, "main_edit:copy");
    shortcut_manager::set_shortcut (m_paste_action, "main_edit:paste");
    shortcut_manager::set_shortcut (m_undo_action, "main_edit:undo");
    shortcut_manager::set_shortcut (m_select_all_action, "main_edit:select_all");
    shortcut_manager::set_shortcut (m_clear_clipboard_action,
                                    "main_edit:clear_clipboard");
    shortcut_manager::set_shortcut (m_find_files_action, "main_edit:find_in_files");
    shortcut_manager::set_shortcut (m_clear_command_history_action,
                                    "main_edit:clear_history");
    shortcut_manager::set_shortcut (m_clear_command_window_action,
                                    "main_edit:clear_command_window");
    shortcut_manager::set_shortcut (m_clear_workspace_action,
                                    "main_edit:clear_workspace");

    // debug menu
    shortcut_manager::set_shortcut (m_debug_step_over, "main_debug:step_over");
    shortcut_manager::set_shortcut (m_debug_step_into, "main_debug:step_into");
    shortcut_manager::set_shortcut (m_debug_step_out,  "main_debug:step_out");
    shortcut_manager::set_shortcut (m_debug_continue,  "main_debug:continue");
    shortcut_manager::set_shortcut (m_debug_quit,  "main_debug:quit");

    // window menu
    shortcut_manager::set_shortcut (m_show_command_window_action,
                                    "main_window:show_command");
    shortcut_manager::set_shortcut (m_show_history_action,
                                    "main_window:show_history");
    shortcut_manager::set_shortcut (m_show_workspace_action,
                                    "main_window:show_workspace");
    shortcut_manager::set_shortcut (m_show_file_browser_action,
                                    "main_window:show_file_browser");
    shortcut_manager::set_shortcut (m_show_editor_action,
                                    "main_window:show_editor");
    shortcut_manager::set_shortcut (m_show_documentation_action,
                                    "main_window:show_doc");
    shortcut_manager::set_shortcut (m_show_variable_editor_action,
                                    "main_window:show_variable_editor");
    shortcut_manager::set_shortcut (m_command_window_action, "main_window:command");
    shortcut_manager::set_shortcut (m_history_action, "main_window:history");
    shortcut_manager::set_shortcut (m_workspace_action,  "main_window:workspace");
    shortcut_manager::set_shortcut (m_file_browser_action,
                                    "main_window:file_browser");
    shortcut_manager::set_shortcut (m_editor_action, "main_window:editor");
    shortcut_manager::set_shortcut (m_documentation_action, "main_window:doc");
    shortcut_manager::set_shortcut (m_variable_editor_action,
                                    "main_window:variable_editor");
    shortcut_manager::set_shortcut (m_reset_windows_action, "main_window:reset");

    // help menu
    shortcut_manager::set_shortcut (m_ondisk_doc_action, "main_help:ondisk_doc");
    shortcut_manager::set_shortcut (m_online_doc_action, "main_help:online_doc");
    shortcut_manager::set_shortcut (m_report_bug_action, "main_help:report_bug");
    shortcut_manager::set_shortcut (m_octave_packages_action, "main_help:packages");
    shortcut_manager::set_shortcut (m_contribute_action, "main_help:contribute");
    shortcut_manager::set_shortcut (m_developer_action, "main_help:developer");
    shortcut_manager::set_shortcut (m_about_octave_action, "main_help:about");

    // news menu
    shortcut_manager::set_shortcut (m_release_notes_action,
                                    "main_news:release_notes");
    shortcut_manager::set_shortcut (m_current_news_action,
                                    "main_news:community_news");
  }

  QList<octave_dock_widget *> main_window::dock_widget_list (void)
  {
    QList<octave_dock_widget *> list = QList<octave_dock_widget *> ();
    list.append (static_cast<octave_dock_widget *> (m_command_window));
    list.append (static_cast<octave_dock_widget *> (m_history_window));
    list.append (static_cast<octave_dock_widget *> (m_file_browser_window));
    list.append (static_cast<octave_dock_widget *> (m_doc_browser_window));
#if defined (HAVE_QSCINTILLA)
    list.append (static_cast<octave_dock_widget *> (m_editor_window));
#endif
    list.append (static_cast<octave_dock_widget *> (m_workspace_window));
    list.append (static_cast<octave_dock_widget *> (m_variable_editor_window));
    return list;
  }

  void news_reader::process (void)
  {
    QString html_text;

    if (m_connect_to_web)
      {
        // Run this part in a separate thread so Octave can continue to
        // run while we wait for the page to load.  Then emit the signal
        // to display it when we have the page contents.

        QString url = m_base_url + '/' + m_page;
        std::ostringstream buf;
        url_transfer octave_dot_org (url.toStdString (), buf);

        if (octave_dot_org.is_valid ())
          {
            Array<std::string> param;
            octave_dot_org.http_get (param);

            if (octave_dot_org.good ())
              html_text = QString::fromStdString (buf.str ());
          }

        if (html_text.contains ("this-is-the-gnu-octave-community-news-page"))
          {
            if (m_serial >= 0)
              {
                QSettings *settings = resource_manager::get_settings ();

                if (settings)
                  {
                    settings->setValue ("news/last_time_checked",
                                        QDateTime::currentDateTime ());

                    settings->sync ();
                  }

                QString tag ("community-news-page-serial=");

                int b = html_text.indexOf (tag);

                if (b)
                  {
                    b += tag.length ();

                    int e = html_text.indexOf ("\n", b);

                    QString tmp = html_text.mid (b, e-b);

                    int curr_page_serial = tmp.toInt ();

                    if (curr_page_serial > m_serial)
                      {
                        if (settings)
                          {
                            settings->setValue ("news/last_news_item",
                                                curr_page_serial);

                            settings->sync ();
                          }
                      }
                    else
                      return;
                  }
                else
                  return;
              }
          }
        else
          html_text = QString
            (tr ("<html>\n"
                 "<body>\n"
                 "<p>\n"
                 "Octave's community news source seems to be unavailable.\n"
                 "</p>\n"
                 "<p>\n"
                 "For the latest news, please check\n"
                 "<a href=\"https://octave.org/community-news.html\">https://octave.org/community-news.html</a>\n"
                 "when you have a connection to the web (link opens in an external browser).\n"
                 "</p>\n"
                 "<p>\n"
                 "<small><em>&mdash; The Octave Developers, ") + OCTAVE_RELEASE_DATE + "</em></small>\n"
             "</p>\n"
             "</body>\n"
             "</html>\n");
      }
    else
      html_text = QString
        (tr ("<html>\n"
             "<body>\n"
             "<p>\n"
             "Connecting to the web to display the latest Octave Community news has been disabled.\n"
             "</p>\n"
             "<p>\n"
             "For the latest news, please check\n"
             "<a href=\"https://octave.org/community-news.html\">https://octave.org/community-news.html</a>\n"
             "when you have a connection to the web (link opens in an external browser)\n"
             "or enable web connections for news in Octave's network settings dialog.\n"
             "</p>\n"
             "<p>\n"
             "<small><em>&mdash; The Octave Developers, ") + OCTAVE_RELEASE_DATE + "</em></small>\n"
         "</p>\n"
         "</body>\n"
         "</html>\n");

    emit display_news_signal (html_text);

    emit finished ();
  }
}
