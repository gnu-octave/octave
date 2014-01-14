/*

Copyright (C) 2013 John W. Eaton
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

#include <QKeySequence>
#include <QApplication>
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
#include <QTextStream>
#include <QThread>
#include <QDateTime>
#include <QDebug>

#include <utility>

#ifdef HAVE_QSCINTILLA
#include "file-editor.h"
#endif
#include "main-window.h"
#include "settings-dialog.h"

#include "Array.h"
#include "cmd-edit.h"
#include "url-transfer.h"

#include "builtin-defun-decls.h"
#include "defaults.h"
#include "symtab.h"
#include "version.h"

static file_editor_interface *
create_default_editor (QWidget *p)
{
#ifdef HAVE_QSCINTILLA
  return new file_editor (p);
#else
  return 0;
#endif
}

main_window::main_window (QWidget *p)
  : QMainWindow (p),
    _workspace_model (new workspace_model ()),
    status_bar (new QStatusBar ()),
    command_window (new terminal_dock_widget (this)),
    history_window (new history_dock_widget (this)),
    file_browser_window (new files_dock_widget (this)),
    doc_browser_window (new documentation_dock_widget (this)),
    editor_window (create_default_editor (this)),
    workspace_window (new workspace_view (this)),
    find_files_dlg (0),
    release_notes_window (0),
    community_news_window (0),
    _octave_qt_link (0),
    _clipboard (QApplication::clipboard ()),
    _cmd_queue (new QStringList ()),  // no command pending
    _cmd_processing (1),
    _cmd_queue_mutex (),
    _dbg_queue (new QStringList ()),  // no debug pending
    _dbg_processing (1),
    _dbg_queue_mutex ()
{
  QSettings *settings = resource_manager::get_settings ();

  bool connect_to_web = true;
  QDateTime last_checked;
  int serial = 0;

  if (settings)
    {
      connect_to_web
        = settings->value ("news/allow_web_connection", true).toBool ();

      last_checked
        = settings->value ("news/last_time_checked", QDateTime ()).toDateTime ();

      serial = settings->value ("news/last_news_item", 0).toInt ();
    }

  QDateTime current = QDateTime::currentDateTime ();
  QDateTime one_day_ago = current.addDays (-1);

  if (connect_to_web
      && (! last_checked.isValid () || one_day_ago > last_checked))
    load_and_display_community_news (serial);

  // We have to set up all our windows, before we finally launch octave.
  construct ();
}

main_window::~main_window (void)
{
  // Destroy the terminal first so that STDERR stream is redirected back
  // to its original pipe to capture error messages at exit.

  delete editor_window;     // first one for dialogs of modified editor-tabs
  delete command_window;
  delete workspace_window;
  delete doc_browser_window;
  delete file_browser_window;
  delete history_window;
  delete status_bar;
  delete _workspace_model;
  if (find_files_dlg)
    {
      delete find_files_dlg;
      find_files_dlg = 0;
    }
  if (release_notes_window)
    {
      delete release_notes_window;
      release_notes_window = 0;
    }
  if (community_news_window)
    {
      delete community_news_window;
      community_news_window = 0;
    }
  delete _octave_qt_link;
  delete _cmd_queue;
}

bool
main_window::command_window_has_focus (void) const
{
  return command_window->has_focus ();
}

void
main_window::focus_command_window (void)
{
  command_window->focus ();
}

void
main_window::new_file (const QString& commands)
{
  emit new_file_signal (commands);
}

void
main_window::open_file (const QString& file_name)
{
  emit open_file_signal (file_name);
}

void
main_window::report_status_message (const QString& statusMessage)
{
  status_bar->showMessage (statusMessage, 1000);
}

void
main_window::handle_save_workspace_request (void)
{
  QString file =
    QFileDialog::getSaveFileName (this, tr ("Save Workspace As"), ".", 0, 0,
                                  QFileDialog::DontUseNativeDialog);

  if (! file.isEmpty ())
    octave_link::post_event (this, &main_window::save_workspace_callback,
                             file.toStdString ());
}

void
main_window::handle_load_workspace_request (const QString& file_arg)
{
  QString file = file_arg;

  if (file.isEmpty ())
    file = QFileDialog::getOpenFileName (this, tr ("Load Workspace"), ".", 0, 0,
                                         QFileDialog::DontUseNativeDialog);

  if (! file.isEmpty ())
    octave_link::post_event (this, &main_window::load_workspace_callback,
                             file.toStdString ());
}

void
main_window::handle_clear_workspace_request (void)
{
  octave_link::post_event (this, &main_window::clear_workspace_callback);
}

void
main_window::handle_rename_variable_request (const QString& old_name,
                                             const QString& new_name)

{
  name_pair names (old_name.toStdString (), new_name.toStdString ());

  octave_link::post_event (this, &main_window::rename_variable_callback,
                           names);
}

void
main_window::handle_undo_request (void)
{
  octave_link::post_event (this, &main_window::command_window_undo_callback);
}

void
main_window::handle_clear_command_window_request (void)
{
  octave_link::post_event (this, &main_window::clear_command_window_callback);
}

void
main_window::handle_clear_history_request (void)
{
  octave_link::post_event (this, &main_window::clear_history_callback);
}

bool
main_window::focus_console_after_command ()
{
  QSettings *settings = resource_manager::get_settings ();
  return settings->value ("terminal/focus_after_command",false).toBool ();
}

void
main_window::execute_command_in_terminal (const QString& command)
{
  queue_command (command);
  if (focus_console_after_command ())
    focus_command_window ();
}

void
main_window::run_file_in_terminal (const QFileInfo& info)
{
  QString file_name = info.canonicalFilePath ();
  QString command = "run \""+file_name+"\"";

  QString function_name = info.fileName ();
  function_name.chop (info.suffix ().length () + 1);

  if (function_name.contains (' '))
    {
      int ans = QMessageBox::question (0, tr ("Octave"),
         tr ("The file %1\n"
             "contains spaces and can not be executed.\n\n"
             "Do you want to execute\n%2\n"
             "instead?").
          arg (file_name).arg (command),
          QMessageBox::Yes | QMessageBox::No, QMessageBox::Yes);

      if (ans == QMessageBox::Yes)
        execute_command_in_terminal (command);

      return;
    }

  octave_link::post_event (this, &main_window::run_file_callback, info);
  if (focus_console_after_command ())
    focus_command_window ();
}

void
main_window::run_file_callback (const QFileInfo& info)
{
  QString dir = info.absolutePath ();
  QString function_name = info.fileName ();
  function_name.chop (info.suffix ().length () + 1);
  if (octave_qt_link::file_in_path (info.absoluteFilePath ().toStdString (),
                                    dir.toStdString ()))
    queue_command (function_name);
}

void
main_window::queue_command (QString command)
{
  _cmd_queue_mutex.lock ();
  _cmd_queue->append (command);   // queue command
  _cmd_queue_mutex.unlock ();

  if (_cmd_processing.tryAcquire ())  // if callback not processing, post event
    octave_link::post_event (this, &main_window::execute_command_callback);
}

void
main_window::handle_new_figure_request (void)
{
  octave_link::post_event (this, &main_window::new_figure_callback);
}

void
main_window::open_online_documentation_page (void)
{
  QDesktopServices::openUrl (QUrl ("http://octave.org/doc/interpreter"));
}

void
main_window::display_release_notes (void)
{
  if (! release_notes_window)
    {
      std::string news_file = Voct_etc_dir + "/NEWS";

      QString news;

      QFile *file = new QFile (QString::fromStdString (news_file));
      if (file->open (QFile::ReadOnly))
        {
          QTextStream *stream = new QTextStream (file);
          news = stream->readAll ();
          if (! news.isEmpty ())
            {
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


      release_notes_window = new QWidget;

      QTextBrowser *browser = new QTextBrowser (release_notes_window);
      browser->setText (news);

      QVBoxLayout *vlayout = new QVBoxLayout;
      vlayout->addWidget (browser);

      release_notes_window->setLayout (vlayout);
      release_notes_window->setWindowTitle (tr ("Octave Release Notes"));

      browser->document()->adjustSize ();
      QSize doc_size = browser->document()->size().toSize ();
      doc_size.rwidth () += 45;
      int h = QApplication::desktop ()->height ();
      if (h > 800)
        h = 800;
      doc_size.rheight () = h;

      release_notes_window->resize (doc_size);
    }

  if (! release_notes_window->isVisible ())
    release_notes_window->show ();
  else if (release_notes_window->isMinimized ())
    release_notes_window->showNormal ();

  release_notes_window->setWindowIcon (QIcon (_release_notes_icon));

  release_notes_window->raise ();
  release_notes_window->activateWindow ();
}

void
news_reader::process (void)
{
  QString html_text;

  if (connect_to_web)
    {
      // Run this part in a separate thread so Octave can continue to
      // run while we wait for the page to load.  Then emit the signal
      // to display it when we have the page contents.

      QString url = base_url + "/" + page;
      std::ostringstream buf;
      url_transfer octave_dot_org (url.toStdString (), buf);

      Array<std::string> param;
      octave_dot_org.http_get (param);

      if (octave_dot_org.good ())
        html_text = QString::fromStdString (buf.str ());

      if (html_text.contains ("this-is-the-gnu-octave-community-news-page"))
        {
          if (serial >= 0)
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

                  if (curr_page_serial > serial)
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
               "<a href=\"http://octave.org/community-news.html\">http://octave.org/community-news.html</a>\n"
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
           "<a href=\"http://octave.org/community-news.html\">http://octave.org/community-news.html</a>\n"
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

void
main_window::load_and_display_community_news (int serial)
{
  QSettings *settings = resource_manager::get_settings ();

  bool connect_to_web
    = (settings
       ? settings->value ("news/allow_web_connection", true).toBool ()
       : true);

  QString base_url = "http://octave.org";
  QString page = "community-news.html";

  QThread *worker_thread = new QThread;

  news_reader *reader = new news_reader (base_url, page, serial,
                                         connect_to_web);

  reader->moveToThread (worker_thread);

  connect (reader, SIGNAL (display_news_signal (const QString&)),
           this, SLOT (display_community_news (const QString&)));

  connect (worker_thread, SIGNAL (started (void)),
           reader, SLOT (process ()));

  connect (reader, SIGNAL (finished (void)), worker_thread, SLOT (quit ()));

  connect (reader, SIGNAL (finished (void)), reader, SLOT (deleteLater ()));

  connect (worker_thread, SIGNAL (finished (void)),
           worker_thread, SLOT (deleteLater ()));

  worker_thread->start ();
}

void
main_window::display_community_news (const QString& news)
{
  if (! community_news_window)
    {
      community_news_window = new QWidget;

      QTextBrowser *browser = new QTextBrowser (community_news_window);

      browser->setHtml (news);
      browser->setObjectName ("OctaveNews");
      browser->setOpenExternalLinks (true);

      QVBoxLayout *vlayout = new QVBoxLayout;

      vlayout->addWidget (browser);

      community_news_window->setLayout (vlayout);
      community_news_window->setWindowTitle (tr ("Octave Community News"));
      community_news_window->resize (640, 480);
      int win_x = QApplication::desktop ()->width ();
      int win_y = QApplication::desktop ()->height ();
      community_news_window->move ((win_x - community_news_window->width ())/2,
                                   (win_y - community_news_window->height ())/2);
    }

  if (! community_news_window->isVisible ())
    community_news_window->show ();
  else if (community_news_window->isMinimized ())
    community_news_window->showNormal ();

  // same icon as release notes
  community_news_window->setWindowIcon (QIcon (_release_notes_icon));

  community_news_window->raise ();
  community_news_window->activateWindow ();
}

void
main_window::open_bug_tracker_page (void)
{
  QDesktopServices::openUrl (QUrl ("http://octave.org/bugs.html"));
}

void
main_window::open_octave_packages_page (void)
{
  QDesktopServices::openUrl (QUrl ("http://octave.org/packages.html"));
}

void
main_window::open_agora_page (void)
{
  QDesktopServices::openUrl (QUrl ("http://agora.octave.org"));
}

void
main_window::open_contribute_page (void)
{
  QDesktopServices::openUrl (QUrl ("http://octave.org/donate.html"));
}

void
main_window::open_developer_page (void)
{
  QDesktopServices::openUrl (QUrl ("http://octave.org/get-involved.html"));
}

void
main_window::process_settings_dialog_request (const QString& desired_tab)
{
  settings_dialog *settingsDialog = new settings_dialog (this, desired_tab);
  int change_settings = settingsDialog->exec ();
  if (change_settings == QDialog::Accepted)
    {
      settingsDialog->write_changed_settings ();
      QSettings *settings = resource_manager::get_settings ();
      if (settings)
        emit settings_changed (settings);
    }
  delete settingsDialog;
}


void
main_window::notice_settings (const QSettings *settings)
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

  while (!widget_icon_data[count].name.isEmpty ())
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
        { // if children has a name
          icon = widget_icon_data[icon_set_found].path; // prefix or octave-logo
          if (widget_icon_data[icon_set_found].name != "NONE")
            icon = icon + name + ".png"; // add widget name and ext.
          widget->setWindowIcon (QIcon (icon));
        }
    }
  if (widget_icon_data[icon_set_found].name != "NONE")
    _release_notes_icon = widget_icon_data[icon_set_found].path
                          + "ReleaseWidget.png";
  else
    _release_notes_icon = ":/actions/icons/logo.png";

  int icon_size = settings->value ("toolbar_icon_size",16).toInt ();
  _main_tool_bar->setIconSize (QSize (icon_size,icon_size));

  resource_manager::update_network_settings ();
}


void
main_window::prepare_to_exit (void)
{
  write_settings ();
}

void
main_window::exit (int status)
{
  qApp->exit (status);
}

void
main_window::reset_windows (void)
{
  QSettings *settings = resource_manager::get_default_settings ();

  set_window_layout (settings);
  showNormal ();  // make sure main window is not minimized
}

void
main_window::change_directory (const QString& dir)
{
  // Remove existing entry, if any, then add new directory at top and
  // mark it as the current directory.  Finally, update the file list
  // widget.

  int index = _current_directory_combo_box->findText (dir);

  if (index >= 0)
    _current_directory_combo_box->removeItem (index);

  _current_directory_combo_box->insertItem (0, dir);
  _current_directory_combo_box->setCurrentIndex (0);

  file_browser_window->update_octave_directory (dir);
}

void
main_window::browse_for_directory (void)
{
  QString dir
    = QFileDialog::getExistingDirectory (this, tr ("Browse directories"), 0,
                                         QFileDialog::DontUseNativeDialog);

  set_current_working_directory (dir);

  // FIXME: on Windows systems, the command window freezes after the
  // previous actions.  Forcing the focus appears to unstick it.

  focus_command_window ();
}

void
main_window::set_current_working_directory (const QString& dir)
{
  // Change to dir if it is an existing directory.

  QString xdir = dir.isEmpty () ? "." : dir;

  QFileInfo fileInfo (xdir);

  if (fileInfo.exists () && fileInfo.isDir ())
    octave_link::post_event (this, &main_window::change_directory_callback,
                             xdir.toStdString ());
}

void
main_window::change_directory_up (void)
{
  set_current_working_directory ("..");
}

// Slot that is called if return is pressed in the line edit of the
// combobox to change to a new directory or a directory that is already
// in the drop down list.

void
main_window::accept_directory_line_edit (void)
{
  // Get new directory name, and change to it if it is new.  Otherwise,
  // the combo box will triggers the "activated" signal to change to the
  // directory.

  QString dir = _current_directory_combo_box->currentText ();

  int index = _current_directory_combo_box->findText (dir);

  if (index < 0)
    set_current_working_directory (dir);
}

void
main_window::handle_enter_debugger (void)
{
  setWindowTitle ("Octave (Debugging)");

  _debug_continue->setEnabled (true);
  _debug_step_into->setEnabled (true);
  _debug_step_over->setEnabled (true);
  _debug_step_out->setEnabled (true);
  _debug_quit->setEnabled (true);

#ifdef HAVE_QSCINTILLA
  editor_window->handle_enter_debug_mode ();
#endif
}

void
main_window::handle_exit_debugger (void)
{
  setWindowTitle ("Octave");

  _debug_continue->setEnabled (false);
  _debug_step_into->setEnabled (false);
  _debug_step_over->setEnabled (false);
  _debug_step_out->setEnabled (false);
  _debug_quit->setEnabled (false);

#ifdef HAVE_QSCINTILLA
  editor_window->handle_exit_debug_mode ();
#endif
}

void
main_window::debug_continue (void)
{
  queue_debug ("cont");
}

void
main_window::debug_step_into (void)
{
  queue_debug ("in");
}

void
main_window::debug_step_over (void)
{
  queue_debug ("step");
}

void
main_window::debug_step_out (void)
{
  queue_debug ("out");
}

void
main_window::debug_quit (void)
{
  queue_debug ("quit");
}

void
main_window::handle_insert_debugger_pointer_request (const QString& file,
                                                     int line)
{
  bool cmd_focus = command_window_has_focus ();

  emit insert_debugger_pointer_signal (file, line);

  if (cmd_focus)
    focus_command_window ();
}

void
main_window::handle_delete_debugger_pointer_request (const QString& file,
                                                     int line)
{
  bool cmd_focus = command_window_has_focus ();

  emit delete_debugger_pointer_signal (file, line);

  if (cmd_focus)
    focus_command_window ();
}

void
main_window::handle_update_breakpoint_marker_request (bool insert,
                                                      const QString& file,
                                                      int line)
{
  bool cmd_focus = command_window_has_focus ();

  emit update_breakpoint_marker_signal (insert, file, line);

  if (cmd_focus)
    focus_command_window ();
}

void
main_window::show_about_octave (void)
{
  std::string message
    = octave_name_version_copyright_copying_warranty_and_bugs (true);

  QMessageBox::about (this, tr ("About Octave"),
                      QString::fromStdString (message));
}

void
main_window::closeEvent (QCloseEvent *e)
{
  e->ignore ();
  octave_link::post_event (this, &main_window::exit_callback);
}

void
main_window::read_settings (void)
{
  QSettings *settings = resource_manager::get_settings ();

  if (!settings)
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
      _current_directory_combo_box->addItem (curr_dirs.at (i));
    }
  emit settings_changed (settings);
}

void
main_window::set_window_layout (QSettings *settings)
{
  QList<octave_dock_widget *> float_and_visible;

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

#if defined (Q_OS_WIN32)
          // If floating, make window from widget.
          if (floating)
            widget->make_window ();
          else if (! widget->parent ())  // should not be floating but is
            widget->make_widget (false); // no docking, just reparent
#else
          // restore geometry
          QVariant val = settings->value ("DockWidgets/" + name);
          widget->restoreGeometry (val.toByteArray ());
#endif
          // make widget visible if desired
          if (floating && visible)              // floating and visible
            float_and_visible.append (widget);  // not show before main win
          else
            {
              widget->make_widget ();
              widget->setVisible (visible);       // not floating -> show
            }
        }
    }

#if ! defined (Q_OS_WIN32)
  // show main first but minimized to avoid flickering,
  // otherwise the name of a floating widget is shown in a global menu bar
  showMinimized ();
  // hide again, otherwise the geometry is not exactly restored
  hide ();
#endif
  // restore geomoetry of main window
  restoreState (settings->value ("MainWindow/windowState").toByteArray ());
  restoreGeometry (settings->value ("MainWindow/geometry").toByteArray ());
  // show main window
  show ();

  // show floating widgets after main win to ensure "Octave" in central menu
  foreach (octave_dock_widget *widget, float_and_visible)
    {
#if ! defined (Q_OS_WIN32)
      widget->make_window ();
#endif
      if (settings->value ("DockWidgets/" + widget->objectName () + "_minimized").toBool ())
        widget->showMinimized ();
      else
        widget->setVisible (true);
    }

}

void
main_window::write_settings (void)
{
  QSettings *settings = resource_manager::get_settings ();
  if (!settings)
    {
      qDebug ("Error: QSettings pointer from resource manager is NULL.");
      return;
    }

  settings->setValue ("MainWindow/geometry", saveGeometry ());
  settings->setValue ("MainWindow/windowState", saveState ());
  // write the list of recent used directories
  QStringList curr_dirs;
  for (int i=0; i<_current_directory_combo_box->count (); i++)
    {
      curr_dirs.append (_current_directory_combo_box->itemText (i));
    }
  settings->setValue ("MainWindow/current_directory_list", curr_dirs);
  settings->sync ();
}


// Connecting the signals emitted when the visibility of a widget changes.
// This has to be done after the window is shown (see octave-gui.cc)
void
main_window::connect_visibility_changed (void)
{
  foreach (octave_dock_widget *widget, dock_widget_list ())
    widget->connect_visibility_changed ();
}

void
main_window::copyClipboard (void)
{
  if (_current_directory_combo_box->hasFocus ())
    {
      QLineEdit * edit = _current_directory_combo_box->lineEdit ();
      if (edit && edit->hasSelectedText ())
        {
          QClipboard *clipboard = QApplication::clipboard ();
          clipboard->setText (edit->selectedText ());
        }
    }
  else
    emit copyClipboard_signal ();
}

void
main_window::pasteClipboard (void)
{
  if (_current_directory_combo_box->hasFocus ())
    {
      QLineEdit * edit = _current_directory_combo_box->lineEdit ();
      QClipboard *clipboard = QApplication::clipboard ();
      QString str =  clipboard->text ();
      if (edit && str.length () > 0)
        {
          edit->insert (str);
        }
    }
  else
    emit pasteClipboard_signal ();
}

// Connect the signals emitted when the Octave thread wants to create
// a dialog box of some sort.  Perhaps a better place for this would be
// as part of the QUIWidgetCreator class.  However, mainWindow currently
// is not a global variable and not accessible for connecting.

void
main_window::connect_uiwidget_links ()
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

void
main_window::handle_create_dialog (const QString& message,
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

void
main_window::handle_create_listview (const QStringList& list,
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
void
main_window::handle_create_inputlayout (const QStringList& prompt,
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

void
main_window::handle_create_filedialog (const QStringList& filters,
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

// Main subroutine of the constructor
void
main_window::construct (void)
{
  _closing = false;   // flag for editor files when closed
  setWindowIcon (QIcon (":/actions/icons/logo.png"));

  workspace_window->setModel (_workspace_model);
  connect (_workspace_model, SIGNAL (model_changed (void)),
           workspace_window, SLOT (handle_model_changed (void)));

  // Create and set the central widget.  QMainWindow takes ownership of
  // the widget (pointer) so there is no need to delete the object upon
  // destroying this main_window.

  QWidget *dummyWidget = new QWidget ();
  dummyWidget->setObjectName ("CentralDummyWidget");
  dummyWidget->resize (10, 10);
  dummyWidget->setSizePolicy (QSizePolicy::Minimum, QSizePolicy::Minimum);
  dummyWidget->hide ();
  setCentralWidget (dummyWidget);

  construct_menu_bar ();

  construct_tool_bar ();

  construct_warning_bar ();

  connect (qApp, SIGNAL (aboutToQuit ()),
           this, SLOT (prepare_to_exit ()));

  connect (this, SIGNAL (settings_changed (const QSettings *)),
           this, SLOT (notice_settings (const QSettings *)));

  connect (file_browser_window, SIGNAL (load_file_signal (const QString&)),
           this, SLOT (handle_load_workspace_request (const QString&)));

  connect (file_browser_window, SIGNAL (find_files_signal (const QString&)),
           this, SLOT (find_files (const QString&)));

  connect (this, SIGNAL (set_widget_shortcuts_signal (bool)),
           editor_window, SLOT (set_shortcuts (bool)));

  connect_uiwidget_links ();

  setWindowTitle ("Octave");

  setDockOptions (QMainWindow::AnimatedDocks
                  | QMainWindow::AllowNestedDocks
                  | QMainWindow::AllowTabbedDocks);

  addDockWidget (Qt::RightDockWidgetArea, command_window);
  addDockWidget (Qt::RightDockWidgetArea, doc_browser_window);
  tabifyDockWidget (command_window, doc_browser_window);

#ifdef HAVE_QSCINTILLA
  addDockWidget (Qt::RightDockWidgetArea, editor_window);
  tabifyDockWidget (command_window, editor_window);
#endif

  addDockWidget (Qt::LeftDockWidgetArea, file_browser_window);
  addDockWidget (Qt::LeftDockWidgetArea, workspace_window);
  addDockWidget (Qt::LeftDockWidgetArea, history_window);

  int win_x = QApplication::desktop ()->width ();
  int win_y = QApplication::desktop ()->height ();

  if (win_x > 960)
    win_x = 960;

  if (win_y > 720)
    win_y = 720;

  setGeometry (0, 0, win_x, win_y);

  setStatusBar (status_bar);

  construct_octave_qt_link ();

#ifdef HAVE_QSCINTILLA
  connect (this,
           SIGNAL (insert_debugger_pointer_signal (const QString&, int)),
           editor_window,
           SLOT (handle_insert_debugger_pointer_request (const QString&, int)));

  connect (this,
           SIGNAL (delete_debugger_pointer_signal (const QString&, int)),
           editor_window,
           SLOT (handle_delete_debugger_pointer_request (const QString&, int)));

  connect (this,
           SIGNAL (update_breakpoint_marker_signal (bool, const QString&, int)),
           editor_window,
           SLOT (handle_update_breakpoint_marker_request (bool,
                                                          const QString&,
                                                          int)));
#endif

  QDir curr_dir;
  set_current_working_directory (curr_dir.absolutePath ());

  octave_link::post_event (this, &main_window::resize_command_window_callback);

  set_global_shortcuts (true);

}

void
main_window::construct_octave_qt_link (void)
{
  _octave_qt_link = new octave_qt_link ();

  connect (_octave_qt_link, SIGNAL (exit_signal (int)),
           this, SLOT (exit (int)));

  connect (_octave_qt_link,
           SIGNAL (set_workspace_signal
                   (bool, const QString&, const QStringList&,
                    const QStringList&, const QStringList&,
                    const QStringList&, const QIntList&)),
           _workspace_model,
           SLOT (set_workspace
                 (bool, const QString&, const QStringList&,
                  const QStringList&, const QStringList&,
                  const QStringList&, const QIntList&)));

  connect (_octave_qt_link, SIGNAL (clear_workspace_signal ()),
           _workspace_model, SLOT (clear_workspace ()));

  connect (_octave_qt_link, SIGNAL (change_directory_signal (QString)),
           this, SLOT (change_directory (QString)));

  connect (_octave_qt_link,
           SIGNAL (execute_command_in_terminal_signal (QString)),
           this, SLOT (execute_command_in_terminal (QString)));

  connect (_octave_qt_link,
           SIGNAL (set_history_signal (const QStringList&)),
           history_window, SLOT (set_history (const QStringList&)));

  connect (_octave_qt_link,
           SIGNAL (append_history_signal (const QString&)),
           history_window, SLOT (append_history (const QString&)));

  connect (_octave_qt_link,
           SIGNAL (clear_history_signal (void)),
           history_window, SLOT (clear_history (void)));

  connect (_octave_qt_link, SIGNAL (enter_debugger_signal ()),
           this, SLOT (handle_enter_debugger ()));

  connect (_octave_qt_link, SIGNAL (exit_debugger_signal ()),
           this, SLOT (handle_exit_debugger ()));

  connect (_octave_qt_link,
           SIGNAL (show_preferences_signal (void)),
           this, SLOT (process_settings_dialog_request ()));

#ifdef HAVE_QSCINTILLA
  connect (_octave_qt_link,
           SIGNAL (edit_file_signal (const QString&)),
           editor_window,
           SLOT (handle_edit_file_request (const QString&)));
#endif

  connect (_octave_qt_link,
           SIGNAL (insert_debugger_pointer_signal (const QString&, int)),
           this,
           SLOT (handle_insert_debugger_pointer_request (const QString&, int)));

  connect (_octave_qt_link,
           SIGNAL (delete_debugger_pointer_signal (const QString&, int)),
           this,
           SLOT (handle_delete_debugger_pointer_request (const QString&, int)));

  connect (_octave_qt_link,
           SIGNAL (update_breakpoint_marker_signal (bool, const QString&, int)),
           this,
           SLOT (handle_update_breakpoint_marker_request (bool, const QString&,
                                                          int)));

  connect (_octave_qt_link,
           SIGNAL (show_doc_signal (const QString &)),
           this, SLOT (handle_show_doc (const QString &)));

  connect (_workspace_model,
           SIGNAL (rename_variable (const QString&, const QString&)),
           this,
           SLOT (handle_rename_variable_request (const QString&,
                                                 const QString&)));

  connect (command_window, SIGNAL (interrupt_signal (void)),
           _octave_qt_link, SLOT (terminal_interrupt (void)));

  _octave_qt_link->execute_interpreter ();

  octave_link::connect_link (_octave_qt_link);
}

void
main_window::construct_menu_bar (void)
{
  QMenuBar *menu_bar = menuBar ();

  construct_file_menu (menu_bar);

  construct_edit_menu (menu_bar);

  construct_debug_menu (menu_bar);

  construct_window_menu (menu_bar);

  construct_help_menu (menu_bar);

  construct_news_menu (menu_bar);
}

void
main_window::construct_file_menu (QMenuBar *p)
{
  QMenu *file_menu = p->addMenu (tr ("&File"));

  construct_new_menu (file_menu);

  _open_action
    = file_menu->addAction (QIcon (":/actions/icons/folder_documents.png"),
                            tr ("Open..."));
  _open_action->setShortcutContext (Qt::ApplicationShortcut);


#ifdef HAVE_QSCINTILLA
  file_menu->addMenu (editor_window->get_mru_menu ());
#endif

  file_menu->addSeparator ();

  QAction *load_workspace_action
    = file_menu->addAction (tr ("Load Workspace..."));

  QAction *save_workspace_action
    = file_menu->addAction (tr ("Save Workspace As..."));

  file_menu->addSeparator ();

  QAction *preferences_action
    = file_menu->addAction (QIcon (":/actions/icons/configure.png"),
                            tr ("Preferences..."));

  file_menu->addSeparator ();

  _exit_action = file_menu->addAction (tr ("Exit"));
  _exit_action->setShortcutContext (Qt::ApplicationShortcut);

  connect (preferences_action, SIGNAL (triggered ()),
           this, SLOT (process_settings_dialog_request ()));

#ifdef HAVE_QSCINTILLA
  connect (_open_action, SIGNAL (triggered ()),
           editor_window, SLOT (request_open_file ()));
#endif

  connect (load_workspace_action, SIGNAL (triggered ()),
           this, SLOT (handle_load_workspace_request ()));

  connect (save_workspace_action, SIGNAL (triggered ()),
           this, SLOT (handle_save_workspace_request ()));

  connect (_exit_action, SIGNAL (triggered ()),
           this, SLOT (close ()));
}

void
main_window::construct_new_menu (QMenu *p)
{
  QMenu *new_menu = p->addMenu (tr ("New"));

  _new_script_action
    = new_menu->addAction (QIcon (":/actions/icons/filenew.png"),
                           tr ("Script"));
  _new_script_action->setShortcutContext (Qt::ApplicationShortcut);

  _new_function_action = new_menu->addAction (tr ("Function..."));
  _new_function_action->setEnabled (true);
  _new_function_action->setShortcutContext (Qt::ApplicationShortcut);

  QAction *new_figure_action = new_menu->addAction (tr ("Figure"));
  new_figure_action->setEnabled (true);

#ifdef HAVE_QSCINTILLA
  connect (_new_script_action, SIGNAL (triggered ()),
           editor_window, SLOT (request_new_script ()));

  connect (_new_function_action, SIGNAL (triggered ()),
           editor_window, SLOT (request_new_function ()));
#endif

  connect (new_figure_action, SIGNAL (triggered ()),
           this, SLOT (handle_new_figure_request ()));
}

void
main_window::construct_edit_menu (QMenuBar *p)
{
  QMenu *edit_menu = p->addMenu (tr ("&Edit"));

  QKeySequence ctrl_shift = Qt::ControlModifier + Qt::ShiftModifier;

  _undo_action
    = edit_menu->addAction (QIcon (":/actions/icons/undo.png"), tr ("Undo"));
  _undo_action->setShortcut (QKeySequence::Undo);

  edit_menu->addSeparator ();

  _copy_action
    = edit_menu->addAction (QIcon (":/actions/icons/editcopy.png"),
                            tr ("Copy"), this, SLOT (copyClipboard ()));
  _copy_action->setShortcut (QKeySequence::Copy);


  _paste_action
    = edit_menu->addAction (QIcon (":/actions/icons/editpaste.png"),
                            tr ("Paste"), this, SLOT (pasteClipboard ()));
  _paste_action->setShortcut (QKeySequence::Paste);

  _clear_clipboard_action
    = edit_menu->addAction (tr ("Clear Clipboard"), this,
                            SLOT (clear_clipboard ()));

  edit_menu->addSeparator ();

  _find_files_action = edit_menu->addAction (tr ("Find Files..."));

  edit_menu->addSeparator ();

  QAction *clear_command_window_action
    = edit_menu->addAction (tr ("Clear Command Window"));

  QAction *clear_command_history
    = edit_menu->addAction (tr ("Clear Command History"));

  QAction *clear_workspace_action
    = edit_menu->addAction (tr ("Clear Workspace"));

  connect (_find_files_action, SIGNAL (triggered ()),
           this, SLOT (find_files ()));

  connect (clear_command_window_action, SIGNAL (triggered ()),
           this, SLOT (handle_clear_command_window_request ()));

  connect (clear_command_history, SIGNAL (triggered ()),
           this, SLOT (handle_clear_history_request ()));

  connect (clear_workspace_action, SIGNAL (triggered ()),
           this, SLOT (handle_clear_workspace_request ()));

  connect (_clipboard, SIGNAL (changed (QClipboard::Mode)),
           this, SLOT (clipboard_has_changed (QClipboard::Mode)));
  clipboard_has_changed (QClipboard::Clipboard);
}

QAction *
main_window::construct_debug_menu_item (const char *icon_file,
                                        const QString& item,
                                        const QKeySequence& key)
{
  QAction *action = _debug_menu->addAction (QIcon (icon_file), item);

  action->setEnabled (false);
  action->setShortcut (key);

#ifdef HAVE_QSCINTILLA
  editor_window->debug_menu ()->addAction (action);
  editor_window->toolbar ()->addAction (action);
#endif

  return action;
}

void
main_window::construct_debug_menu (QMenuBar *p)
{
  _debug_menu = p->addMenu (tr ("De&bug"));

  _debug_step_over = construct_debug_menu_item
                       (":/actions/icons/db_step.png", tr ("Step"),
                        Qt::Key_F10);

  _debug_step_into = construct_debug_menu_item
                       (":/actions/icons/db_step_in.png", tr ("Step In"),
                        Qt::Key_F11);

  _debug_step_out = construct_debug_menu_item
                      (":/actions/icons/db_step_out.png", tr ("Step Out"),
                       Qt::ShiftModifier + Qt::Key_F11);

  _debug_continue = construct_debug_menu_item
                      (":/actions/icons/db_cont.png", tr ("Continue"),
                       Qt::Key_F5);

  _debug_menu->addSeparator ();
#ifdef HAVE_QSCINTILLA
  editor_window->debug_menu ()->addSeparator ();
#endif

  _debug_quit = construct_debug_menu_item
                (":/actions/icons/db_stop.png", tr ("Exit Debug Mode"),
                 Qt::ShiftModifier + Qt::Key_F5);

  connect (_debug_step_over, SIGNAL (triggered ()),
           this, SLOT (debug_step_over ()));

  connect (_debug_step_into, SIGNAL (triggered ()),
           this, SLOT (debug_step_into ()));

  connect (_debug_step_out, SIGNAL (triggered ()),
           this, SLOT (debug_step_out ()));

  connect (_debug_continue, SIGNAL (triggered ()),
           this, SLOT (debug_continue ()));

  connect (_debug_quit, SIGNAL (triggered ()),
           this, SLOT (debug_quit ()));
}

QAction *
main_window::construct_window_menu_item (QMenu *p, const QString& item,
                                         bool checkable,
                                         const QKeySequence& key)
{
  QAction *action = p->addAction (item);

  action->setCheckable (checkable);
  action->setShortcut (key);
  action->setShortcutContext (Qt::ApplicationShortcut);

  return action;
}

void
main_window::construct_window_menu (QMenuBar *p)
{
  QMenu *window_menu = p->addMenu (tr ("&Window"));

  QKeySequence ctrl = Qt::ControlModifier;
  QKeySequence ctrl_shift = Qt::ControlModifier + Qt::ShiftModifier;

  QAction *show_command_window_action = construct_window_menu_item
                                        (window_menu,
                                         tr ("Show Command Window"), true,
                                         ctrl_shift + Qt::Key_0);

  QAction *show_history_action = construct_window_menu_item
                                 (window_menu, tr ("Show Command History"),
                                  true, ctrl_shift + Qt::Key_1);

  QAction *show_file_browser_action =  construct_window_menu_item
                                       (window_menu, tr ("Show File Browser"),
                                        true, ctrl_shift + Qt::Key_2);

  QAction *show_workspace_action = construct_window_menu_item
                                   (window_menu, tr ("Show Workspace"), true,
                                    ctrl_shift + Qt::Key_3);

  QAction *show_editor_action = construct_window_menu_item
                                (window_menu, tr ("Show Editor"), true,
                                 ctrl_shift + Qt::Key_4);

  QAction *show_documentation_action = construct_window_menu_item
                                       (window_menu, tr ("Show Documentation"),
                                        true, ctrl_shift + Qt::Key_5);

  window_menu->addSeparator ();

  QAction *command_window_action = construct_window_menu_item
                                   (window_menu, tr ("Command Window"), false,
                                    ctrl + Qt::Key_0);

  QAction *history_action = construct_window_menu_item
                            (window_menu, tr ("Command History"), false,
                             ctrl + Qt::Key_1);

  QAction *file_browser_action = construct_window_menu_item
                                 (window_menu, tr ("File Browser"), false,
                                  ctrl + Qt::Key_2);

  QAction *workspace_action = construct_window_menu_item
                              (window_menu, tr ("Workspace"), false,
                               ctrl + Qt::Key_3);

  QAction *editor_action = construct_window_menu_item
                           (window_menu, tr ("Editor"), false,
                            ctrl + Qt::Key_4);

  QAction *documentation_action = construct_window_menu_item
                                  (window_menu, tr ("Documentation"), false,
                                   ctrl + Qt::Key_5);

  window_menu->addSeparator ();

  QAction *reset_windows_action
    = window_menu->addAction (tr ("Reset Default Window Layout"));

  connect (show_command_window_action, SIGNAL (toggled (bool)),
           command_window, SLOT (setVisible (bool)));

  connect (command_window, SIGNAL (active_changed (bool)),
           show_command_window_action, SLOT (setChecked (bool)));

  connect (show_workspace_action, SIGNAL (toggled (bool)),
           workspace_window, SLOT (setVisible (bool)));

  connect (workspace_window, SIGNAL (active_changed (bool)),
           show_workspace_action, SLOT (setChecked (bool)));

  connect (show_history_action, SIGNAL (toggled (bool)),
           history_window, SLOT (setVisible (bool)));

  connect (history_window, SIGNAL (active_changed (bool)),
           show_history_action, SLOT (setChecked (bool)));

  connect (show_file_browser_action, SIGNAL (toggled (bool)),
           file_browser_window, SLOT (setVisible (bool)));

  connect (file_browser_window, SIGNAL (active_changed (bool)),
           show_file_browser_action, SLOT (setChecked (bool)));

#ifdef HAVE_QSCINTILLA
  connect (show_editor_action, SIGNAL (toggled (bool)),
           editor_window, SLOT (setVisible (bool)));

  connect (editor_window, SIGNAL (active_changed (bool)),
           show_editor_action, SLOT (setChecked (bool)));
#endif

  connect (show_documentation_action, SIGNAL (toggled (bool)),
           doc_browser_window, SLOT (setVisible (bool)));

  connect (doc_browser_window, SIGNAL (active_changed (bool)),
           show_documentation_action, SLOT (setChecked (bool)));

  connect (command_window_action, SIGNAL (triggered ()),
           command_window, SLOT (focus ()));

  connect (workspace_action, SIGNAL (triggered ()),
           workspace_window, SLOT (focus ()));

  connect (history_action, SIGNAL (triggered ()),
           history_window, SLOT (focus ()));

  connect (file_browser_action, SIGNAL (triggered ()),
           file_browser_window, SLOT (focus ()));

#ifdef HAVE_QSCINTILLA
  connect (editor_action, SIGNAL (triggered ()),
           editor_window, SLOT (focus ()));
#endif

  connect (documentation_action, SIGNAL (triggered ()),
           doc_browser_window, SLOT (focus ()));

  connect (reset_windows_action, SIGNAL (triggered ()),
           this, SLOT (reset_windows ()));
}

void
main_window::construct_help_menu (QMenuBar *p)
{
  QMenu *help_menu = p->addMenu (tr ("&Help"));

  construct_documentation_menu (help_menu);

  help_menu->addSeparator ();

  QAction *report_bug_action
    = help_menu->addAction (tr ("Report Bug"));

  QAction *octave_packages_action
    = help_menu->addAction (tr ("Octave Packages"));

  QAction *agora_action
    = help_menu->addAction (tr ("Share Code"));

  QAction *contribute_action
    = help_menu->addAction (tr ("Contribute to Octave"));

  QAction *developer_action
    = help_menu->addAction (tr ("Octave Developer Resources"));

  help_menu->addSeparator ();

  QAction *about_octave_action
    = help_menu->addAction (tr ("About Octave"));

  connect (report_bug_action, SIGNAL (triggered ()),
           this, SLOT (open_bug_tracker_page ()));

  connect (octave_packages_action, SIGNAL (triggered ()),
           this, SLOT (open_octave_packages_page ()));

  connect (agora_action, SIGNAL (triggered ()),
           this, SLOT (open_agora_page ()));

  connect (contribute_action, SIGNAL (triggered ()),
           this, SLOT (open_contribute_page ()));

  connect (developer_action, SIGNAL (triggered ()),
           this, SLOT (open_developer_page ()));

  connect (about_octave_action, SIGNAL (triggered ()),
           this, SLOT (show_about_octave ()));
}

void
main_window::construct_documentation_menu (QMenu *p)
{
  QMenu *documentation_menu = p->addMenu (tr ("Documentation"));

  QAction *ondisk_documentation_action
    = documentation_menu->addAction (tr ("On Disk"));

  QAction *online_documentation_action
    = documentation_menu->addAction (tr ("Online"));

  connect (ondisk_documentation_action, SIGNAL (triggered ()),
           doc_browser_window, SLOT (focus ()));

  connect (online_documentation_action, SIGNAL (triggered ()),
           this, SLOT (open_online_documentation_page ()));
}

void
main_window::construct_news_menu (QMenuBar *p)
{
  QMenu *news_menu = p->addMenu (tr ("&News"));

  QAction *release_notes_action
    = news_menu->addAction (tr ("Release Notes"));

  QAction *current_news_action
    = news_menu->addAction (tr ("Community News"));

  connect (release_notes_action, SIGNAL (triggered ()),
           this, SLOT (display_release_notes ()));

  connect (current_news_action, SIGNAL (triggered ()),
           this, SLOT (load_and_display_community_news ()));
}

void
main_window::construct_warning_bar (void)
{
  QSettings *settings = resource_manager::get_settings ();

  if (settings
      && settings->value ("General/hide_new_gui_warning", false).toBool ())
    {
      construct_gui_info_button ();

      return;
    }

  _warning_bar = new QDockWidget (this);
  _warning_bar->setAttribute (Qt::WA_DeleteOnClose);

  QFrame *box = new QFrame (_warning_bar);

  QLabel *icon = new QLabel (box);
  QIcon warning_icon
    = QIcon::fromTheme ("dialog-warning",
                        QIcon (":/actions/icons/warning.png"));
  QPixmap icon_pixmap = warning_icon.pixmap (QSize (32, 32));
  icon->setPixmap (icon_pixmap);

  QTextBrowser *msg = new QTextBrowser (box);
  msg->setOpenExternalLinks (true);
  msg->setText
    (tr ("<strong>You are using a release candidate of Octave's experimental GUI.</strong>  "
         "Octave is under continuous improvement and the GUI will be the "
         "default interface for the 4.0 release.  For more information, "
         "select the \"Release Notes\" item in the \"News\" menu of the GUI, "
         "or visit <a href=\"http://octave.org\">http://octave.org</a>."));

  msg->setStyleSheet ("background-color: #ffd97f; color: black; margin 4px;");
  msg->setMinimumWidth (100);
  msg->setMinimumHeight (60);
  msg->setMaximumHeight (80);
  msg->setSizePolicy (QSizePolicy (QSizePolicy::Expanding,
                                   QSizePolicy::Minimum));

  QPushButton *info_button = new QPushButton (tr ("More Info"), box);
  QPushButton *hide_button = new QPushButton (tr ("Hide"), box);

  connect (info_button, SIGNAL (clicked ()),
           this, SLOT (show_gui_info ()));

  connect (hide_button, SIGNAL (clicked ()),
           this, SLOT (hide_warning_bar ()));

  QVBoxLayout *button_layout = new QVBoxLayout;

  button_layout->addWidget (info_button);
  button_layout->addWidget (hide_button);

  QHBoxLayout *icon_and_message = new QHBoxLayout;

  icon_and_message->addWidget (icon);
  icon_and_message->addSpacing (10);
  icon_and_message->addWidget (msg);
  icon_and_message->addSpacing (10);
  icon_and_message->addLayout (button_layout);

  icon_and_message->setAlignment (hide_button, Qt::AlignTop);

  box->setFrameStyle (QFrame::Box);
  box->setLineWidth (2);
  box->setMaximumWidth (1000);
  box->adjustSize ();
  box->setLayout (icon_and_message);

  _warning_bar->setFeatures (QDockWidget::NoDockWidgetFeatures);
  _warning_bar->setObjectName ("WarningToolBar");
  _warning_bar->setWidget (box);

  setCorner (Qt::TopLeftCorner, Qt::TopDockWidgetArea);
  setCorner (Qt::TopRightCorner, Qt::TopDockWidgetArea);

  addDockWidget (Qt::TopDockWidgetArea, _warning_bar);
};

void
main_window::construct_gui_info_button (void)
{
  QIcon warning_icon
    = QIcon::fromTheme ("dialog-warning",
                        QIcon (":/actions/icons/warning.png"));

  _gui_info_button
    = new QPushButton (warning_icon, tr ("Experimental GUI Info"));

  _main_tool_bar->addWidget (_gui_info_button);

  connect (_gui_info_button, SIGNAL (clicked ()),
           this, SLOT (show_gui_info ()));
}

void
main_window::hide_warning_bar (void)
{
  QSettings *settings = resource_manager::get_settings ();

  if (settings)
    {
      settings->setValue ("General/hide_new_gui_warning", true);

      settings->sync ();
    }

  removeDockWidget (_warning_bar);

  construct_gui_info_button ();
}

void
main_window::show_gui_info (void)
{
  QString gui_info
    ( QObject::tr ("<p><strong>A Note about Octave's New GUI</strong></p>"
         "<p>One of the biggest new features for Octave 3.8 is a graphical "
         "user interface.  It is the one thing that users have requested "
         "most often over the last few years and now it is almost ready.  "
         "But because it is not quite as polished as we would like, we "
         "have decided to wait until the 4.0.x release series before "
         "making the GUI the default interface.</p>"
         "<p>Given the length of time and the number of bug fixes and "
         "improvements since the last major release, we also "
         "decided against delaying the release of all these new "
         "improvements any longer just to perfect the GUI.  So please "
         "enjoy the 3.8 release of Octave and the preview of the new GUI.  "
         "We believe it is working reasonably well, but we also know that "
         "there are some obvious rough spots and many things that could be "
         "improved.</p>"
         "<p><strong>We Need Your Help</strong></p>"
         "<p>There are many ways that you can help us fix the remaining "
         "problems, complete the GUI, and improve the overall user "
         "experience for both novices and experts alike (links will open "
         "an external browser):</p>"
         "<p><ul><li>If you are a skilled software developer, you can "
         "help by contributing your time to help "
         "<a href=\"http://octave.org/get-involved.html\">develop "
         "Octave</a>.</li>"
         "<li>If Octave does not work properly, you are encouraged to "
         "<a href=\"http://octave.org/bugs.html\">report problems </a> "
         "that you find.</li>"
         "<li>Whether you are a user or developer, you can "
         "<a href=\"http://octave.org/donate.html\">help to fund the "
         "project</a>.  "
         "Octave development takes a lot of time and expertise.  "
         "Your contributions help to ensure that Octave will continue "
         "to improve.</li></ul></p>"
         "<p>We hope you find Octave to be useful.  Please help us make "
         "it even better for the future!</p>"));

  QMessageBox gui_info_dialog (QMessageBox::Warning,
                               tr ("Experimental GUI Info"),
                               gui_info, QMessageBox::Close);

  gui_info_dialog.exec ();
}

void
main_window::construct_tool_bar (void)
{
  _main_tool_bar = addToolBar ("Main");

  _main_tool_bar->setObjectName ("MainToolBar");
  _main_tool_bar->addAction (_new_script_action);
  _main_tool_bar->addAction (_open_action);

  _main_tool_bar->addSeparator ();

  _main_tool_bar->addAction (_copy_action);
  _main_tool_bar->addAction (_paste_action);
  _main_tool_bar->addAction (_undo_action);

  _main_tool_bar->addSeparator ();

  _current_directory_combo_box = new QComboBox (this);
  _current_directory_combo_box->setFixedWidth (current_directory_width);
  _current_directory_combo_box->setEditable (true);
  _current_directory_combo_box->setInsertPolicy (QComboBox::NoInsert);
  _current_directory_combo_box->setToolTip (tr ("Enter directory name"));
  _current_directory_combo_box->setMaxVisibleItems (
    current_directory_max_visible);
  _current_directory_combo_box->setMaxCount (current_directory_max_count);
  QSizePolicy sizePol (QSizePolicy::Expanding, QSizePolicy::Preferred);
  _current_directory_combo_box->setSizePolicy (sizePol);

  // addWidget takes ownership of the objects so there is no
  // need to delete these upon destroying this main_window.
  _main_tool_bar->addWidget (new QLabel (tr ("Current Directory: ")));
  _main_tool_bar->addWidget (_current_directory_combo_box);
  QAction *current_dir_up = _main_tool_bar->addAction (
                              QIcon (":/actions/icons/up.png"),
                              tr ("One directory up"));
  QAction *current_dir_search = _main_tool_bar->addAction (
                                  QIcon (":/actions/icons/folder.png"),
                                  tr ("Browse directories"));

  connect (_current_directory_combo_box, SIGNAL (activated (QString)),
           this, SLOT (set_current_working_directory (QString)));

  connect (_current_directory_combo_box->lineEdit (), SIGNAL (returnPressed ()),
           this, SLOT (accept_directory_line_edit ()));

  connect (current_dir_search, SIGNAL (triggered ()),
           this, SLOT (browse_for_directory ()));

  connect (current_dir_up, SIGNAL (triggered ()),
           this, SLOT (change_directory_up ()));

  connect (_undo_action, SIGNAL (triggered ()),
           this, SLOT (handle_undo_request ()));
}

void
main_window::save_workspace_callback (const std::string& file)
{
  Fsave (ovl (file));
}

void
main_window::load_workspace_callback (const std::string& file)
{
  Fload (ovl (file));

  octave_link::set_workspace (true, symbol_table::workspace_info ());
}

void
main_window::clear_workspace_callback (void)
{
  Fclear ();
}

void
main_window::rename_variable_callback (const main_window::name_pair& names)
{
  /* bool status = */ symbol_table::rename (names.first, names.second);

  // if (status)
  octave_link::set_workspace (true, symbol_table::workspace_info ());

  //  else
  //    ; // we need an octave_link action that runs a GUI error option.
}

void
main_window::command_window_undo_callback (void)
{
  command_editor::undo ();
  command_editor::redisplay ();
}

void
main_window::clear_command_window_callback (void)
{
  Fclc ();
  command_editor::interrupt (true);
}

void
main_window::resize_command_window_callback (void)
{
  command_editor::resize_terminal ();
}

void
main_window::clear_history_callback (void)
{
  Fhistory (ovl ("-c"));
}

void
main_window::execute_command_callback ()
{
  bool repost = false;          // flag for reposting event for this callback

  if (!_cmd_queue->isEmpty ())  // list can not be empty here, just to make sure
    {
      std::string pending_input = command_editor::get_current_line ();
      command_editor::set_initial_input (pending_input);

      _cmd_queue_mutex.lock (); // critical path
      std::string command = _cmd_queue->takeFirst ().toStdString ();
      if (_cmd_queue->isEmpty ())
        _cmd_processing.release ();  // cmd queue empty, processing will stop
      else
        repost = true;          // not empty, repost at end
      _cmd_queue_mutex.unlock ();

      command_editor::replace_line (command);

      command_editor::redisplay ();
      // We are executing inside the command editor event loop.  Force
      // the current line to be returned for processing.
      command_editor::interrupt ();
    }

  if (repost)  // queue not empty, so repost event for further processing
    octave_link::post_event (this, &main_window::execute_command_callback);

}

void
main_window::new_figure_callback (void)
{
  Fbuiltin (ovl ("figure"));
  Fdrawnow ();
}

void
main_window::change_directory_callback (const std::string& directory)
{
  Fcd (ovl (directory));
}

// The next callbacks are invoked by GUI buttons.  Those buttons
// should only be active when we are doing debugging, which means that
// Octave is waiting for input in get_debug_input.  Calling
// command_editor::interrupt will force readline to return even if it
// has not read any input, and then get_debug_input will return,
// allowing the evaluator to continue and execute the next statement.

void
main_window::queue_debug (QString debug_cmd)
{
  _dbg_queue_mutex.lock ();
  _dbg_queue->append (debug_cmd);   // queue command
  _dbg_queue_mutex.unlock ();

  if (_dbg_processing.tryAcquire ())  // if callback not processing, post event
    octave_link::post_event (this, &main_window::execute_debug_callback);
}

void
main_window::execute_debug_callback ()
{
  bool repost = false;          // flag for reposting event for this callback

  if (!_dbg_queue->isEmpty ())  // list can not be empty here, just to make sure
    {
      _dbg_queue_mutex.lock (); // critical path
      QString debug = _dbg_queue->takeFirst ();
      if (_dbg_queue->isEmpty ())
        _dbg_processing.release ();  // cmd queue empty, processing will stop
      else
        repost = true;          // not empty, repost at end
      _dbg_queue_mutex.unlock ();

      if (debug == "step")
        Fdbstep ();
      else if (debug == "cont")
        Fdbcont ();
      else if (debug == "quit")
        Fdbquit ();
      else
        Fdbstep (ovl (debug.toStdString ()));

      command_editor::interrupt (true);
    }

  if (repost)  // queue not empty, so repost event for further processing
    octave_link::post_event (this, &main_window::execute_debug_callback);

}

void
main_window::exit_callback (void)
{
  Fquit ();
}

void
main_window::find_files (const QString &start_dir)
{

  if (! find_files_dlg)
    {
      find_files_dlg = new find_files_dialog (this);

      connect (find_files_dlg, SIGNAL (finished (int)),
               this, SLOT (find_files_finished (int)));

      connect (find_files_dlg, SIGNAL (dir_selected (const QString &)),
               file_browser_window,
               SLOT (set_current_directory (const QString&)));

      connect (find_files_dlg, SIGNAL (file_selected (const QString &)),
               this, SLOT (open_file (const QString &)));

      find_files_dlg->setWindowModality (Qt::NonModal);
    }

  if (! find_files_dlg->isVisible ())
    {
      find_files_dlg->show ();
    }

  find_files_dlg->set_search_dir (start_dir);

  find_files_dlg->activateWindow ();

}

void
main_window::find_files_finished (int)
{

}

void
main_window::set_global_shortcuts (bool set_shortcuts)
{
  if (set_shortcuts)
    {

      _open_action->setShortcut (QKeySequence::Open);
      _new_script_action->setShortcut (QKeySequence::New);
      _new_function_action->setShortcut (Qt::ControlModifier
                                       + Qt::ShiftModifier
                                       + Qt::Key_N);

      _exit_action->setShortcut (QKeySequence::Quit);

      _find_files_action->setShortcut (Qt::ControlModifier
                                       + Qt::ShiftModifier
                                       + Qt::Key_F);

    }
  else
    {

      QKeySequence no_key = QKeySequence ();

      _open_action->setShortcut (no_key);
      _new_script_action->setShortcut (no_key);
      _new_function_action->setShortcut (no_key);

      _exit_action->setShortcut (no_key);

      _find_files_action->setShortcut (no_key);

    }

  emit set_widget_shortcuts_signal (set_shortcuts);
}

void
main_window::handle_show_doc (const QString& file)
{
  doc_browser_window->setVisible (true);
  emit show_doc_signal (file);
}

void
main_window::clipboard_has_changed (QClipboard::Mode cp_mode)
{
  if (cp_mode == QClipboard::Clipboard)
    {
      if (_clipboard->text ().isEmpty ())
        {
          _paste_action->setEnabled (false);
          _clear_clipboard_action->setEnabled (false);
        }
      else
        {
          _paste_action->setEnabled (true);
          _clear_clipboard_action->setEnabled (true);
        }
    }
}

void
main_window::clear_clipboard ()
{
  _clipboard->clear (QClipboard::Clipboard);
}
