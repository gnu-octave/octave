/*

Copyright (C) 2013-2019 John W. Eaton
Copyright (C) 2011-2019 Jacob Dawid
Copyright (C) 2011-2019 John P. Swensen

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

#include <QDialog>
#include <QDir>
#include <QMetaType>
#include <QPushButton>
#include <QStringList>

#include "dialog.h"
#include "qt-interpreter-events.h"
#include "resource-manager.h"

#include "oct-env.h"
#include "str-vec.h"

#include "builtin-defun-decls.h"
#include "error.h"
#include "interpreter-private.h"
#include "load-path.h"
#include "oct-map.h"
#include "octave.h"
#include "ov.h"
#include "syminfo.h"
#include "utils.h"

Q_DECLARE_METATYPE (octave_value)
Q_DECLARE_METATYPE (octave::symbol_info_list)
Q_DECLARE_METATYPE (octave::fcn_callback)
Q_DECLARE_METATYPE (octave::meth_callback)

namespace octave
{
  static QStringList
  make_qstring_list (const std::list<std::string>& lst)
  {
    QStringList retval;

    for (auto it = lst.begin (); it != lst.end (); it++)
      retval.append (QString::fromStdString (*it));

    return retval;
  }

  static QStringList
  make_filter_list (const event_manager::filter_list& lst)
  {
    QStringList retval;

    // We have pairs of data, first being the list of extensions
    // exta;exb;extc etc second the name to use as filter name
    // (optional).  Qt wants a list of filters in the format of
    // 'FilterName (space separated exts)'.

    for (auto it = lst.begin (); it != lst.end (); it++)
      {
        QString ext = QString::fromStdString (it->first);
        QString name = QString::fromStdString (it->second);

        // Strip out extensions from name and replace ';' with spaces in
        // list.

        name.replace (QRegExp ("\\(.*\\)"), "");
        ext.replace (";", " ");

        if (name.isEmpty ())
          {
            // No name field.  Build one from the extensions.
            name = ext.toUpper () + " Files";
          }

        retval.append (name + " (" + ext + ')');
      }

    return retval;
  }

  qt_interpreter_events::qt_interpreter_events (void)
    : interpreter_events (), m_shutdown_confirm_result (false)
  {
    qRegisterMetaType<octave_value> ("octave_value");
    qRegisterMetaType<symbol_info_list> ("symbol_info_list");

    qRegisterMetaType<fcn_callback> ("fcn_callback");
    qRegisterMetaType<meth_callback> ("meth_callback");
  }

  std::list<std::string>
  qt_interpreter_events::file_dialog (const filter_list& filter,
                                      const std::string& title,
                                      const std::string& filename,
                                      const std::string& dirname,
                                      const std::string& multimode)
  {
    std::list<std::string> retval;

    // Lock mutex before signaling.
    uiwidget_creator.lock ();

    uiwidget_creator.signal_filedialog (make_filter_list (filter),
                                        QString::fromStdString (title),
                                        QString::fromStdString (filename),
                                        QString::fromStdString (dirname),
                                        QString::fromStdString (multimode));

    // Wait while the user is responding to dialog.
    uiwidget_creator.wait ();

    // The GUI has sent a signal and the thread has been awakened.

    // Add all the file dialog results to a string list.
    QStringList inputLine = uiwidget_creator.get_string_list ();

    for (auto it = inputLine.begin (); it != inputLine.end (); it++)
      retval.push_back (it->toStdString ());

    retval.push_back (uiwidget_creator.get_dialog_path ().toStdString ());
    retval.push_back ((QString ("%1").arg (uiwidget_creator.get_dialog_result ())).toStdString ());

    uiwidget_creator.unlock ();

    return retval;
  }

  std::list<std::string>
  qt_interpreter_events::input_dialog (const std::list<std::string>& prompt,
                                       const std::string& title,
                                       const std::list<float>& nr,
                                       const std::list<float>& nc,
                                       const std::list<std::string>& defaults)
  {
    std::list<std::string> retval;

    // Lock mutex before signaling.
    uiwidget_creator.lock ();

    uiwidget_creator.signal_inputlayout (make_qstring_list (prompt),
                                         QString::fromStdString (title),
                                         QFloatList::fromStdList (nr),
                                         QFloatList::fromStdList (nc),
                                         make_qstring_list (defaults));

    // Wait while the user is responding to message box.
    uiwidget_creator.wait ();

    // The GUI has sent a signal and the thread has been awakened.

    QStringList inputLine = uiwidget_creator.get_string_list ();

    uiwidget_creator.unlock ();

    for (auto it = inputLine.begin (); it != inputLine.end (); it++)
      retval.push_back (it->toStdString ());

    return retval;
  }

  std::pair<std::list<int>, int>
  qt_interpreter_events::list_dialog (const std::list<std::string>& list,
                                      const std::string& mode,
                                      int width, int height,
                                      const std::list<int>& initial,
                                      const std::string& name,
                                      const std::list<std::string>& prompt,
                                      const std::string& ok_string,
                                      const std::string& cancel_string)
  {
    // Lock mutex before signaling.
    uiwidget_creator.lock ();

    uiwidget_creator.signal_listview (make_qstring_list (list),
                                      QString::fromStdString (mode),
                                      width, height,
                                      QList<int>::fromStdList (initial),
                                      QString::fromStdString (name),
                                      make_qstring_list (prompt),
                                      QString::fromStdString (ok_string),
                                      QString::fromStdString (cancel_string));

    // Wait while the user is responding to message box.
    uiwidget_creator.wait ();

    // The GUI has sent a signal and the thread has been awakened.

    QIntList selected = uiwidget_creator.get_list_index ();
    int ok = uiwidget_creator.get_dialog_result ();

    uiwidget_creator.unlock ();

    return std::pair<std::list<int>, int> (selected.toStdList (), ok);
  }

  std::string
  qt_interpreter_events::question_dialog (const std::string& msg,
                                          const std::string& title,
                                          const std::string& btn1,
                                          const std::string& btn2,
                                          const std::string& btn3,
                                          const std::string& btndef)
  {
    QStringList btn;
    QStringList role;
    // Must use ResetRole which is left-aligned for all OS and WM.
    role << "ResetRole" << "ResetRole" << "ResetRole";
    btn << QString::fromStdString (btn1);
    if (btn2 == "")
      role.removeAt (0);
    else
      btn << QString::fromStdString (btn2);
    btn << QString::fromStdString (btn3);

    // Lock mutex before signaling.
    uiwidget_creator.lock ();

    uiwidget_creator.signal_dialog (QString::fromStdString (msg),
                                    QString::fromStdString (title),
                                    "quest", btn,
                                    QString::fromStdString (btndef),
                                    role);

    // Wait while the user is responding to message box.
    uiwidget_creator.wait ();

    // The GUI has sent a signal and the thread has been awakened.

    std::string answer = uiwidget_creator.get_dialog_button ().toStdString ();

    uiwidget_creator.unlock ();

    return answer;
  }

  void qt_interpreter_events::update_path_dialog (void)
  {
    emit update_path_dialog_signal ();
  }

  void qt_interpreter_events::show_preferences (void)
  {
    emit show_preferences_signal ();
  }

  void qt_interpreter_events::show_doc (const std::string& file)
  {
    emit show_doc_signal (QString::fromStdString (file));
  }

  bool qt_interpreter_events::edit_file (const std::string& file)
  {
    emit edit_file_signal (QString::fromStdString (file));

    return true;
  }

  void qt_interpreter_events::edit_variable (const std::string& expr,
                                             const octave_value& val)
  {
    emit edit_variable_signal (QString::fromStdString (expr), val);
  }

  bool qt_interpreter_events::confirm_shutdown (void)
  {
    // Lock the mutex before emitting signal.
    lock ();

    emit confirm_shutdown_signal ();

    // Wait while the GUI shuts down.
    wait ();

    // The GUI has sent a signal and the thread has been awakened.

    unlock ();

    return m_shutdown_confirm_result;
  }

  bool qt_interpreter_events::prompt_new_edit_file (const std::string& file)
  {
    QSettings *settings = resource_manager::get_settings ();

    if (! settings || settings->value ("editor/create_new_file",false).toBool ())
      return true;

    std::string abs_fname = sys::env::make_absolute (file);

    QStringList btn;
    QStringList role;
    role << "YesRole" << "RejectRole";
    btn << tr ("Create") << tr ("Cancel");

    // Lock mutex before signaling.
    uiwidget_creator.lock ();

    uiwidget_creator.signal_dialog
      (tr ("File\n%1\ndoes not exist. Do you want to create it?").
       arg (QString::fromStdString (abs_fname)),
       tr ("Octave Editor"), "quest", btn, tr ("Create"), role);

    // Wait while the user is responding to message box.
    uiwidget_creator.wait ();

    // The GUI has sent a signal and the thread has been awakened.

    QString answer = uiwidget_creator.get_dialog_button ();

    uiwidget_creator.unlock ();

    return (answer == tr ("Create"));
  }

  // Prompt to allow file to be run by setting cwd (or if
  // addpath_option==true, alternatively setting the path).  This uses a
  // QMessageBox unlike other functions in this file, because
  // uiwidget_creator.waitcondition.wait hangs when called from
  // file_editor_tab::handle_context_menu_break_condition().  (FIXME:
  // why hang?)

  int
  qt_interpreter_events::debug_cd_or_addpath_error (const std::string& file,
                                                    const std::string& dir,
                                                    bool addpath_option)
  {
    int retval = -1;

    QString qdir = QString::fromStdString (dir);
    QString qfile = QString::fromStdString (file);
    QString msg
      = (addpath_option
         ? tr ("The file %1 does not exist in the load path.  To run or debug the function you are editing, you must either change to the directory %2 or add that directory to the load path.").arg (qfile).arg (qdir)
         : tr ("The file %1 is shadowed by a file with the same name in the load path. To run or debug the function you are editing, change to the directory %2.").arg (qfile).arg (qdir));

    QString title = tr ("Change Directory or Add Directory to Load Path");

    QString cd_txt = tr ("&Change Directory");
    QString addpath_txt = tr ("&Add Directory to Load Path");
    QString cancel_txt = tr ("Cancel");

    QStringList btn;
    QStringList role;
    btn << cd_txt;
    role << "YesRole";
    if (addpath_option)
      {
        btn << addpath_txt;
        role << "AcceptRole";
      }
    btn << cancel_txt;
    role << "RejectRole";

    // Lock mutex before signaling.
    uiwidget_creator.lock ();

    uiwidget_creator.signal_dialog (msg, title, "quest", btn, cancel_txt, role);

    // Wait while the user is responding to message box.
    uiwidget_creator.wait ();

    // The GUI has sent a signal and the thread has been awakened.

    QString result = uiwidget_creator.get_dialog_button ();

    uiwidget_creator.unlock ();

    if (result == cd_txt)
      retval = 1;
    else if (result == addpath_txt)
      retval = 2;

    return retval;
  }

  uint8NDArray qt_interpreter_events::get_named_icon (const std::string& icon_name)
  {
    uint8NDArray retval;
    QIcon icon = resource_manager::icon (QString::fromStdString (icon_name));
    if (! icon.isNull ())
      {
        QImage img = icon.pixmap (QSize (32, 32)).toImage ();

        if (img.format () == QImage::Format_ARGB32_Premultiplied)
          {
            retval.resize (dim_vector (img.height (), img.width (), 4), 0);
            uint8_t* bits = img.bits ();
            for (int i = 0; i < img.height (); i++)
              for (int j = 0; j < img.width (); j++)
                {
                  retval(i,j,2) = bits[0];
                  retval(i,j,1) = bits[1];
                  retval(i,j,0) = bits[2];
                  retval(i,j,3) = bits[3];
                  bits += 4;
                }
          }
      }
    return retval;
  }

  std::string
  qt_interpreter_events::gui_preference (const std::string& key,
                                         const std::string& value)
  {
    QString pref_value;

    // Lock the mutex before signaling
    lock ();

    // Emit the signal for changing or getting a preference
    emit gui_preference_signal (QString::fromStdString (key),
                                QString::fromStdString (value), &pref_value);

    // Wait for the GUI and unlock when resumed
    wait ();
    unlock ();

    return pref_value.toStdString ();
  }

  bool qt_interpreter_events::copy_image_to_clipboard (const std::string& file)
  {
    emit copy_image_to_clipboard_signal (QString::fromStdString (file), true);

    return true;
  }

  void qt_interpreter_events::execute_command_in_terminal
  (const std::string& command)
  {
    emit execute_command_in_terminal_signal (QString::fromStdString (command));
  }

  void qt_interpreter_events::register_doc (const std::string& file)
  {
    emit register_doc_signal (QString::fromStdString (file));
  }

  void qt_interpreter_events::unregister_doc (const std::string& file)
  {
    emit unregister_doc_signal (QString::fromStdString (file));
  }

  void qt_interpreter_events::directory_changed (const std::string& dir)
  {
    emit directory_changed_signal (QString::fromStdString (dir));
  }

  void qt_interpreter_events::file_remove (const std::string& old_name,
                                           const std::string& new_name)
  {
    // Lock the mutex before signaling
    lock ();

    // Emit the signal for the editor for closing the file if it is open
    emit file_remove_signal (QString::fromStdString (old_name),
                             QString::fromStdString (new_name));

    // Wait for the GUI and unlock when resumed
    wait ();
    unlock ();
  }

  void qt_interpreter_events::file_renamed (bool load_new)
  {
    emit file_renamed_signal (load_new);
  }

  void qt_interpreter_events::set_workspace (bool top_level, bool debug,
                                             const symbol_info_list& syminfo,
                                             bool update_variable_editor)
  {
    if (! top_level && ! debug)
      return;

    emit set_workspace_signal (top_level, debug, syminfo);

    if (update_variable_editor)
      emit refresh_variable_editor_signal ();
  }

  void qt_interpreter_events::clear_workspace (void)
  {
    emit clear_workspace_signal ();
  }

  void qt_interpreter_events::set_history (const string_vector& hist)
  {
    QStringList qt_hist;

    for (octave_idx_type i = 0; i < hist.numel (); i++)
      qt_hist.append (QString::fromStdString (hist[i]));

    emit set_history_signal (qt_hist);
  }

  void qt_interpreter_events::append_history (const std::string& hist_entry)
  {
    emit append_history_signal (QString::fromStdString (hist_entry));
  }

  void qt_interpreter_events::clear_history (void)
  {
    emit clear_history_signal ();
  }

  void qt_interpreter_events::pre_input_event (void)
  { }

  void qt_interpreter_events::post_input_event (void)
  { }

  void qt_interpreter_events::enter_debugger_event (const std::string& /*fcn_name*/,
                                                    const std::string& fcn_file_name,
                                                    int line)
  {
    if (fcn_file_name.empty ())
      return;

    insert_debugger_pointer (fcn_file_name, line);

    emit enter_debugger_signal ();
  }

  void
  qt_interpreter_events::execute_in_debugger_event (const std::string& file,
                                                    int line)
  {
    delete_debugger_pointer (file, line);
  }

  void qt_interpreter_events::exit_debugger_event (void)
  {
    emit exit_debugger_signal ();
  }

  // Display (if @insert true) or remove the appropriate symbol for a breakpoint
  // in @file at @line with condition @cond.
  void qt_interpreter_events::update_breakpoint (bool insert,
                                                 const std::string& file,
                                                 int line,
                                                 const std::string& cond)
  {
    emit update_breakpoint_marker_signal (insert, QString::fromStdString (file),
                                          line, QString::fromStdString (cond));
  }

  void
  qt_interpreter_events::insert_debugger_pointer (const std::string& file,
                                                  int line)
  {
    emit insert_debugger_pointer_signal (QString::fromStdString (file), line);
  }

  void
  qt_interpreter_events::delete_debugger_pointer (const std::string& file,
                                                  int line)
  {
    emit delete_debugger_pointer_signal (QString::fromStdString (file), line);
  }
}
