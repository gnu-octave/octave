/*

Copyright (C) 2013-2018 John W. Eaton
Copyright (C) 2011-2018 Jacob Dawid
Copyright (C) 2011-2018 John P. Swensen

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

#include "oct-env.h"
#include "str-vec.h"

#include "builtin-defun-decls.h"
#include "dialog.h"
#include "error.h"
#include "interpreter-private.h"
#include "load-path.h"
#include "ov.h"
#include "octave.h"
#include "oct-map.h"
#include "symscope.h"
#include "utils.h"

#include "octave-gui.h"
#include "octave-qt-link.h"
#include "resource-manager.h"

Q_DECLARE_METATYPE (octave_value)
Q_DECLARE_METATYPE (octave::symbol_scope)

namespace octave
{
  octave_qt_link::octave_qt_link (QWidget *, gui_application *app_context)
    : octave_link (), m_app_context (app_context)
  {
    qRegisterMetaType<octave_value> ("octave_value");
    qRegisterMetaType<symbol_scope> ("symbol_scope");
  }

  bool octave_qt_link::do_confirm_shutdown (void)
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

  bool octave_qt_link::do_copy_image_to_clipboard (const std::string& file)
  {
    emit copy_image_to_clipboard_signal (QString::fromStdString (file), true);

    return true;
  }

  bool octave_qt_link::do_edit_file (const std::string& file)
  {
    emit edit_file_signal (QString::fromStdString (file));

    return true;
  }

  bool octave_qt_link::do_prompt_new_edit_file (const std::string& file)
  {
    QSettings *settings = resource_manager::get_settings ();

    if (! settings || settings->value ("editor/create_new_file",false).toBool ())
      return true;

    std::string abs_fname = octave::sys::env::make_absolute (file);

    QStringList btn;
    QStringList role;
    role << "YesRole" << "RejectRole";
    btn << tr ("Create") << tr ("Cancel");

    // Lock mutex before signaling.
    uiwidget_creator.lock ();

    uiwidget_creator.signal_dialog (
                                    tr ("File\n%1\ndoes not exist. Do you want to create it?").
                                    arg (QString::fromStdString (abs_fname)),
                                    tr ("Octave Editor"), "quest", btn, tr ("Create"), role);

    // Wait while the user is responding to message box.
    uiwidget_creator.wait ();

    // The GUI has sent a signal and the thread has been awakened.

    QString answer = uiwidget_creator.get_dialog_button ();

    uiwidget_creator.unlock ();

    return (answer == tr ("Create"));
  }

  uint8NDArray octave_qt_link::do_get_named_icon (const std::string& icon_name)
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

  std::string octave_qt_link::do_question_dialog (const std::string& msg,
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
                                    "quest",
                                    btn,
                                    QString::fromStdString (btndef),
                                    role);

    // Wait while the user is responding to message box.
    uiwidget_creator.wait ();

    // The GUI has sent a signal and the thread has been awakened.

    std::string answer = uiwidget_creator.get_dialog_button ().toStdString ();

    uiwidget_creator.unlock ();

    return answer;
  }

  static QStringList
  make_qstring_list (const std::list<std::string>& lst)
  {
    QStringList retval;

    for (auto it = lst.begin (); it != lst.end (); it++)
      retval.append (QString::fromStdString (*it));

    return retval;
  }

  static QStringList
  make_filter_list (const octave_link::filter_list& lst)
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

  std::pair<std::list<int>, int>
  octave_qt_link::do_list_dialog (const std::list<std::string>& list,
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

    const QIntList *selected = uiwidget_creator.get_list_index ();
    int ok = uiwidget_creator.get_dialog_result ();

    uiwidget_creator.unlock ();

    return std::pair<std::list<int>, int> (selected->toStdList (), ok);
  }

  std::list<std::string>
  octave_qt_link::do_input_dialog (const std::list<std::string>& prompt,
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

    const QStringList *inputLine = uiwidget_creator.get_string_list ();

    uiwidget_creator.unlock ();

    for (auto it = inputLine->begin (); it != inputLine->end (); it++)
      retval.push_back (it->toStdString ());

    return retval;
  }

  std::list<std::string>
  octave_qt_link::do_file_dialog (const filter_list& filter,
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
    const QStringList *inputLine = uiwidget_creator.get_string_list ();

    for (auto it = inputLine->begin (); it != inputLine->end (); it++)
      retval.push_back (it->toStdString ());

    retval.push_back (uiwidget_creator.get_dialog_path ()->toStdString ());
    retval.push_back ((QString ("%1").arg (
                                           uiwidget_creator.get_dialog_result ())).toStdString ());

    uiwidget_creator.unlock ();

    return retval;
  }

  // Prompt to allow file to be run by setting cwd (or if addpath_option==true,
  // alternatively setting the path).
  // This uses a QMessageBox unlike other functions in this file,
  // because uiwidget_creator.waitcondition.wait hangs when called from
  // file_editor_tab::handle_context_menu_break_condition().  (FIXME: why hang?)
  int octave_qt_link::do_debug_cd_or_addpath_error (const std::string& file,
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

  void octave_qt_link::do_change_directory (const std::string& dir)
  {
    emit change_directory_signal (QString::fromStdString (dir));
  }

  void octave_qt_link::do_file_remove (const std::string& old_name,
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

  void octave_qt_link::do_file_renamed (bool load_new)
  {
    emit file_renamed_signal (load_new);
  }

  void octave_qt_link::do_execute_command_in_terminal
    (const std::string& command)
  {
    emit execute_command_in_terminal_signal (QString::fromStdString (command));
  }

  void octave_qt_link::do_set_workspace (bool top_level, bool debug,
                                         const symbol_scope& scope,
                                         bool update_variable_editor)
  {
    if (! top_level && ! debug)
      return;

    emit set_workspace_signal (top_level, debug, scope);

    if (update_variable_editor)
      emit refresh_variable_editor_signal ();
  }

  void octave_qt_link::do_clear_workspace (void)
  {
    emit clear_workspace_signal ();
  }

  void octave_qt_link::do_set_history (const string_vector& hist)
  {
    QStringList qt_hist;

    for (octave_idx_type i = 0; i < hist.numel (); i++)
      qt_hist.append (QString::fromStdString (hist[i]));

    emit set_history_signal (qt_hist);
  }

  void octave_qt_link::do_append_history (const std::string& hist_entry)
  {
    emit append_history_signal (QString::fromStdString (hist_entry));
  }

  void octave_qt_link::do_clear_history (void)
  {
    emit clear_history_signal ();
  }

  void octave_qt_link::do_pre_input_event (void)
  { }

  void octave_qt_link::do_post_input_event (void)
  { }

  void octave_qt_link::do_enter_debugger_event (const std::string& file,
                                                int line)
  {
    interpreter& interp = __get_interpreter__ (
                                  "octave_qt_link::do_enter_debugger_event");
    octave_value_list fct = F__which__ (interp, ovl (file),0);
    octave_map map = fct(0).map_value ();

    std::string type = map.contents ("type").data ()[0].string_value ();
    if (type == "command-line function")
      return;

    do_insert_debugger_pointer (file, line);

    emit enter_debugger_signal ();
  }

  void octave_qt_link::do_execute_in_debugger_event (const std::string& file,
                                                     int line)
  {
    do_delete_debugger_pointer (file, line);
  }

  void octave_qt_link::do_exit_debugger_event (void)
  {
    emit exit_debugger_signal ();
  }

  // Display (if @insert true) or remove the appropriate symbol for a breakpoint
  // in @file at @line with condition @cond.
  void octave_qt_link::do_update_breakpoint (bool insert,
                                             const std::string& file,
                                             int line,
                                             const std::string& cond)
  {
    emit update_breakpoint_marker_signal (insert, QString::fromStdString (file),
                                          line, QString::fromStdString (cond));
  }

  bool octave_qt_link::file_in_path (const std::string& file,
                                     const std::string& dir)
  {

    bool ok = false;
    bool addpath_option = true;

    std::string curr_dir = sys::env::get_current_directory ();

    if (same_file (curr_dir, dir))
      ok = true;
    else
      {
        load_path& lp = __get_load_path__ ("octave_qt_link::file_in_path");

        bool dir_in_load_path = lp.contains_canonical (dir);

        // get base name, allowing "@class/method.m" (bug #41514)
        std::string base_file = (file.length () > dir.length ())
          ? file.substr (dir.length () + 1)
          : sys::env::base_pathname (file);

        std::string lp_file = lp.find_file (base_file);

        if (dir_in_load_path)
          {
            if (same_file (lp_file, file))
              ok = true;
          }
        else
          {
            // File directory is not in path.  Is the file in the path in
            // the current directory?  If so, then changing the current
            // directory will be needed.  Adding directory to path is
            // not enough because the file in the current directory would
            // still be found.

            if (same_file (lp_file, base_file))
              {
                if (same_file (curr_dir, dir))
                  ok = true;
                else
                  addpath_option = false;
              }
          }
      }

    if (! ok)
      {
        int action = debug_cd_or_addpath_error (file, dir, addpath_option);
        switch (action)
          {
          case 1:
            Fcd (ovl (dir));
            ok = true;
            break;

          case 2:
            {
              load_path& lp = __get_load_path__ ("octave_qt_link::file_in_path");

              lp.prepend (dir);
              ok = true;
            }
            break;

          default:
            break;
          }
      }

    return ok;
  }

  void octave_qt_link::do_show_preferences (void)
  {
    emit show_preferences_signal ();
  }

  std::string octave_qt_link::do_gui_preference (const std::string& key,
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

  void octave_qt_link::do_show_doc (const std::string& file)
  {
    emit show_doc_signal (QString::fromStdString (file));
  }

  void octave_qt_link::do_register_doc (const std::string& file)
  {
    emit register_doc_signal (QString::fromStdString (file));
  }

  void octave_qt_link::do_unregister_doc (const std::string& file)
  {
    emit unregister_doc_signal (QString::fromStdString (file));
  }

  void octave_qt_link::do_edit_variable (const std::string& expr,
                                         const octave_value& val)
  {
    emit edit_variable_signal (QString::fromStdString (expr), val);
  }

  void octave_qt_link::do_insert_debugger_pointer (const std::string& file,
                                                   int line)
  {
    emit insert_debugger_pointer_signal (QString::fromStdString (file), line);
  }

  void octave_qt_link::do_delete_debugger_pointer (const std::string& file,
                                                   int line)
  {
    emit delete_debugger_pointer_signal (QString::fromStdString (file), line);
  }
}
