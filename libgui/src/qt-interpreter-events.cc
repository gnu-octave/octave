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

#include <iostream>
#include <sstream>

#include <QDialog>
#include <QDir>
#include <QIcon>
#include <QMetaType>
#include <QPushButton>
#include <QStringList>

#include "dialog.h"
#include "gui-preferences-ed.h"
#include "octave-qobject.h"
#include "qt-interpreter-events.h"
#include "qt-utils.h"

#include "localcharset-wrapper.h"
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

OCTAVE_BEGIN_NAMESPACE(octave)

static QStringList
make_qstring_list (const std::list<std::string>& lst)
{
  QStringList retval;

  for (const auto& s : lst)
    retval.append (QString::fromStdString (s));

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

  for (const auto& ext_name : lst)
    {
      QString ext = QString::fromStdString (ext_name.first);
      QString name = QString::fromStdString (ext_name.second);

      // Strip out extensions from name and replace ';' with spaces in list.

      name.replace (QRegExp (R"(\(.*\))"), "");
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

qt_interpreter_events::qt_interpreter_events (base_qobject& oct_qobj)
  : interpreter_events (), m_octave_qobj (oct_qobj),
    m_uiwidget_creator (oct_qobj), m_result (), m_mutex (),
    m_waitcondition ()
{
  qRegisterMetaType<QIntList> ("QIntList");
  qRegisterMetaType<QFloatList> ("QFloatList");

  qRegisterMetaType<octave_value> ("octave_value");
  qRegisterMetaType<symbol_info_list> ("symbol_info_list");

  qRegisterMetaType<fcn_callback> ("fcn_callback");
  qRegisterMetaType<meth_callback> ("meth_callback");

  connect (this, &qt_interpreter_events::confirm_shutdown_signal,
           this, &qt_interpreter_events::confirm_shutdown_octave);

  connect (this, &qt_interpreter_events::get_named_icon_signal,
           this, &qt_interpreter_events::get_named_icon_slot);

  connect (this, &qt_interpreter_events::gui_preference_signal,
           this, &qt_interpreter_events::gui_preference_slot);
}

void qt_interpreter_events::start_gui (bool gui_app)
{
  if (m_octave_qobj.experimental_terminal_widget ())
    emit start_gui_signal (gui_app);
}

void qt_interpreter_events::close_gui (void)
{
  if (m_octave_qobj.experimental_terminal_widget ())
    emit close_gui_signal ();
}

std::list<std::string>
qt_interpreter_events::file_dialog (const filter_list& filter,
                                    const std::string& title,
                                    const std::string& filename,
                                    const std::string& dirname,
                                    const std::string& multimode)
{
  QStringList lst
    = m_uiwidget_creator.file_dialog (make_filter_list (filter),
                                      QString::fromStdString (title),
                                      QString::fromStdString (filename),
                                      QString::fromStdString (dirname),
                                      QString::fromStdString (multimode));

  std::list<std::string> retval;

  for (const auto& s : lst)
    retval.push_back (s.toStdString ());

  return retval;
}

std::list<std::string>
qt_interpreter_events::input_dialog (const std::list<std::string>& prompt,
                                     const std::string& title,
                                     const std::list<float>& nr,
                                     const std::list<float>& nc,
                                     const std::list<std::string>& defaults)
{
  QStringList lst
    = m_uiwidget_creator.input_dialog (make_qstring_list (prompt),
                                       QString::fromStdString (title),
                                       std_list_to_qt_list<float> (nr),
                                       std_list_to_qt_list<float> (nc),
                                       make_qstring_list (defaults));
  std::list<std::string> retval;

  for (const auto& s : lst)
    retval.push_back (s.toStdString ());

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
  QPair<QIntList, int> result
    = m_uiwidget_creator.list_dialog (make_qstring_list (list),
                                      QString::fromStdString (mode),
                                      width, height,
                                      std_list_to_qt_list<int> (initial),
                                      QString::fromStdString (name),
                                      make_qstring_list (prompt),
                                      QString::fromStdString (ok_string),
                                      QString::fromStdString (cancel_string));

  QIntList& lst = result.first;
  return std::pair<std::list<int>, int> (std::list<int> (lst.begin (),
                                                         lst.end ()),
                                         result.second);
}

std::string
qt_interpreter_events::question_dialog (const std::string& msg,
                                        const std::string& title,
                                        const std::string& btn1,
                                        const std::string& btn2,
                                        const std::string& btn3,
                                        const std::string& btndef)
{
  QString icon = "quest";
  QStringList buttons;
  QStringList role;

  // Must use ResetRole which is left-aligned for all OS and WM.
  role << "ResetRole" << "ResetRole" << "ResetRole";

  buttons << QString::fromStdString (btn1);
  if (btn2 == "")
    role.removeAt (0);
  else
    buttons << QString::fromStdString (btn2);
  buttons << QString::fromStdString (btn3);

  QString answer
    = m_uiwidget_creator.message_dialog (QString::fromStdString (msg),
                                         QString::fromStdString (title),
                                         icon, buttons,
                                         QString::fromStdString (btndef),
                                         role);

  return answer.toStdString ();
}

void qt_interpreter_events::update_path_dialog (void)
{
  emit update_path_dialog_signal ();
}

void qt_interpreter_events::show_preferences (void)
{
  emit show_preferences_signal ();
}

void qt_interpreter_events::apply_preferences (void)
{
  emit apply_new_settings ();
}

void qt_interpreter_events::show_terminal_window (void)
{
  emit show_terminal_window_signal ();
}

bool qt_interpreter_events::show_documentation (const std::string& file)
{
  emit show_documentation_signal (QString::fromStdString (file));

  return true;
}

void qt_interpreter_events::show_file_browser (void)
{
  emit show_file_browser_signal ();
}

void qt_interpreter_events::show_command_history (void)
{
  emit show_command_history_signal ();
}

void qt_interpreter_events::show_workspace (void)
{
  emit show_workspace_signal ();
}

void qt_interpreter_events::show_community_news (int serial)
{
  emit show_community_news_signal (serial);
}

void qt_interpreter_events::show_release_notes (void)
{
  emit show_release_notes_signal ();
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
  QMutexLocker autolock (&m_mutex);

  emit confirm_shutdown_signal ();

  // Wait for result.
  wait ();

  return m_result.toBool ();
}

bool qt_interpreter_events::prompt_new_edit_file (const std::string& file)
{
  resource_manager& rmgr = m_octave_qobj.get_resource_manager ();
  gui_settings *settings = rmgr.get_settings ();

  if (! settings || settings->value (ed_create_new_file).toBool ())
    return true;

  std::string abs_fname = sys::env::make_absolute (file);

  QStringList btn;
  QStringList role;
  role << "YesRole" << "RejectRole";
  btn << tr ("Create") << tr ("Cancel");

  QString answer = m_uiwidget_creator.message_dialog
    (tr ("File\n%1\ndoes not exist. Do you want to create it?").
     arg (QString::fromStdString (abs_fname)),
     tr ("Octave Editor"), "quest", btn, tr ("Create"), role);

  return (answer == tr ("Create"));
}

// Prompt to allow file to be run by setting cwd (or if
// addpath_option==true, alternatively setting the path).

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

  QString result
    = m_uiwidget_creator.message_dialog (msg, title, "quest", btn,
                                         cancel_txt, role);

  if (result == cd_txt)
    retval = 1;
  else if (result == addpath_txt)
    retval = 2;

  return retval;
}

uint8NDArray qt_interpreter_events::get_named_icon (const std::string& name)
{
  QMutexLocker autolock (&m_mutex);

  emit get_named_icon_signal (QString::fromStdString (name));

  // Wait for result.
  wait ();

  uint8NDArray empty_img;

  QIcon icon = m_result.value<QIcon> ();

  if (icon.isNull ())
    return empty_img;

  QImage img = icon.pixmap (QSize (32, 32)).toImage ();

  if (img.format () != QImage::Format_ARGB32_Premultiplied)
    return empty_img;

  dim_vector dims (img.height (), img.width (), 4);

  uint8NDArray retval (dims, 0);

  uint8_t *bits = img.bits ();

  for (int i = 0; i < img.height (); i++)
    {
      for (int j = 0; j < img.width (); j++)
        {
          retval(i, j, 2) = bits[0];
          retval(i, j, 1) = bits[1];
          retval(i, j, 0) = bits[2];
          retval(i, j, 3) = bits[3];

          bits += 4;
        }
    }

  return retval;
}

void qt_interpreter_events::get_named_icon_slot (const QString& name)
{
  QMutexLocker autolock (&m_mutex);

  resource_manager& rmgr = m_octave_qobj.get_resource_manager ();
  m_result = QVariant::fromValue (rmgr.icon (name));

  wake_all ();
}

std::string
qt_interpreter_events::gui_preference (const std::string& key,
                                       const std::string& value)
{
  QString pref_value;

  QMutexLocker autolock (&m_mutex);

  // Emit the signal for changing or getting a preference
  emit gui_preference_signal (QString::fromStdString (key),
                              QString::fromStdString (value));

  // Wait for response (pref_value).
  wait ();

  QString pref = m_result.toString ();

  return pref.toStdString ();
}

bool qt_interpreter_events::copy_image_to_clipboard (const std::string& file)
{
  emit copy_image_to_clipboard_signal (QString::fromStdString (file), true);

  return true;
}

void qt_interpreter_events::focus_window (const std::string win_name)
{
  emit focus_window_signal (QString::fromStdString (win_name));
}

void qt_interpreter_events::execute_command_in_terminal
(const std::string& command)
{
  emit execute_command_in_terminal_signal (QString::fromStdString (command));
}

void qt_interpreter_events::register_documentation (const std::string& file)
{
  emit register_documentation_signal (QString::fromStdString (file));
}

void qt_interpreter_events::unregister_documentation (const std::string& file)
{
  emit unregister_documentation_signal (QString::fromStdString (file));
}

void qt_interpreter_events::interpreter_output (const std::string& msg)
{
  if (m_octave_qobj.experimental_terminal_widget ()
      && m_octave_qobj.have_terminal_window ())
    emit interpreter_output_signal (QString::fromStdString (msg));
  else
    {
      // FIXME: is this the correct thing to do?
      std::cout << msg;
    }
}

void qt_interpreter_events::display_exception (const execution_exception& ee,
                                               bool beep)
{
  if (m_octave_qobj.experimental_terminal_widget ()
      && m_octave_qobj.have_terminal_window ())
    {
      // Output the exception message
      std::ostringstream buf;
      ee.display (buf);
      emit interpreter_output_signal (QString::fromStdString (buf.str ()));
      // Create w new command line
      emit new_command_line_signal ();
    }
  else
    {
      if (beep)
        std::cerr << "\a";

      ee.display (std::cerr);
    }
}

void qt_interpreter_events::gui_status_update (const std::string& feature,
                                               const std::string& status)
{
  emit gui_status_update_signal (QString::fromStdString (feature),
                                 QString::fromStdString (status));
}

void qt_interpreter_events::update_gui_lexer (void)
{
  emit update_gui_lexer_signal (true);
}

void qt_interpreter_events::directory_changed (const std::string& dir)
{
  emit directory_changed_signal (QString::fromStdString (dir));
}

void qt_interpreter_events::file_remove (const std::string& old_name,
                                         const std::string& new_name)
{
  QMutexLocker autolock (&m_mutex);

  // Emit the signal for the editor for closing the file if it is open
  emit file_remove_signal (QString::fromStdString (old_name),
                           QString::fromStdString (new_name));

  // Wait for file removal to complete before continuing.
  wait ();
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

void qt_interpreter_events::update_prompt (const std::string& prompt)
{
  emit update_prompt_signal (QString::fromStdString (prompt));
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

void
qt_interpreter_events::confirm_shutdown_octave (void)
{
  QMutexLocker autolock (&m_mutex);

  m_result = m_octave_qobj.confirm_shutdown ();

  wake_all ();
}

// If VALUE is empty, return current value of preference named by KEY.
//
// If VALUE is not empty, set preference named by KEY to VALUE return
// previous value.
//
// FIXME: should we have separate get and set functions?  With only
// one, we don't allow a preference value to be set to the empty
// string.

void
qt_interpreter_events::gui_preference_slot (const QString& key,
                                            const QString& value)
{
  QMutexLocker autolock (&m_mutex);

  resource_manager& rmgr = m_octave_qobj.get_resource_manager ();
  gui_settings *settings = rmgr.get_settings ();

  QString read_value = settings->value (key).toString ();

  // Some preferences need extra handling
  QString adjusted_value = gui_preference_adjust (key, value);

  if (! adjusted_value.isEmpty () && (read_value != adjusted_value))
    {
      // Change settings only for new, non-empty values
      settings->setValue (key, QVariant (adjusted_value));

      emit settings_changed (settings, true);   // true: changed by worker
    }

  m_result = read_value;

  wake_all ();
}

QString
qt_interpreter_events::gui_preference_adjust (const QString& key,
                                              const QString& value)
{
  // Immediately return if no new value is given.

  if (value.isEmpty ())
    return value;

  QString adjusted_value = value;

  // Not all encodings are available.  Encodings are uppercase and do
  // not use CPxxx but IBMxxx or WINDOWS-xxx.

  if (key == ed_default_enc.key)
    {
      adjusted_value = adjusted_value.toUpper ();

      QStringList codecs;
      resource_manager& rmgr = m_octave_qobj.get_resource_manager ();
      rmgr.get_codecs (&codecs);

      QRegExp re ("^CP(\\d+)$");

      if (adjusted_value == "SYSTEM")
        adjusted_value =
          QString ("SYSTEM (") +
          QString (octave_locale_charset_wrapper ()).toUpper () +
          QString (")");
      else if (re.indexIn (adjusted_value) > -1)
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

OCTAVE_END_NAMESPACE(octave)
