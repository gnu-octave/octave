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

#if ! defined (octave_qt_interpreter_events_h)
#define octave_qt_interpreter_events_h 1

#include <list>
#include <string>

#include <QList>
#include <QMutex>
#include <QObject>
#include <QString>
#include <QWaitCondition>

#include "dialog.h"
#include "gui-settings.h"

#include "event-manager.h"

// Defined for purposes of sending QList<int> as part of signal.
typedef QList<int> QIntList;

class octave_value;

OCTAVE_BEGIN_NAMESPACE(octave)

class base_qobject;

// The functions in this class are not normally called directly, but
// are invoked from the Octave interpreter thead by methods in the
// event_manager class.  In most cases, they should only translate
// data from the types typically used in the interpreter to whatever
// is required by the GUI (for example, std::string to QString) and
// emit a Qt signal.
//
// The use of Qt signals provides a thread-safe way for the Octave
// interpreter to notify the GUI of events (directory or workspace has
// changed, for example) or to request that the GUI perform actions
// (display a dialog, for example).
//
// By using this class as a wrapper around the Qt signals, we maintain
// a separation between the Octave interpreter and any specific GUI
// toolkit (no Qt headers are used in the Octave interpreter sources).

class qt_interpreter_events : public QObject, public interpreter_events
{
  Q_OBJECT

public:

  qt_interpreter_events (base_qobject& oct_qobj);

  // No copying!

  qt_interpreter_events (const qt_interpreter_events&) = delete;

  qt_interpreter_events& operator = (const qt_interpreter_events&) = delete;

  ~qt_interpreter_events (void) = default;

  // Note: these functions currently do nothing with the old terminal
  // widget.
  void start_gui (bool gui_app = false);
  void close_gui (void);

  bool have_dialogs (void) const { return true; }

  std::list<std::string>
  file_dialog (const filter_list& filter, const std::string& title,
               const std::string& filename, const std::string& pathname,
               const std::string& multimode);

  std::list<std::string>
  input_dialog (const std::list<std::string>& prompt,
                const std::string& title, const std::list<float>& nr,
                const std::list<float>& nc,
                const std::list<std::string>& defaults);

  std::pair<std::list<int>, int>
  list_dialog (const std::list<std::string>& list,
               const std::string& mode, int width, int height,
               const std::list<int>& initial_value,
               const std::string& name,
               const std::list<std::string>& prompt,
               const std::string& ok_string,
               const std::string& cancel_string);

  std::string
  question_dialog (const std::string& msg, const std::string& title,
                   const std::string& btn1, const std::string& btn2,
                   const std::string& btn3, const std::string& btndef);

  void update_path_dialog (void);

  void show_preferences (void);

  void apply_preferences (void);

  void show_terminal_window (void);

  bool show_documentation (const std::string& file);

  void show_file_browser (void);

  void show_command_history (void);

  void show_workspace (void);

  void show_community_news (int serial);
  void show_release_notes (void);

  bool edit_file (const std::string& file);

  void edit_variable (const std::string& name, const octave_value& val);

  bool confirm_shutdown (void);

  bool prompt_new_edit_file (const std::string& file);

  int debug_cd_or_addpath_error (const std::string& file,
                                 const std::string& dir,
                                 bool addpath_option);

  uint8NDArray get_named_icon (const std::string& icon_name);

  std::string gui_preference (const std::string& key,
                              const std::string& value);

  bool copy_image_to_clipboard (const std::string& file);

  void focus_window (const std::string win_name);

  void execute_command_in_terminal (const std::string& command);

  void register_documentation (const std::string& file);

  void unregister_documentation (const std::string& file);

  // Note: this function currently does nothing with the old terminal
  // widget.
  void interpreter_output (const std::string& msg);

  void display_exception (const execution_exception& ee, bool beep);

  void gui_status_update (const std::string& feature, const std::string& status);

  void update_gui_lexer (void);

  void directory_changed (const std::string& dir);

  void file_remove (const std::string& old_name,
                    const std::string& new_name);

  void file_renamed (bool load_new = true);

  void set_workspace (bool top_level, bool debug,
                      const symbol_info_list& syminfo,
                      bool update_variable_editor);

  void clear_workspace (void);

  void update_prompt (const std::string& prompt);

  void set_history (const string_vector& hist);

  void append_history (const std::string& hist_entry);

  void clear_history (void);

  void pre_input_event (void);

  void post_input_event (void);

  void enter_debugger_event (const std::string& fcn_name,
                             const std::string& fcn_file_name, int line);

  void execute_in_debugger_event (const std::string& file, int line);

  void exit_debugger_event (void);

  void update_breakpoint (bool insert, const std::string& file, int line,
                          const std::string& cond);

  void lock (void) { m_mutex.lock (); }

  void wait (void) { m_waitcondition.wait (&m_mutex); }

  void unlock (void) { m_mutex.unlock (); }

  void wake_all (void) { m_waitcondition.wakeAll (); }

public slots:

  void confirm_shutdown_octave (void);

  void get_named_icon_slot (const QString& name);

  void gui_preference_slot (const QString& key, const QString& value);

signals:

  // Note: these signals are not currently used by the old terminal widget.
  void start_gui_signal (bool gui_app);
  void close_gui_signal (void);

  void copy_image_to_clipboard_signal (const QString& file, bool remove_file);

  void focus_window_signal (const QString& win_name);

  void edit_file_signal (const QString& file);

  void directory_changed_signal (const QString& dir);

  void update_path_dialog_signal (void);

  void file_remove_signal (const QString& old_name, const QString& new_name);

  void file_renamed_signal (bool load_new);

  void execute_command_in_terminal_signal (const QString& command);

  void set_workspace_signal (bool top_level, bool debug,
                             const symbol_info_list& syminfo);

  void clear_workspace_signal (void);

  void update_prompt_signal (const QString& prompt);

  void set_history_signal (const QStringList& hist);

  void append_history_signal (const QString& hist_entry);

  void clear_history_signal (void);

  void enter_debugger_signal (void);

  void exit_debugger_signal (void);

  void update_breakpoint_marker_signal (bool insert, const QString& file,
                                        int line, const QString& cond);

  void insert_debugger_pointer_signal (const QString&, int);

  void delete_debugger_pointer_signal (const QString&, int);

  void show_preferences_signal (void);

  void gui_preference_signal (const QString& key, const QString& value);

  void show_terminal_window_signal (void);

  void show_documentation_signal (const QString& file);

  void register_documentation_signal (const QString& file);

  void unregister_documentation_signal (const QString& file);

  void show_file_browser_signal (void);

  void show_command_history_signal (void);

  void show_workspace_signal (void);

  void show_community_news_signal (int serial);
  void show_release_notes_signal (void);

  // Note: the next two signals are currently not used by the old terminal widget.
  void interpreter_output_signal (const QString& msg);
  void new_command_line_signal (const QString& msg = QString ());

  void gui_status_update_signal (const QString& feature, const QString& status);

  void update_gui_lexer_signal (bool update_apis_only);

  void edit_variable_signal (const QString& name, const octave_value& val);

  void refresh_variable_editor_signal (void);

  void confirm_shutdown_signal (void);

  void get_named_icon_signal (const QString& name);

  void settings_changed (const gui_settings *, bool);

  void apply_new_settings (void);

private:

  QString gui_preference_adjust (const QString& key, const QString& value);

  void insert_debugger_pointer (const std::string& file, int line);

  void delete_debugger_pointer (const std::string& file, int line);

  base_qobject& m_octave_qobj;

  QUIWidgetCreator m_uiwidget_creator;

  QVariant m_result;

  QMutex m_mutex;

  QWaitCondition m_waitcondition;
};

OCTAVE_END_NAMESPACE(octave)

#endif
