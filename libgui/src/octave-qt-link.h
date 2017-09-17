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

#if ! defined (octave_qt_link_h)
#define octave_qt_link_h 1

#include <list>
#include <string>

#include <QList>
#include <QObject>
#include <QString>
#include <QMutex>
#include <QWaitCondition>

#include "octave-gui.h"
#include "octave-link.h"

// Defined for purposes of sending QList<int> as part of signal.
typedef QList<int> QIntList;

class octave_value;

namespace octave
{

  //! Provides threadsafe access to octave.
  //! @author Jacob Dawid
  //!
  //! This class is a wrapper around octave and provides thread safety by
  //! buffering access operations to octave and executing them in the
  //! readline event hook, which lives in the octave thread.

  class octave_qt_link : public QObject, public octave_link
  {
    Q_OBJECT

  public:

    octave_qt_link (QWidget *p, gui_application *app_context);

    // No copying!

    octave_qt_link (const octave_qt_link&) = delete;

    octave_qt_link& operator = (const octave_qt_link&) = delete;

    ~octave_qt_link (void) = default;

    bool do_confirm_shutdown (void);

    bool do_copy_image_to_clipboard (const std::string& file);

    bool do_edit_file (const std::string& file);
    bool do_prompt_new_edit_file (const std::string& file);

    int do_message_dialog (const std::string& dlg, const std::string& msg,
                           const std::string& title);

    std::string
    do_question_dialog (const std::string& msg, const std::string& title,
                        const std::string& btn1, const std::string& btn2,
                        const std::string& btn3, const std::string& btndef);

    std::pair<std::list<int>, int>
    do_list_dialog (const std::list<std::string>& list,
                    const std::string& mode,
                    int width, int height,
                    const std::list<int>& initial_value,
                    const std::string& name,
                    const std::list<std::string>& prompt,
                    const std::string& ok_string,
                    const std::string& cancel_string);

    std::list<std::string>
    do_input_dialog (const std::list<std::string>& prompt,
                     const std::string& title,
                     const std::list<float>& nr,
                     const std::list<float>& nc,
                     const std::list<std::string>& defaults);

    std::list<std::string>
    do_file_dialog (const filter_list& filter, const std::string& title,
                    const std::string& filename, const std::string& pathname,
                    const std::string& multimode);

    int
    do_debug_cd_or_addpath_error (const std::string& file,
                                  const std::string& dir,
                                  bool addpath_option);

    void do_change_directory (const std::string& dir);

    void do_file_remove (const std::string& old_name,
                         const std::string& new_name);
    void do_file_renamed (bool load_new = true);

    void do_execute_command_in_terminal (const std::string& command);

    void do_set_workspace (bool top_level, bool debug,
                           const symbol_scope& scope,
                           bool update_variable_editor);

    void do_clear_workspace (void);

    void do_set_history (const string_vector& hist);
    void do_append_history (const std::string& hist_entry);
    void do_clear_history (void);

    void do_pre_input_event (void);
    void do_post_input_event (void);

    void do_enter_debugger_event (const std::string& file, int line);
    void do_execute_in_debugger_event (const std::string& file, int line);
    void do_exit_debugger_event (void);

    void do_update_breakpoint (bool insert, const std::string& file, int line,
                               const std::string& cond);

    static bool file_in_path (const std::string& file, const std::string& dir);

    void do_show_preferences (void);

    std::string do_gui_preference (const std::string& key,
                                   const std::string& value);
    void do_show_doc (const std::string& file);
    void do_register_doc (const std::string& file);
    void do_unregister_doc (const std::string& file);

    void do_edit_variable (const std::string& name, const octave_value& val);

    void shutdown_confirmation (bool sd) { m_shutdown_confirm_result = sd; }

    void lock (void) { m_mutex.lock (); }
    void wait (void) { m_waitcondition.wait (&m_mutex); }
    void unlock (void) { m_mutex.unlock (); }
    void wake_all (void) { m_waitcondition.wakeAll (); }

  private:

    void do_insert_debugger_pointer (const std::string& file, int line);
    void do_delete_debugger_pointer (const std::string& file, int line);

    gui_application *m_app_context;

    bool m_shutdown_confirm_result;

    QMutex m_mutex;
    QWaitCondition m_waitcondition;

  signals:

    void copy_image_to_clipboard_signal (const QString& file, bool remove_file);

    void edit_file_signal (const QString& file);

    void change_directory_signal (const QString& dir);

    void file_remove_signal (const QString& old_name, const QString& new_name);
    void file_renamed_signal (bool load_new);

    void execute_command_in_terminal_signal (const QString& command);

    void set_workspace_signal (bool top_level, bool debug,
                               const symbol_scope& scope);

    void clear_workspace_signal (void);

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

    void gui_preference_signal (const QString&, const QString&, QString*);

    void show_doc_signal (const QString& file);

    void register_doc_signal (const QString& file);

    void unregister_doc_signal (const QString& file);

    void edit_variable_signal (const QString& name, const octave_value& val);

    void refresh_variable_editor_signal (void);

    void confirm_shutdown_signal (void);
  };
}

#endif
