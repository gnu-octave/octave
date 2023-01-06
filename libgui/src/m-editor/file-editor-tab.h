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

#if ! defined (octave_file_editor_tab_h)
#define octave_file_editor_tab_h 1

#include <QAbstractButton>
#include <QCloseEvent>
#include <QDateTime>
#include <QFileInfo>
#include <QFileSystemWatcher>
#include <QLabel>
#include <QStatusBar>
#include <QWidget>
#include <Qsci/qsciapis.h>

#include "gui-settings.h"
#include "marker.h"
#include "octave-qscintilla.h"
#include "qt-interpreter-events.h"

class octave_value_list;

OCTAVE_BEGIN_NAMESPACE(octave)

class base_qobject;
class file_editor;

class file_editor_tab : public QWidget
{
  Q_OBJECT

public:

  file_editor_tab (base_qobject& oct_qobj, const QString& directory = "");

  ~file_editor_tab (void);

  octave_qscintilla * qsci_edit_area (void) { return m_edit_area; }

  // Will initiate close if associated with the identifier tag.
  bool conditional_close (void);

  void update_breakpoints ();
  void set_file_name (const QString& fileName);
  void enable_file_watcher (bool do_enable);

  QString file_name (void) const { return m_file_name; }
  QString encoding (void) const { return m_encoding; }

signals:

  void tab_ready_to_close (void);
  void file_name_changed (const QString& fileName,
                          const QString& toolTip,
                          bool modified);
  void editor_state_changed (bool copy_available, bool is_octave_file,
                             bool is_modified);
  void set_focus_editor_signal (QWidget *);
  void edit_area_changed (octave_qscintilla *edit_area);
  void tab_remove_request (void);
  void mru_add_file (const QString& file_name, const QString& encoding);
  void editor_check_conflict_save (const QString& saveFileName,
                                   bool remove_on_success);
  void run_file_signal (const QFileInfo& info);
  void request_open_file (const QString&, const QString& = QString ());
  void edit_mfile_request (const QString&, const QString&,
                           const QString&, int);

  void autoc_closed (void);

  void update_breakpoints_signal (const octave_value_list& args);

  void remove_breakpoint_via_debugger_linenr (int debugger_linenr);
  void request_remove_breakpoint_via_editor_linenr (int editor_linenr);
  void remove_all_breakpoints_signal (void);
  void find_translated_line_number (int original_linenr,
                                    int& translated_linenr, marker*&);
  void find_linenr_just_before (int linenr, int& original_linenr,
                                int& editor_linenr);
  void report_marker_linenr (QIntList& lines, QStringList& conditions);
  void remove_position_via_debugger_linenr (int debugger_linenr);
  void remove_all_positions (void);

  void debug_quit_signal (void);

  void interpreter_event (const fcn_callback& fcn);
  void interpreter_event (const meth_callback& meth);

  void maybe_remove_next (int remove_line);

  void dbstop_if (const QString& prompt, int line, const QString& cond);
  void request_add_breakpoint (int line, const QString& cond);
  void request_add_octave_apis (const QStringList&);
  void api_entries_added (void);

  void do_save_file_signal (const QString& file_to_save,
                            bool remove_on_success, bool restore_breakpoints);

  void confirm_dbquit_and_save_signal (const QString& file_to_save,
                                       const QString& base_name,
                                       bool remove_on_success,
                                       bool restore_breakpoints);

  // FIXME: The following is similar to "process_octave_code" signal.
  // However, currently that signal is connected to something that simply
  // focuses a window and does not actually communicate with Octave.
  //
  // void evaluate_octave_command (const QString& command);

public slots:

  void update_window_title (bool modified);
  void handle_copy_available (bool enableCopy);
  void handle_margin_clicked (int line, int margin,
                              Qt::KeyboardModifiers state);

  // Tells the editor tab to react on changed settings.
  void notice_settings (const gui_settings *settings, bool init = false);

  // Change to a different editor tab by identifier tag.
  void change_editor_state (const QWidget *ID);

  void set_focus (const QWidget *ID);
  void set_current_directory (const QString& dir);
  void context_help (const QWidget *ID, bool);
  void context_edit (const QWidget *ID);
  void save_file (const QWidget *ID);
  void save_file (const QWidget *ID, const QString& fileName,
                  bool remove_on_success);
  void save_file_as (const QWidget *ID);
  void print_file (const QWidget *ID);
  void run_file (const QWidget *ID, bool step_into = false);
  void context_run (const QWidget *ID);
  void toggle_bookmark (const QWidget *ID);
  void next_bookmark (const QWidget *ID);
  void previous_bookmark (const QWidget *ID);
  void remove_bookmark (const QWidget *ID);

  void toggle_breakpoint (const QWidget *ID);
  void next_breakpoint (const QWidget *ID);
  void previous_breakpoint (const QWidget *ID);
  void remove_all_breakpoints (const QWidget *ID);

  void scintilla_command (const QWidget *, unsigned int);

  void comment_selected_text (const QWidget *ID, bool input_str);
  void uncomment_selected_text (const QWidget *ID);

  void indent_selected_text (const QWidget *ID);
  void unindent_selected_text (const QWidget *ID);
  void smart_indent_line_or_selected_text (const QWidget *ID);
  void convert_eol (const QWidget *ID, QsciScintilla::EolMode);

  void zoom_in (const QWidget *ID);
  void zoom_out (const QWidget *ID);
  void zoom_normal (const QWidget *ID);

  void goto_line (const QWidget *ID, int line = -1);
  void move_match_brace (const QWidget *ID, bool select);
  void show_auto_completion (const QWidget *ID);

  void insert_debugger_pointer (const QWidget *ID, int line = -1);
  void delete_debugger_pointer (const QWidget *ID, int line = -1);

  void do_breakpoint_marker (bool insert, const QWidget *ID, int line = -1,
                             const QString& cond = "");

  void recover_from_exit (void);
  void set_modified (bool modified = true);

  void set_encoding (const QString& new_encoding);

  QString load_file (const QString& fileName);

  void new_file (const QString& commands = QString ());

  void file_has_changed (const QString& path, bool do_close = false);

  void handle_context_menu_edit (const QString&);
  void handle_context_menu_break_condition (int linenr);

  void handle_request_add_breakpoint (int line, const QString& cond);
  void handle_request_remove_breakpoint (int line);

  void update_breakpoints_handler (const octave_value_list& argout);
  void update_rowcol_indicator (int line, int col);
  void update_lexer_settings (bool update_apis_only = false);

private slots:

  // When user closes message box for decoding problems
  void handle_decode_warning_answer (QAbstractButton *btn);

  // When user closes message box for reload question.
  void handle_file_reload_answer (int decision);

  // When user closes message box for resave question.
  void handle_file_resave_answer (int decision);

  // When user closes QFileDialog box.
  void handle_save_file_as_answer (const QString& fileName);
  void handle_save_file_as_answer_close (const QString& fileName);
  void handle_save_file_as_answer_cancel (void);
  void handle_save_as_filter_selected (const QString& filter);

  // When user changes encoding after decoding errors were found
  void handle_current_enc_changed (const QString& enc);

  // When apis preparation has finished and is ready to save
  void save_apis_info (void);

  // When the numer of lines changes -> adapt width of margin
  void auto_margin_width (void);

  void handle_cursor_moved (int line, int col);
  void handle_char_added (int character);
  void handle_double_click (int p, int l, int modifier);
  void handle_lines_changed (void);

  void handle_remove_next (int remove_line);
  void handle_dbstop_if (const QString& prompt, int line,
                         const QString& cond);
  void handle_add_octave_apis (const QStringList& api_entries);
  void handle_api_entries_added (void);

  void do_save_file (const QString& file_to_save, bool remove_on_success,
                     bool restore_breakpoints);

  void confirm_dbquit_and_save (const QString& file_to_save,
                                const QString& base_name,
                                bool remove_on_success,
                                bool restore_breakpoints);

protected:

  void closeEvent (QCloseEvent *event);

private:

  base_qobject& m_octave_qobj;

  void add_breakpoint_event (int line, const QString& cond);

  bool valid_file_name (const QString& file = QString ());
  void save_file (const QString& saveFileName, bool remove_on_success = false,
                  bool restore_breakpoints = true);
  void save_file_as (bool remove_on_success = false);
  bool check_valid_identifier (QString file_name);
  QTextCodec * check_valid_codec (void);

  bool unchanged_or_saved (void);

  void update_lexer (void);

  void show_dialog (QDialog *dlg, bool modal);

public:

  int check_file_modified (bool remove = false);
  QString get_all_bookmarks (void);

private:
  void do_comment_selected_text (bool comment, bool input_str = false);
  void do_indent_selected_text (bool indent);
  void do_smart_indent_line_or_selected_text (void);

  void check_restore_breakpoints (void);
  void center_current_line (bool always=true);

  QString get_function_name (void);

  QsciScintilla::EolMode detect_eol_mode (void);
  void update_eol_indicator (void);

  octave_qscintilla *m_edit_area;

  QStatusBar *m_status_bar;
  QLabel *m_row_indicator;
  QLabel *m_col_indicator;
  QLabel *m_eol_indicator;
  QLabel *m_enc_indicator;

  QsciScintilla::EolMode m_save_as_desired_eol;

  QString m_file_name;
  QString m_file_name_short;
  QString m_ced;
  QString m_encoding;
  QString m_new_encoding;
  QDateTime m_last_modified;

  bool m_autoc_active;
  bool m_copy_available;
  bool m_is_octave_file;
  bool m_always_reload_changed_files;
  bool m_smart_indent;
  int m_auto_endif;
  int m_ind_char_width;

  QFileSystemWatcher m_file_system_watcher;

  QIntList m_bp_lines;
  QStringList m_bp_conditions;

  QsciAPIs *m_lexer_apis;
  QString m_prep_apis_path;
  QString m_prep_apis_file;

  int m_line_break;
  bool m_line_break_comments;
  int m_line;
  int m_col;
  bool m_lines_changed;
  bool m_highlight_all_occurrences;
  int m_bp_restore_count;

  struct breakpoint_info
  {
    bool remove_next;
    int remove_line;
    int do_not_remove_line;
  };

  breakpoint_info m_breakpoint_info;
};

OCTAVE_END_NAMESPACE(octave)

#endif
