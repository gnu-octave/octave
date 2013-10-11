/*

Copyright (C) 2011-2012 Jacob Dawid

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

#if !defined (file_editor_tab_h)
#define file_editor_tab_h 1

#include <QWidget>
#include <QCloseEvent>
#include <QFileSystemWatcher>
#include <QSettings>
#include <QFileInfo>
#include <Qsci/qsciapis.h>

#include "find-dialog.h"
#include "octave-qscintilla.h"

class file_editor;

class file_editor_tab : public QWidget
{
  Q_OBJECT

public:

  file_editor_tab (const QString& directory = "");

  ~file_editor_tab (void);

public slots:

  void update_window_title (bool modified);
  void handle_copy_available (bool enableCopy);
  void handle_margin_clicked (int line, int margin,
                              Qt::KeyboardModifiers state);

  // Tells the editor tab to react on changed settings.
  void notice_settings (const QSettings *settings);

  // Will initiate close if associated with the identifier tag.
  void conditional_close (const QWidget *ID, bool app_closing = false);

  // Change to a different editor tab by identifier tag.
  void change_editor_state (const QWidget *ID);

  // Simply transmit file name.
  void file_name_query (const QWidget *ID);

  void set_focus (const QWidget *ID);
  void undo (const QWidget *ID);
  void redo (const QWidget *ID);
  void copy (const QWidget *ID);
  void cut (const QWidget *ID);
  void paste (const QWidget *ID);
  void context_help (const QWidget *ID, bool);
  void save_file (const QWidget *ID);
  void save_file (const QWidget *ID, const QString& fileName,
                  bool remove_on_success);
  void save_file_as (const QWidget *ID);
  void print_file (const QWidget *ID);
  void run_file (const QWidget *ID);
  void toggle_bookmark (const QWidget *ID);
  void next_bookmark (const QWidget *ID);
  void previous_bookmark (const QWidget *ID);
  void remove_bookmark (const QWidget *ID);

  void toggle_breakpoint (const QWidget *ID);
  void next_breakpoint (const QWidget *ID);
  void previous_breakpoint (const QWidget *ID);
  void remove_all_breakpoints (const QWidget *ID);

  void comment_selected_text (const QWidget *ID);
  void uncomment_selected_text (const QWidget *ID);
  void find (const QWidget *ID);
  void goto_line (const QWidget *ID, int line = -1);

  void insert_debugger_pointer (const QWidget *ID, int line = -1);
  void delete_debugger_pointer (const QWidget *ID, int line = -1);

  void do_breakpoint_marker (bool insert, const QWidget *ID, int line = -1);

  void set_modified (bool modified = true);

  QString load_file (const QString& fileName);
  void new_file (const QString& commands = QString ());

  void file_has_changed (const QString& fileName);

  void execute_command_in_terminal (const QString& command);

signals:

  void file_name_changed (const QString& fileName, const QString& toolTip);
  void editor_state_changed (bool copy_available, const QString& fileName);
  void tab_remove_request ();
  void add_filename_to_list (const QString&, QWidget *);
  void mru_add_file (const QString& file_name);
  void editor_check_conflict_save (const QString& saveFileName,
                                   bool remove_on_success);
  void run_file_signal (const QFileInfo& info);
  void execute_command_in_terminal_signal (const QString&);

protected:

  void closeEvent (QCloseEvent *event);
  void set_file_name (const QString& fileName);

private slots:

  // When user closes message box for reload question.
  void handle_file_reload_answer (int decision);

  // When user closes message box for resave question.
  void handle_file_resave_answer (int decision);

  // When user closes message box for modified question.
  void handle_file_modified_answer (int decision);

  // When user closes find_dialog box.
  void handle_find_dialog_finished (int decision);

  // When user closes QFileDialog box.
  void handle_save_file_as_answer (const QString& fileName);
  void handle_save_file_as_answer_close (const QString& fileName);
  void handle_save_file_as_answer_cancel ();

  // When apis preparation has finished and is ready to save
  void save_apis_info ();

  // When the numer of lines changes -> adapt width of margin
  void auto_margin_width ();

private:

  enum editor_markers
    {
      bookmark,
      breakpoint,
      debugger_position
    };

  struct bp_info
  {
    bp_info (const QString& f, const QString& d, const QString& fn, int l)
      : file (f.toStdString ()), dir (d.toStdString ()),
        function_name (fn.toStdString ()), line (l)
    { }

    std::string file;
    std::string dir;
    std::string function_name;
    int line;
  };

  void save_file (const QString& saveFileName, bool remove_on_success = false);
  void save_file_as (bool remove_on_success = false);
  void message_duplicate_file_name (const QString& fileName);

  void update_lexer ();
  void request_add_breakpoint (int line);
  void request_remove_breakpoint (int line);

  int check_file_modified ();
  void do_comment_selected_text (bool comment);
  QString comment_string (const QString&);

  void add_breakpoint_callback (const bp_info& info);
  void remove_breakpoint_callback (const bp_info& info);
  void remove_all_breakpoints_callback (const bp_info& info);
  void center_current_line ();

  octave_qscintilla *_edit_area;

  QString _file_name;
  QString _file_name_short;

  bool _long_title;
  bool _copy_available;
  bool _app_closing;

  QFileSystemWatcher _file_system_watcher;

  find_dialog *_find_dialog;
  bool _find_dialog_is_visible;
  QRect _find_dialog_geometry;

  QsciAPIs *_lexer_apis;
  QString _prep_apis_file;
};

#endif
