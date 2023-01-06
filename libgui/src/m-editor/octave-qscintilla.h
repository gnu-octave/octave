////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2013-2023 The Octave Project Developers
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

#if ! defined (octave_octave_qscintilla_h)
#define octave_octave_qscintilla_h 1

#include <QContextMenuEvent>
#include <QKeyEvent>
#include <QLabel>
#include <QMenu>
#include <Qsci/qsciscintilla.h>
#include <QTemporaryFile>

#include "gui-settings.h"
#include "qt-interpreter-events.h"

OCTAVE_BEGIN_NAMESPACE(octave)

class base_qobject;

class octave_qscintilla : public QsciScintilla
{
  Q_OBJECT

public:

  octave_qscintilla (QWidget *p, base_qobject& oct_qobj);

  ~octave_qscintilla (void) = default;

  enum
    {
      ST_NONE = 0,
      ST_LINE_COMMENT,
      ST_BLOCK_COMMENT
    };

  virtual void contextMenuEvent (QContextMenuEvent *e);
  virtual void setCursorPosition (int line, int col);

  void context_help_doc (bool);
  void context_edit (void);
  void context_run (void);
  void get_global_textcursor_pos (QPoint *global_pos, QPoint *local_pos);
  bool get_actual_word (void);
  void clear_selection_markers (void);
  QString eol_string (void);
  void get_current_position (int *pos, int *line, int *col);
  QStringList comment_string (bool comment = true);
  int get_style (int pos = -1);
  int is_style_comment (int pos = -1);
  void smart_indent (bool do_smart_indent, int do_auto_close,
                     int line, int ind_char_width);

  void smart_indent_line_or_selected_text (int lineFrom, int lineTo);

  void set_word_selection (const QString& word = QString ());

  void show_selection_markers (int l1, int c1, int l2, int c2);

  void set_selection_marker_color (const QColor& c);

  void replace_all (const QString& o_str, const QString& n_str,
                    bool re, bool cs, bool wo);

signals:

  void update_rowcol_indicator_signal (int line, int col);
  void execute_command_in_terminal_signal (const QString&);
  void create_context_menu_signal (QMenu *);
  void context_menu_edit_signal (const QString&);
  void qsci_has_focus_signal (bool);
  void status_update (bool, bool);
  void show_doc_signal (const QString&);
  void context_menu_break_condition_signal (int);
  void context_menu_break_once (int);
  void ctx_menu_run_finished_signal (bool, int, QTemporaryFile *,
                                     QTemporaryFile *, bool, bool);
  void focus_console_after_command_signal (void);

  void interpreter_event (const fcn_callback& fcn);
  void interpreter_event (const meth_callback& meth);

public slots:

  void handle_enter_debug_mode (void);
  void handle_exit_debug_mode (void);

private slots:

  void ctx_menu_run_finished (bool, int, QTemporaryFile *, QTemporaryFile *,
                              bool, bool);

  void contextmenu_help (bool);
  void contextmenu_doc (bool);
  void contextmenu_help_doc (bool);
  void contextmenu_edit (bool);
  void contextmenu_run (bool);
  void contextmenu_run_temp_error (void);

  void contextmenu_break_condition (bool);
  void contextmenu_break_once (const QPoint&);

  void text_changed (void);
  void cursor_position_changed (int, int);

protected:

  void focusInEvent (QFocusEvent *focusEvent);

  void show_replace_action_tooltip (void);

  bool event (QEvent *e);

  void keyPressEvent (QKeyEvent *e);

  void dragEnterEvent (QDragEnterEvent *e);

private:

  void auto_close (int auto_endif, int l,
                   const QString& line, QString& first_word);

  base_qobject& m_octave_qobj;

  bool m_debug_mode;

  QString m_word_at_cursor;

  QString m_selection;
  QString m_selection_replacement;
  int m_selection_line;
  int m_selection_col;
  int m_indicator_id;
};

OCTAVE_END_NAMESPACE(octave)

#endif
