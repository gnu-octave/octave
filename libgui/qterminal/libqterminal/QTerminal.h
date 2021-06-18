/*

Copyright (C) 2012-2019 Michael Goffioul.
Copyright (C) 2012-2019 Jacob Dawid.

This file is part of QTerminal.

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not,
see <https://www.gnu.org/licenses/>.

*/

#ifndef QTERMINAL_H
#define QTERMINAL_H

#include <QColor>
#include <QList>
#include <QPoint>
#include <QString>
#include <QWidget>

// For now, we need to use the following #include and using statement
// for the signal/slot macros.  Could maybe change later when using
// Qt5-style signal/slot connections.
#include "gui-settings.h"

using octave::gui_settings;

namespace octave
{
  class base_qobject;
}

class QMenu;
class QAction;

class QTerminal : public QWidget
{
  Q_OBJECT

public:

  static QTerminal *
  create (octave::base_qobject& oct_qobj, QWidget *xparent);

  virtual ~QTerminal (void) = default;

  virtual void setTerminalFont (const QFont& font) = 0;

  virtual void setSize (int h, int v) = 0;

  virtual void sendText (const QString& text) = 0;

  virtual QString selectedText () = 0;

  virtual void has_extra_interrupt (bool extra) = 0;

  virtual QList<QAction*> get_hotspot_actions (const QPoint&)
  { return QList<QAction*> (); }

  enum CursorType
  {
    IBeamCursor,
    BlockCursor,
    UnderlineCursor
  };

  virtual void setCursorType (CursorType type, bool blinking)
  {
    // Provide empty default impl in order to avoid conflicts with the
    // win impl.

    Q_UNUSED (type);
    Q_UNUSED (blinking);
  }

  virtual void setBackgroundColor (const QColor& color) = 0;

  virtual void setForegroundColor (const QColor& color) = 0;

  virtual void setSelectionColor (const QColor& color) = 0;

  virtual void setCursorColor (bool useForegroundColor,
                               const QColor& color) = 0;

  virtual void setScrollBufferSize(int value=1000) = 0;

signals:

  void report_status_message (const QString&);

  void interrupt_signal (void);

  void edit_mfile_request (const QString&, int);

  void show_doc_signal (const QString&);

  void execute_command_in_terminal_signal (const QString&);

  void request_edit_mfile_signal (const QString&, int);

  void request_open_file_signal (const QString&, const QString&, int);

  void set_screen_size_signal (int, int);

  void clear_command_window_request (void);

public slots:

  virtual void copyClipboard (void) = 0;

  virtual void pasteClipboard (void) = 0;

  virtual void selectAll (void) = 0;

  virtual void handleCustomContextMenuRequested (const QPoint& at);

  void notice_settings (const gui_settings *settings);

  virtual void init_terminal_size (void) { }

  void terminal_interrupt (void) { emit interrupt_signal (); }

  void run_selection (void);

  void edit_file (void);

  void edit_selected (void);

  void help_on_expression (void);

  void doc_on_expression (void);

  virtual void handle_visibility_changed (bool) { };

protected:

  QTerminal (octave::base_qobject& oct_qobj, QWidget *xparent = nullptr)
            : QWidget (xparent), m_octave_qobj (oct_qobj) { }

  void construct (octave::base_qobject& oct_qobj);

private:

  octave::base_qobject& m_octave_qobj;

  QMenu *_contextMenu;
  QAction * _copy_action;
  QAction * _paste_action;
  QAction * _selectall_action;
  QAction * _edit_action;
  QAction * _run_selection_action;
  QAction * m_edit_selected_action;
  QAction * m_help_selected_action;
  QAction * m_doc_selected_action;

  QAction *_interrupt_action;
  QAction *_nop_action;
};

#endif // QTERMINAL_H
