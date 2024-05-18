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

#include "octave-config.h"

#include <QColor>
#include <QList>
#include <QPoint>
#include <QString>
#include <QWidget>

#include "qt-interpreter-events.h"

class QMenu;
class QAction;

class QTerminal : public QWidget
{
  Q_OBJECT

public:

  static QTerminal * create (QWidget *xparent);

  virtual ~QTerminal () = default;

  virtual void setTerminalFont (const QFont& font) = 0;

  virtual void setSize (int h, int v) = 0;

  virtual void sendText (const QString& text) = 0;

  virtual QString selectedText () = 0;

  virtual void has_extra_interrupt (bool extra) = 0;

  virtual QList<QAction *> get_hotspot_actions (const QPoint&)
  { return QList<QAction *> (); }

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

Q_SIGNALS:

  void report_status_message (const QString&);

  void interrupt_signal ();

  void edit_mfile_request (const QString&, int);

  void show_doc_signal (const QString&);

  void execute_command_in_terminal_signal (const QString&);

  void request_edit_mfile_signal (const QString&, int);

  void request_open_file_signal (const QString&, const QString&, int);

  void set_screen_size_signal (int, int);

  void clear_command_window_request ();

  void interpreter_event (const octave::fcn_callback& fcn);
  void interpreter_event (const octave::meth_callback& meth);

public Q_SLOTS:

  virtual void copyClipboard () = 0;

  virtual void pasteClipboard () = 0;

  virtual void selectAll () = 0;

  virtual void handleCustomContextMenuRequested (const QPoint& at);

  void notice_settings ();

  virtual void init_terminal_size () { }

  void terminal_interrupt () { Q_EMIT interrupt_signal (); }

  void run_selection ();

  void edit_file ();

  void edit_selected ();

  void help_on_expression ();

  void doc_on_expression ();

  virtual void handle_visibility_changed (bool) { };

protected:

  QTerminal (QWidget *xparent = nullptr)
    : QWidget (xparent)
  { }

  void construct ();

private:

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
