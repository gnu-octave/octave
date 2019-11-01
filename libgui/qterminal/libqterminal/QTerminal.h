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

#include <QKeySequence>
#include <QWidget>
#include <QStringList>
#include <QColor>
#include <QList>
#include <QMenu>
#include <QClipboard>
#include <QApplication>
#include <QAction>

#include "gui-settings.h"
#include "resource-manager.h"

using octave::gui_settings;

class QTerminal : public QWidget
{
  Q_OBJECT

public:

  static QTerminal *create (QWidget *xparent = nullptr);

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

public slots:

  virtual void copyClipboard (void) = 0;

  virtual void pasteClipboard (void) = 0;

  virtual void selectAll (void) = 0;

  virtual void handleCustomContextMenuRequested (const QPoint& at);

  void notice_settings (const gui_settings *settings);

  virtual void init_terminal_size (void) { }

  void terminal_interrupt (void) { emit interrupt_signal (); }

  void set_global_shortcuts (bool focus_out);

  void run_selection (void);

  void edit_file (void);

  void edit_selected (void);

  void help_on_expression (void);

  void doc_on_expression (void);

  virtual void handle_visibility_changed (bool) { };

protected:

  QTerminal (QWidget *xparent = nullptr) : QWidget (xparent)
  {
    octave::resource_manager& rmgr
      = octave::__get_resource_manager__ ("QTerminal::QTerminal");

    // context menu
    setContextMenuPolicy (Qt::CustomContextMenu);

    _contextMenu = new QMenu (this);

    _copy_action
      = _contextMenu->addAction (rmgr.icon ("edit-copy"), tr ("Copy"), this,
                                 SLOT (copyClipboard ()));

    _paste_action
      = _contextMenu->addAction (rmgr.icon ("edit-paste"), tr ("Paste"), this,
                                 SLOT (pasteClipboard ()));

    _contextMenu->addSeparator ();

    _selectall_action
      = _contextMenu->addAction (tr ("Select All"), this, SLOT (selectAll ()));

    _run_selection_action
      = _contextMenu->addAction (tr ("Run Selection"), this,
                                 SLOT (run_selection ()));

    m_edit_selected_action
      = _contextMenu->addAction (tr ("Edit selection"), this,
                                 SLOT (edit_selected ()));
    m_help_selected_action
      = _contextMenu->addAction (tr ("Help on selection"), this,
                                 SLOT (help_on_expression ()));
    m_doc_selected_action
      = _contextMenu->addAction (tr ("Documentation on selection"), this,
                                 SLOT (doc_on_expression ()));

    _edit_action = _contextMenu->addAction (tr (""), this, SLOT (edit_file ()));

    _contextMenu->addSeparator ();

    _contextMenu->addAction (tr ("Clear Window"), parent (),
                             SLOT (handle_clear_command_window_request ()));

    connect (this, SIGNAL (customContextMenuRequested (QPoint)),
             this, SLOT (handleCustomContextMenuRequested (QPoint)));

    connect (this, SIGNAL (report_status_message (const QString&)),
             xparent, SLOT (report_status_message (const QString&)));

    connect (this, SIGNAL (show_doc_signal (const QString&)),
             xparent, SLOT (handle_show_doc (const QString&)));

    connect (this, SIGNAL (edit_mfile_request (const QString&, int)),
             xparent, SLOT (edit_mfile (const QString&, int)));

    connect (this, SIGNAL (execute_command_in_terminal_signal (const QString&)),
             xparent, SLOT (execute_command_in_terminal (const QString&)));

    connect (xparent, SIGNAL (settings_changed (const gui_settings *)),
             this, SLOT (notice_settings (const gui_settings *)));

    connect (xparent, SIGNAL (init_terminal_size_signal ()),
             this, SLOT (init_terminal_size ()));

    connect (xparent, SIGNAL (copyClipboard_signal ()),
             this, SLOT (copyClipboard ()));

    connect (xparent, SIGNAL (pasteClipboard_signal ()),
             this, SLOT (pasteClipboard ()));

    connect (xparent, SIGNAL (selectAll_signal ()),
             this, SLOT (selectAll ()));

    // extra interrupt action
    _interrupt_action = new QAction (this);
    addAction (_interrupt_action);

    _interrupt_action->setShortcut
      (QKeySequence (Qt::ControlModifier + Qt::Key_C));

    connect (_interrupt_action, SIGNAL (triggered ()),
             this, SLOT (terminal_interrupt ()));

    // dummy (nop) action catching Ctrl-D in terminal, no connection
    _nop_action = new QAction (this);
    addAction (_nop_action);

    _nop_action->setShortcut (QKeySequence (Qt::ControlModifier + Qt::Key_D));
  }

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
