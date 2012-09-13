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

#ifndef FILEEDITORINTERFACE_H
#define FILEEDITORINTERFACE_H

#include <QDockWidget>
#include <QMenu>
#include <QToolBar>

class QTerminal;
class main_window;

class file_editor_interface : public QDockWidget
{
  Q_OBJECT

  public:
  file_editor_interface (QTerminal *terminal, main_window *mainWindow)
    : QDockWidget ((QWidget*)mainWindow) // QDockWidget constructor is explicit, hence the cast.
  {
    setObjectName ("FileEditor");
    _terminal = terminal;
    _main_window = mainWindow;

    connect (this, SIGNAL (visibilityChanged (bool)), this,
             SLOT (handle_visibility_changed (bool)));
  }

  virtual ~file_editor_interface () { }

  virtual QMenu *debug_menu () = 0;
  virtual QToolBar *toolbar () = 0;

  virtual void handle_entered_debug_mode () = 0;
  virtual void handle_quit_debug_mode () = 0;

public slots:
  virtual void request_new_file () = 0;
  virtual void request_open_file () = 0;
  virtual void request_open_file (const QString& fileName, bool silent = false) = 0;

signals:
  void active_changed (bool active);

protected:
  QTerminal* _terminal;
  main_window* _main_window;

  void closeEvent (QCloseEvent *e)
  {
    emit active_changed (false);
    QDockWidget::closeEvent (e);
  }

protected slots:
  void handle_visibility_changed (bool visible)
  {
    if (visible)
      emit active_changed (true);
  }
};

#endif // FILEEDITORINTERFACE_H
