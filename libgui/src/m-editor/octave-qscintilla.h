/*

Copyright (C) 2013 Torsten <ttl@justmail.de>

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

#if !defined (octave_qscintilla_h)
#define octave_qscintilla_h 1

#include <Qsci/qsciscintilla.h>
#include <QMenu>
#include <QContextMenuEvent>

class octave_qscintilla : public QsciScintilla
{
  Q_OBJECT

public:

  octave_qscintilla (QWidget *p);
  ~octave_qscintilla ();

#ifdef HAVE_QSCI_VERSION_2_6_0
  virtual void contextMenuEvent (QContextMenuEvent *e);
#endif

signals:

  void execute_command_in_terminal_signal (const QString&);

private slots:

  void contextmenu_help (bool);

private:

  QString _word_at_cursor;

};

#endif
