/*

Copyright (C) 2013 John W. Eaton
Copyright (C) 2011-2013 Jacob Dawid

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

#ifndef WELCOMEWIZARD_H
#define WELCOMEWIZARD_H

#include <QDialog>

class welcome_wizard : public QDialog
{
  Q_OBJECT

public:

  typedef QWidget *(*page_creator_fptr) (welcome_wizard *wizard);

  welcome_wizard (QWidget *parent = 0);

  ~welcome_wizard (void) { }

private:

  QList<page_creator_fptr> page_ctor_list;
  QList<page_creator_fptr>::iterator page_list_iterator;
  QWidget *current_page;
  bool allow_web_connect_state;

private slots:

  void handle_web_connect_option (int state);

  void show_page (void);
  void previous_page (void);
  void next_page (void);

  void accept (void);
};

#endif // WELCOMEWIZARD_H
