/****************************************************************************
**
** Copyright (C) 2009 Nokia Corporation and/or its subsidiary(-ies).
** All rights reserved.
** Contact: Nokia Corporation (qt-info@nokia.com)
**
** This file is part of the examples of the Qt Toolkit.
**
** $QT_BEGIN_LICENSE:LGPL$
** Commercial Usage
** Licensees holding valid Qt Commercial licenses may use this file in
** accordance with the Qt Commercial License Agreement provided with the
** Software or, alternatively, in accordance with the terms contained in
** a written agreement between you and Nokia.
**
** GNU Lesser General Public License Usage
** Alternatively, this file may be used under the terms of the GNU Lesser
** General Public License version 2.1 as published by the Free Software
** Foundation and appearing in the file LICENSE.LGPL included in the
** packaging of this file.  Please review the following information to
** ensure the GNU Lesser General Public License version 2.1 requirements
** will be met: http://www.gnu.org/licenses/old-licenses/lgpl-2.1.html.
**
** In addition, as a special exception, Nokia gives you certain additional
** rights.  These rights are described in the Nokia Qt LGPL Exception
** version 1.1, included in the file LGPL_EXCEPTION.txt in this package.
**
** GNU General Public License Usage
** Alternatively, this file may be used under the terms of the GNU
** General Public License version 3.0 as published by the Free Software
** Foundation and appearing in the file LICENSE.GPL included in the
** packaging of this file.  Please review the following information to
** ensure the GNU General Public License version 3.0 requirements will be
** met: http://www.gnu.org/copyleft/gpl.html.
**
** If you have questions regarding the use of this file, please contact
** Nokia at qt-info@nokia.com.
** $QT_END_LICENSE$
**
****************************************************************************/

#include <QtGui>
#include "find-dialog.h"

find_dialog::find_dialog (QsciScintilla* edit_area, QWidget *parent)
  : QDialog (parent)
{
  setWindowTitle ("Search file");
  _label = new QLabel (tr ("Find &what:"));
  _search_line_edit = new QLineEdit;
  _label->setBuddy (_search_line_edit);

  _case_check_box = new QCheckBox (tr ("Match &case"));
  _from_start_check_box = new QCheckBox (tr ("Search from &start"));
  _from_start_check_box->setChecked (true);

  _find_next_button = new QPushButton (tr ("&Find Next"));
  _find_next_button->setDefault (true);

  _more_button = new QPushButton (tr ("&More"));
  _more_button->setCheckable (true);
  _more_button->setAutoDefault (false);

  _button_box = new QDialogButtonBox (Qt::Vertical);
  _button_box->addButton (_find_next_button, QDialogButtonBox::ActionRole);
  _button_box->addButton (_more_button, QDialogButtonBox::ActionRole);

  _extension = new QWidget (this);
  _whole_words_check_box = new QCheckBox (tr ("&Whole words"));
  _regex_check_box = new QCheckBox (tr ("Regular E&xpressions"));
  _backward_check_box = new QCheckBox (tr ("Search &backward"));
  _search_selection_check_box = new QCheckBox (tr ("Search se&lection"));

  edit_area = edit_area;
  connect (_find_next_button, SIGNAL (clicked ()), this, SLOT (search_next ()));
  connect (_more_button, SIGNAL (toggled (bool)), _extension, SLOT (setVisible (bool)));

  QVBoxLayout *extension_layout = new QVBoxLayout ();
  extension_layout->setMargin (0);
  extension_layout->addWidget (_whole_words_check_box);
  extension_layout->addWidget (_regex_check_box);
  extension_layout->addWidget (_backward_check_box);
  extension_layout->addWidget (_search_selection_check_box);
  _extension->setLayout (extension_layout);

  QHBoxLayout *top_left_layout = new QHBoxLayout;
  top_left_layout->addWidget (_label);
  top_left_layout->addWidget (_search_line_edit);

  QVBoxLayout *left_layout = new QVBoxLayout;
  left_layout->addLayout (top_left_layout);
  left_layout->addWidget (_case_check_box);
  left_layout->addWidget (_from_start_check_box);
  left_layout->addStretch (1);

  QGridLayout *main_layout = new QGridLayout;
  main_layout->setSizeConstraint (QLayout::SetFixedSize);
  main_layout->addLayout (left_layout, 0, 0);
  main_layout->addWidget (_button_box, 0, 1);
  main_layout->addWidget (_extension, 1, 0, 1, 2);
  setLayout (main_layout);
  _extension->hide ();
}

void
find_dialog::search_next ()
{
  int line = -1, col = -1;
  if (_from_start_check_box->isChecked ())
    {
      line = 1;
      col  = 1;
    }

  if (_edit_area)
    {
      _edit_area->findFirst (_search_line_edit->text (),
                             _regex_check_box->isChecked (),
                             _case_check_box->isChecked (),
                             _whole_words_check_box->isChecked (),
                             true,
                             !_backward_check_box->isChecked (),
                             line,
                             col,
                             true,
                             true
                             );
    }
}
