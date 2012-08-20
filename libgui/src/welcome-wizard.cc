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

#include "welcome-wizard.h"
#include "ui-welcome-wizard.h"

welcome_wizard::welcome_wizard (QWidget *parent) :
  QDialog (parent),
  _ui (new Ui::welcome_wizard)
{
  _ui->setupUi (this);
  connect (_ui->nextButton1, SIGNAL (clicked ()), this, SLOT (next ()));
  connect (_ui->nextButton2, SIGNAL (clicked ()), this, SLOT (next ()));
  connect (_ui->nextButton3, SIGNAL (clicked ()), this, SLOT (next ()));
  connect (_ui->nextButton4, SIGNAL (clicked ()), this, SLOT (next ()));

  connect (_ui->previousButton2, SIGNAL (clicked ()), this, SLOT (previous ()));
  connect (_ui->previousButton3, SIGNAL (clicked ()), this, SLOT (previous ()));
  connect (_ui->previousButton4, SIGNAL (clicked ()), this, SLOT (previous ()));
  connect (_ui->previousButton5, SIGNAL (clicked ()), this, SLOT (previous ()));
}

welcome_wizard::~welcome_wizard()
{
  delete _ui;
}

void
welcome_wizard::next ()
{
  _ui->stackedWidget->setCurrentIndex (_ui->stackedWidget->currentIndex () + 1);
}

void
welcome_wizard::previous ()
{
  _ui->stackedWidget->setCurrentIndex (_ui->stackedWidget->currentIndex () - 1);
}

