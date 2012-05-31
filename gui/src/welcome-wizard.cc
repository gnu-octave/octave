/* OctaveGUI - A graphical user interface for Octave
 * Copyright (C) 2011 Jacob Dawid (jacob.dawid@googlemail.com)
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */

#include "welcome-wizard.h"
#include "ui_welcome-wizard.h"

welcome_wizard::welcome_wizard (QWidget *parent) :
  QDialog (parent),
  ui (new user_interface::welcome_wizard)
{
  ui->setupUi (this);
  connect (ui->nextButton1, SIGNAL (clicked ()), this, SLOT (next ()));
  connect (ui->nextButton2, SIGNAL (clicked ()), this, SLOT (next ()));
  connect (ui->nextButton3, SIGNAL (clicked ()), this, SLOT (next ()));
  connect (ui->nextButton4, SIGNAL (clicked ()), this, SLOT (next ()));

  connect (ui->previousButton2, SIGNAL (clicked ()), this, SLOT (previous ()));
  connect (ui->previousButton3, SIGNAL (clicked ()), this, SLOT (previous ()));
  connect (ui->previousButton4, SIGNAL (clicked ()), this, SLOT (previous ()));
  connect (ui->previousButton5, SIGNAL (clicked ()), this, SLOT (previous ()));
}

welcome_wizard::~welcome_wizard()
{
  delete ui;
}

void
welcome_wizard::next ()
{
  ui->stackedWidget->setCurrentIndex (ui->stackedWidget->currentIndex () + 1);
}

void
welcome_wizard::previous ()
{
  ui->stackedWidget->setCurrentIndex (ui->stackedWidget->currentIndex () - 1);
}

