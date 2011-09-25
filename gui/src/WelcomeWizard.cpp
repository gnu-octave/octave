/* OctaveGUI - A graphical user interface for Octave
 * Copyright (C) 2011 Jacob Dawid (jacob.dawid@googlemail.com)
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */

#include "WelcomeWizard.h"
#include "ui_WelcomeWizard.h"

WelcomeWizard::WelcomeWizard (QWidget *parent) :
  QDialog (parent),
  ui (new Ui::WelcomeWizard)
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

WelcomeWizard::~WelcomeWizard()
{
  delete ui;
}

void
WelcomeWizard::next ()
{
  ui->stackedWidget->setCurrentIndex (ui->stackedWidget->currentIndex () + 1);
}

void
WelcomeWizard::previous ()
{
  ui->stackedWidget->setCurrentIndex (ui->stackedWidget->currentIndex () - 1);
}

