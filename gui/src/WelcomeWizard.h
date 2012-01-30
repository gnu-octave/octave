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

#ifndef WELCOMEWIZARD_H
#define WELCOMEWIZARD_H

#include <QDialog>

namespace Ui {
class WelcomeWizard;
}

class WelcomeWizard : public QDialog
{
  Q_OBJECT

public:
  explicit WelcomeWizard(QWidget *parent = 0);
  ~WelcomeWizard();

public slots:
  void next ();
  void previous ();

private:
  Ui::WelcomeWizard *ui;
};

#endif // WELCOMEWIZARD_H
