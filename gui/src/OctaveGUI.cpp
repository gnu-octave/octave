/* OctaveGUI - A graphical user interface for Octave
 * Copyright (C) 2011 Jacob Dawid
 * jacob.dawid@googlemail.com
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#include <QtGui/QApplication>
#include <QTranslator>
#include <QSettings>
#include "ResourceManager.h"
#include "MainWindow.h"

int
main (int argc, char *argv[])
{
  QApplication application (argc, argv);
  // QSettings *settings = ResourceManager::instance ()->instance ();

  // TODO: reimplement translation.
  // translator.load (translatorFile);
  // application.installTranslator (&translator);

  MainWindow w;
  w.show ();
  return application.exec ();
}
