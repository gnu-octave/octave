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

#include <QtGui/QApplication>
#include <QTranslator>
#include <QSettings>
#include "welcome-wizard.h"
#include "resource-manager.h"
#include "main-window.h"

int
main (int argc, char *argv[])
{
  QApplication application (argc, argv);
  while (true)
    {
      if (resource_manager::instance ()->is_first_run ())
        {
          welcome_wizard welcomeWizard;
          welcomeWizard.exec ();
          resource_manager::instance ()->reload_settings ();
        }
      else
        {
          QSettings *settings = resource_manager::instance ()->get_settings ();
          QString language = settings->value ("language").toString ();

          QString translatorFile = resource_manager::instance ()->find_translator_file (language);
          QTranslator translator;
          translator.load (translatorFile);
          application.installTranslator (&translator);

          resource_manager::instance ()->update_network_settings ();
          resource_manager::instance ()->load_icons ();

          main_window w;
          w.show ();
          return application.exec ();
        }
    }
}
