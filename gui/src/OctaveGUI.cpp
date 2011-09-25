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

#include <QtGui/QApplication>
#include <QTranslator>
#include <QSettings>
#include "CommandLineParser.h"
#include "WelcomeWizard.h"
#include "ResourceManager.h"
#include "MainWindow.h"

int
main (int argc, char *argv[])
{
  QApplication application (argc, argv);
  while (true)
    {
      if (ResourceManager::instance ()->isFirstRun ())
        {
          WelcomeWizard welcomeWizard;
          int returnCode = welcomeWizard.exec ();

          QSettings *settings = ResourceManager::instance ()->settings ();
          settings->setValue ("connectOnStartup", true);
          settings->setValue ("showMessageOfTheDay", true);
          settings->setValue ("showTopic", true);
          settings->setValue ("autoIdentification", false);
          settings->setValue ("nickServPassword", "");
          settings->setValue ("useCustomFileEditor", false);
          settings->setValue ("customFileEditor", "emacs");
          settings->setValue ("editor/showLineNumbers", true);
          settings->setValue ("editor/highlightActualLine", true);
          settings->setValue ("editor/codeCompletion", true);
          settings->setValue ("editor/fontName", "Monospace");
          settings->setValue ("editor/fontSize", 10);
          settings->setValue ("editor/shortWindowTitle", true);
          settings->setValue ("showFilenames", true);
          settings->setValue ("showFileSize", false);
          settings->setValue ("showFileType", false);
          settings->setValue ("showLastModified", false);
          settings->setValue ("showHiddenFiles", false);
          settings->setValue ("useAlternatingRowColors", true);
          settings->setValue ("useProxyServer", false);
          settings->setValue ("proxyType", "Sock5Proxy");
          settings->setValue ("proxyHostName", "none");
          settings->setValue ("proxyPort", 8080);
          settings->setValue ("proxyUserName", "");
          settings->setValue ("proxyPassword", "");
          settings->sync ();
          ResourceManager::instance ()->reloadSettings ();

          application.quit ();
          // We are in an infinite loop, so everything else than a return
          // will cause the application to restart from the very beginning.
          if (returnCode == QDialog::Rejected)
            return 0;
        }
      else
        {
          CommandLineParser commandLineParser;
          commandLineParser.registerOption ("--config", "-c", "Tells OctaveGUI to use that configuration file.", true);
          commandLineParser.parse (argc, argv);

          QSettings *settings = ResourceManager::instance ()->settings ();
          QString language = settings->value ("language").toString ();

          QString translatorFile = ResourceManager::instance ()->findTranslatorFile (language);
          QTranslator translator;
          translator.load (translatorFile);
          application.installTranslator (&translator);

          ResourceManager::instance ()->updateNetworkSettings ();
          ResourceManager::instance ()->loadIcons ();

          MainWindow w;
          w.show ();
          w.activateWindow();
          return application.exec ();
        }
    }
}
