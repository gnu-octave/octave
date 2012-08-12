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

#ifndef RESOURCEMANAGER_H
#define RESOURCEMANAGER_H

#include <QSettings>
#include <QDesktopServices>
#include <QMap>
#include <QIcon>

class resource_manager
{
public:

  ~resource_manager ();

  static resource_manager *
  instance ()
  {
    return (instance_ok ()) ? _instance : 0;
  }

  QSettings *get_settings ();
  QString get_home_path ();
  void reload_settings ();
  void set_settings (QString file);
  QString find_translator_file (QString language);
  void update_network_settings ();

  bool is_first_run ();
  const char *octave_keywords ();

private:
  resource_manager ();

  static bool instance_ok ();

  static void cleanup_instance () { delete _instance; _instance = 0; }

  QSettings *_settings;
  QString _home_path;
  static resource_manager *_instance;
  bool _first_run;
};

#endif // RESOURCEMANAGER_H
