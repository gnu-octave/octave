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

#ifndef RESOURCEMANAGER_H
#define RESOURCEMANAGER_H

#include <QDesktopServices>
#include <QIcon>
#include <QMap>
#include <QSettings>


class resource_manager
{
protected:

  resource_manager (void);

public:

  ~resource_manager ();

  static QSettings *get_settings (void)
  {
    return instance_ok () ? instance->do_get_settings () : 0;
  }

  static QString get_home_path (void)
  {
    return instance_ok () ? instance->do_get_home_path () : QString ();
  }

  static void reload_settings (void)
  {
    if (instance_ok ())
      instance->do_reload_settings ();
  }

  static void set_settings (const QString& file)
  {
    if (instance_ok ())
      instance->do_set_settings (file);
  }

  static QString find_translator_file (const QString& language);

  static void update_network_settings (void)
  {
    if (instance_ok ())
      instance->do_update_network_settings ();
  }

  static bool is_first_run (void)
  {
    return instance_ok () ? instance->do_is_first_run () : true;
  }

  static const char *octave_keywords (void);

private:

  static resource_manager *instance;

  static void cleanup_instance (void) { delete instance; instance = 0; }

  // No copying!

  resource_manager (const resource_manager&);

  resource_manager& operator = (const resource_manager&);

  static bool instance_ok (void);

  QSettings *settings;

  QString home_path;

  bool first_run;

  QSettings *do_get_settings (void);

  QString do_get_home_path (void);

  void do_reload_settings (void);

  void do_set_settings (const QString& file);

  void do_update_network_settings (void);

  bool do_is_first_run (void);

};

#endif // RESOURCEMANAGER_H
