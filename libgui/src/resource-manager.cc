/*

Copyright (C) 2011-2013 Jacob Dawid

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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <string>

#include <QFile>
#include <QDir>
#include <QNetworkProxy>
 #include <QLibraryInfo>

#include "error.h"
#include "file-ops.h"
#include "help.h"
#include "oct-env.h"
#include "singleton-cleanup.h"

#include "defaults.h"

#include "QTerminal.h"
#include "workspace-model.h"
#include "resource-manager.h"

resource_manager *resource_manager::instance = 0;

static QString
default_qt_settings_file (void)
{
  std::string dsf = octave_env::getenv ("OCTAVE_DEFAULT_QT_SETTINGS");

  if (dsf.empty ())
    dsf = Voct_etc_dir + file_ops::dir_sep_str () + "default-qt-settings";

  return QString::fromStdString (dsf);
}

resource_manager::resource_manager (void)
  : settings (0), home_path (), first_run (false)
{
  do_reload_settings ();

  default_settings = new QSettings (default_qt_settings_file (),
                                    QSettings::IniFormat);
}

resource_manager::~resource_manager (void)
{
  delete settings;
  delete default_settings;
}


QString
resource_manager::get_gui_translation_dir (void)
{
  // get environment variable for the locale dir (e.g. from run-octave)
  std::string dldir = octave_env::getenv ("OCTAVE_LOCALE_DIR");
  if (dldir.empty ())
    dldir = Voct_locale_dir; // env-var empty, load the default location
  return QString::fromStdString (dldir);
}

void
resource_manager::config_translators (QTranslator *qt_tr,
                                      QTranslator *qsci_tr,
                                      QTranslator *gui_tr)
{
  bool loaded;

  QString qt_trans_dir = QLibraryInfo::location(QLibraryInfo::TranslationsPath);
  QSettings *settings = resource_manager::get_settings ();
  // FIXME -- what should happen if settings is 0?

  // get the locale from the settings
  QString language = settings->value ("language","SYSTEM").toString ();
  if (language == "SYSTEM")
      language = QLocale::system().name();    // get system wide locale

  // load the translator file for qt strings
  loaded = qt_tr->load("qt_" + language, qt_trans_dir);
  if (!loaded) // try lower case
    qt_tr->load("qt_" + language.toLower (), qt_trans_dir);

  // load the translator file for qscintilla settings
  loaded = qsci_tr->load("qscintilla_" + language, qt_trans_dir);
  if (!loaded) // try lower case
    qsci_tr->load("qscintilla_" + language.toLower (), qt_trans_dir);

  // load the translator file for gui strings
  gui_tr->load (language, get_gui_translation_dir ());
}

bool
resource_manager::instance_ok (void)
{
  bool retval = true;

  if (! instance)
    {
      instance = new resource_manager ();

      if (instance)
        singleton_cleanup_list::add (cleanup_instance);
    }

  if (! instance)
    {
      ::error ("unable to create resource_manager object!");

      retval = false;
    }

  return retval;
}

QSettings *
resource_manager::do_get_settings (void) const
{
  return settings;
}

QSettings *
resource_manager::do_get_default_settings (void) const
{
  return default_settings;
}

QString
resource_manager::do_get_home_path (void) const
{
  return home_path;
}

QString
resource_manager::do_get_settings_path (void)
{
  QDesktopServices desktopServices;
  home_path = desktopServices.storageLocation (QDesktopServices::HomeLocation);
  QString settings_path = home_path + "/.config/octave/";
  return settings_path;
}

QString
resource_manager::do_get_settings_file (void)
{
  return do_get_settings_path ()  + "qt-settings";
}

void
resource_manager::do_reload_settings (void)
{
  QDesktopServices desktopServices;
  home_path = desktopServices.storageLocation (QDesktopServices::HomeLocation);
  QString settings_path = do_get_settings_path ();
  QString settings_file = do_get_settings_file ();

  if (!QFile::exists (settings_file))
    {
      QDir("/").mkpath (settings_path);
      QFile::copy (default_qt_settings_file (), settings_file);
      first_run = true;
    }
  else
    first_run = false;

  do_set_settings (settings_file);
}

void
resource_manager::do_set_settings (const QString& file)
{
  delete settings;
  settings = new QSettings (file, QSettings::IniFormat);
}

bool
resource_manager::do_is_first_run (void) const
{
  return first_run;
}

void
resource_manager::do_update_network_settings (void)
{
  QNetworkProxy::ProxyType proxyType = QNetworkProxy::NoProxy;

  if (settings->value ("useProxyServer",false).toBool ())
    {
      QString proxyTypeString = settings->value ("proxyType").toString ();

      if (proxyTypeString == "Socks5Proxy")
        proxyType = QNetworkProxy::Socks5Proxy;
      else if (proxyTypeString == "HttpProxy")
        proxyType = QNetworkProxy::HttpProxy;
    }

  QNetworkProxy proxy;

  proxy.setType (proxyType);
  proxy.setHostName (settings->value ("proxyHostName").toString ());
  proxy.setPort (settings->value ("proxyPort",80).toInt ());
  proxy.setUser (settings->value ("proxyUserName").toString ());
  proxy.setPassword (settings->value ("proxyPassword").toString ());

  QNetworkProxy::setApplicationProxy (proxy);
}

QStringList 
resource_manager::storage_class_names (void)
{
  return workspace_model::storage_class_names ();
}

QList<QColor>
resource_manager::storage_class_default_colors (void)
{
  return workspace_model::storage_class_default_colors ();
}

QStringList 
resource_manager::terminal_color_names (void)
{
  return QTerminal::color_names ();
}

QList<QColor>
resource_manager::terminal_default_colors (void)
{
  return QTerminal::default_colors ();
}
