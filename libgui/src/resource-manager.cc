/*

Copyright (C) 2011-2018 Jacob Dawid

This file is part of Octave.

Octave is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<https://www.gnu.org/licenses/>.

*/

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <string>

#include <QFile>
#include <QDir>
#include <QNetworkProxy>
#include <QLibraryInfo>
#include <QMessageBox>
#if defined (HAVE_QSTANDARDPATHS)
#  include <QStandardPaths>
#endif
#include <QTextCodec>

#include "error.h"
#include "file-ops.h"
#include "help.h"
#include "oct-env.h"

#include "defaults.h"

#include "QTerminal.h"
#include "workspace-model.h"
#include "variable-editor.h"
#include "resource-manager.h"
#include "gui-preferences.h"

namespace octave
{
  resource_manager *resource_manager::instance = nullptr;

  static QString
  default_qt_settings_file (void)
  {
    std::string dsf = sys::env::getenv ("OCTAVE_DEFAULT_QT_SETTINGS");

    if (dsf.empty ())
      dsf = (config::oct_etc_dir ()
             + sys::file_ops::dir_sep_str ()
             + "default-qt-settings");

    return QString::fromStdString (dsf);
  }

  resource_manager::resource_manager (void)
    : m_settings_directory (), m_settings_file (), m_settings (nullptr),
      m_default_settings (nullptr)
  {
#if defined (HAVE_QSTANDARDPATHS)
    QString home_path
      = QStandardPaths::writableLocation (QStandardPaths::HomeLocation);
#else
    QString home_path
      = QDesktopServices::storageLocation (QDesktopServices::HomeLocation);
#endif

    m_settings_directory = home_path + "/.config/octave";

    m_settings_file = m_settings_directory + "/qt-settings";

    m_default_settings = new QSettings (default_qt_settings_file (),
                                        QSettings::IniFormat);
  }

  resource_manager::~resource_manager (void)
  {
    delete m_settings;
    delete m_default_settings;
  }

  QString resource_manager::get_gui_translation_dir (void)
  {
    // get environment variable for the locale dir (e.g. from run-octave)
    std::string dldir = sys::env::getenv ("OCTAVE_LOCALE_DIR");
    if (dldir.empty ())
      dldir = config::oct_locale_dir (); // env-var empty, load the default location
    return QString::fromStdString (dldir);
  }

  void resource_manager::config_translators (QTranslator *qt_tr,
                                             QTranslator *qsci_tr,
                                             QTranslator *gui_tr)
  {
    bool loaded;

    QString qt_trans_dir
      = QLibraryInfo::location (QLibraryInfo::TranslationsPath);

    QString language = "SYSTEM";  // take system language per default

    QSettings *settings = resource_manager::get_settings ();

    if (settings)
      {
        // get the locale from the settings if already available
        language = settings->value ("language", "SYSTEM").toString ();
      }

    if (language == "SYSTEM")
      language = QLocale::system ().name ();    // get system wide locale

    // load the translator file for qt strings
    loaded = qt_tr->load ("qt_" + language, qt_trans_dir);

    if (! loaded) // try lower case
      qt_tr->load ("qt_" + language.toLower (), qt_trans_dir);

    // load the translator file for qscintilla settings
    loaded = qsci_tr->load ("qscintilla_" + language, qt_trans_dir);

    if (! loaded) // try lower case
      qsci_tr->load ("qscintilla_" + language.toLower (), qt_trans_dir);

    // load the translator file for gui strings
    gui_tr->load (language, get_gui_translation_dir ());
  }

  QStringList resource_manager::storage_class_names (void)
  {
    return workspace_model::storage_class_names ();
  }

  QList<QColor> resource_manager::storage_class_default_colors (void)
  {
    return workspace_model::storage_class_default_colors ();
  }

  QStringList resource_manager::terminal_color_names (void)
  {
    return QTerminal::color_names ();
  }

  QList<QColor> resource_manager::terminal_default_colors (void)
  {
    return QTerminal::default_colors ();
  }

  QList<QColor> resource_manager::varedit_default_colors(void)
  {
    return variable_editor::default_colors ();
  }

  QStringList resource_manager::varedit_color_names(void)
  {
    return variable_editor::color_names ();
  }

  bool resource_manager::instance_ok (void)
  {
    bool retval = true;

    if (! instance)
      instance = new resource_manager ();

    if (! instance)
      {
        error ("unable to create resource_manager object!");

        retval = false;
      }

    return retval;
  }

  QSettings * resource_manager::do_get_settings (void) const
  {
    return m_settings;
  }

  QSettings * resource_manager::do_get_default_settings (void) const
  {
    return m_default_settings;
  }

  QString resource_manager::do_get_settings_directory (void)
  {
    return m_settings_directory;
  }

  QString resource_manager::do_get_settings_file (void)
  {
    return m_settings_file;
  }

  QString resource_manager::do_get_default_font_family (void)
  {
    // Get the default monospaced font
#if defined (HAVE_QFONT_MONOSPACE)
    QFont fixed_font;
    fixed_font.setStyleHint (QFont::Monospace);
    QString default_family = fixed_font.defaultFamily ();
#else
    QString default_family = global_font_family;
#endif

    std::string env_default_family = sys::env::getenv ("OCTAVE_DEFAULT_FONT");
    if (! env_default_family.empty ())
      default_family = QString::fromStdString (env_default_family);

    return default_family;
  }

  void resource_manager::do_reload_settings (void)
  {
    QString default_family = do_get_default_font_family ();

    if (! QFile::exists (m_settings_file))
      {
        QDir ("/").mkpath (m_settings_directory);
        QFile qt_settings (default_qt_settings_file ());

        if (! qt_settings.open (QFile::ReadOnly))
          return;

        QTextStream in (&qt_settings);
        QString settings_text = in.readAll ();
        qt_settings.close ();

        default_family = do_get_default_font_family ();

        QString default_font_size = "10";

        std::string env_default_font_size
          = sys::env::getenv ("OCTAVE_DEFAULT_FONT_SIZE");

        if (! env_default_font_size.empty ())
          default_font_size = QString::fromStdString (env_default_font_size);

        // Get the default custom editor
#if defined (Q_OS_WIN32)
        QString custom_editor = "notepad++ -n%l %f";
#else
        QString custom_editor = "emacs +%l %f";
#endif

        std::string env_default_editor
          = sys::env::getenv ("OCTAVE_DEFAULT_EDITOR");

        if (! env_default_editor.empty ())
          custom_editor = QString::fromStdString (env_default_editor);

        // Replace placeholders
        settings_text.replace ("__default_custom_editor__", custom_editor);
        settings_text.replace ("__default_font__", default_family);
        settings_text.replace ("__default_font_size__", default_font_size);

        QFile user_settings (m_settings_file);

        if (! user_settings.open (QIODevice::WriteOnly))
          return;

        QTextStream out (&user_settings);

        out << settings_text;

        user_settings.close ();
      }

    do_set_settings (m_settings_file);

    // Write the default monospace font into the settings for later use by
    // console and editor as fallbacks of their font prefernces.
    if (m_settings)
      m_settings->setValue (global_mono_font.key, default_family);

  }

  void resource_manager::do_set_settings (const QString& file)
  {
    delete m_settings;
    m_settings = new QSettings (file, QSettings::IniFormat);

    if (! (m_settings
           && QFile::exists (m_settings->fileName ())
           && m_settings->isWritable ()
           && m_settings->status () == QSettings::NoError))
      {
        QString msg = QString (QT_TR_NOOP (
                                           "The settings file\n%1\n"
                                           "does not exist and can not be created.\n"
                                           "Make sure you have read and write permissions to\n%2\n\n"
                                           "Octave GUI must be closed now."));
        QMessageBox::critical (nullptr, QString (QT_TR_NOOP ("Octave Critical Error")),
                               msg.arg (do_get_settings_file ()).arg (do_get_settings_directory ()));
        exit (1);
      }
  }

  bool resource_manager::do_update_settings_key (const QString& old_key,
                                                 const QString& new_key)
  {
    if (m_settings->contains (old_key))
      {
        QVariant preference = m_settings->value (old_key);
        m_settings->setValue (new_key, preference);
        m_settings->remove (old_key);
        return true;
      }

    return false;
  }

  bool resource_manager::do_is_first_run (void) const
  {
    return ! QFile::exists (m_settings_file);
  }

  void resource_manager::do_update_network_settings (void)
  {
    if (m_settings)
      {
        QNetworkProxy::ProxyType proxyType = QNetworkProxy::NoProxy;

        if (m_settings->value ("useProxyServer",false).toBool ())
          {
            QString proxyTypeString = m_settings->value ("proxyType").toString ();

            if (proxyTypeString == "Socks5Proxy")
              proxyType = QNetworkProxy::Socks5Proxy;
            else if (proxyTypeString == "HttpProxy")
              proxyType = QNetworkProxy::HttpProxy;
          }

        QNetworkProxy proxy;

        proxy.setType (proxyType);
        proxy.setHostName (m_settings->value ("proxyHostName").toString ());
        proxy.setPort (m_settings->value ("proxyPort",80).toInt ());
        proxy.setUser (m_settings->value ("proxyUserName").toString ());
        proxy.setPassword (m_settings->value ("proxyPassword").toString ());

        QNetworkProxy::setApplicationProxy (proxy);
      }
    else
      {
        // FIXME: Is this an error?  If so, what should we do?
      }
  }

  QIcon resource_manager::do_icon (const QString& icon_name, bool fallback)
  {
    // If system icon theme is not desired, take own icon files
    if (! m_settings->value (global_icon_theme.key, global_icon_theme.def).toBool ())
      return QIcon (":/actions/icons/" + icon_name + ".png");

    // Use system icon theme with own files as fallback except the fallback is
    // explicitly disabled (fallback=false)
    if (fallback)
      return QIcon::fromTheme (icon_name,
                               QIcon (":/actions/icons/" + icon_name + ".png"));
    else
      return QIcon::fromTheme (icon_name);
  }

  // get a list of all available encodings
  void resource_manager::do_get_codecs (QStringList *codecs)
  {
    // get the codec name for each mib
    QList<int> all_mibs = QTextCodec::availableMibs ();
    foreach (int mib, all_mibs)
      {
        QTextCodec *c = QTextCodec::codecForMib (mib);
        codecs->append (c->name ().toUpper ());
      }
    codecs->removeDuplicates ();
    qSort (*codecs);
  }

  // initialize a given combo box with available text encodings
  void resource_manager::do_combo_encoding (QComboBox *combo, QString current)
  {
    QStringList all_codecs;
    do_get_codecs (&all_codecs);

    // get the value from the settings file if no current encoding is given
    QString enc = current;
    if (enc.isEmpty ())
      {
        enc = m_settings->value (ed_default_enc.key, ed_default_enc.def).toString ();
        if (enc.isEmpty ())  // still empty?
          enc = ed_default_enc.def.toString ();     // take default
      }

    // fill the combo box
    foreach (QString c, all_codecs)
      combo->addItem (c);

    // prepend the default item
    combo->insertSeparator (0);
    combo->insertItem (0, ed_default_enc.def.toString ());

    // select the current/default item
    int idx = combo->findText (enc);
    if (idx >= 0)
      combo->setCurrentIndex (idx);
    else
      combo->setCurrentIndex (0);

    combo->setMaxVisibleItems (12);
  }
}
