////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2011-2020 The Octave Project Developers
//
// See the file COPYRIGHT.md in the top-level directory of this
// distribution or <https://octave.org/copyright/>.
//
// This file is part of Octave.
//
// Octave is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// Octave is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Octave; see the file COPYING.  If not, see
// <https://www.gnu.org/licenses/>.
//
////////////////////////////////////////////////////////////////////////

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <string>

#include <QDir>
#include <QFile>
#include <QLibraryInfo>
#include <QMessageBox>
#include <QNetworkProxy>
#if defined (HAVE_QSTANDARDPATHS)
#  include <QStandardPaths>
#else
#  include <QDesktopServices>
#endif

#include <QTextCodec>

#include "QTerminal.h"
#include "gui-preferences-ed.h"
#include "gui-preferences-global.h"
#include "octave-qobject.h"
#include "resource-manager.h"
#include "variable-editor.h"
#include "workspace-model.h"

#include "file-ops.h"
#include "localcharset-wrapper.h"
#include "oct-env.h"

#include "defaults.h"
#include "error.h"
#include "help.h"

namespace octave
{
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
      m_default_settings (nullptr), m_temporary_files ()
  {
    // Let gui_settings decide where to put the ini file with gui preferences
    m_default_settings
      = new gui_settings (QSettings::IniFormat, QSettings::UserScope,
                          "octave", "octave-gui");

    m_settings_file = m_default_settings->fileName ();

    QFileInfo sfile (m_settings_file);
    m_settings_directory = sfile.absolutePath ();

    QString xdg_config_home
      = QString::fromLocal8Bit (qgetenv ("XDG_CONFIG_HOME"));

    if ((! sfile.exists ()) && xdg_config_home.isEmpty ())
      {
        // File does not exist yet: Look for a settings file at the old
        // location ($HOME/.config/octave/qt-settings) for impoting all
        // available keys into the new settings file.
        // Do not look for an old settings file if XDG_CONFIG_HOME is set,
        // since then a nonexistent new settings file does not necessarily
        // indicate a first run of octave with new config file locations.
#if defined (HAVE_QSTANDARDPATHS)
        QString home_path
          = QStandardPaths::writableLocation (QStandardPaths::HomeLocation);
#else
        QString home_path
          = QDesktopServices::storageLocation (QDesktopServices::HomeLocation);
#endif

        QString old_settings_directory = home_path + "/.config/octave";
        QString old_settings_file = old_settings_directory + "/qt-settings";

        QFile ofile (old_settings_file);

        if (ofile.exists ())
          {
            // Old settings file exists; create a gui_settings object related
            // to it and copy all available keys to the new settings
            gui_settings old_settings (old_settings_file, QSettings::IniFormat);

            QStringList keys = old_settings.allKeys ();
            for (int i = 0; i < keys.count(); i++)
              m_default_settings->setValue (keys.at(i),
                                            old_settings.value(keys.at(i)));

            m_default_settings->sync ();  // Done, make sure keys are written
          }
      }
  }

  resource_manager::~resource_manager (void)
  {
    delete m_settings;
    delete m_default_settings;

    for (int i = m_temporary_files.count () - 1; i >=0; i--)
      remove_tmp_file (m_temporary_files.at (i));
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

    // FIXME: can we somehow ensure that the settings object will always
    // be initialize and valid?

    if (m_settings)
      {
        // get the locale from the settings if already available
        language = m_settings->value (global_language.key,
                                      global_language.def).toString ();
      }

    // load the translations depending on the settings
    if (language == "SYSTEM")
      {
        // get the system locale and pass it to the translators for loading
        // the suitable translation files
        QLocale sys_locale = QLocale::system ();

        qt_tr->load (sys_locale, "qt", "_", qt_trans_dir);
        qsci_tr->load (sys_locale, "qscintilla", "_", qt_trans_dir);
        gui_tr->load (sys_locale, "", "", get_gui_translation_dir ());
      }
    else
      {
        // load the translation files depending on the given locale name
        loaded = qt_tr->load ("qt_" + language, qt_trans_dir);
        if (! loaded) // try lower case
          qt_tr->load ("qt_" + language.toLower (), qt_trans_dir);

        loaded = qsci_tr->load ("qscintilla_" + language, qt_trans_dir);
        if (! loaded) // try lower case
          qsci_tr->load ("qscintilla_" + language.toLower (), qt_trans_dir);

        gui_tr->load (language, get_gui_translation_dir ());
      }

  }

  gui_settings * resource_manager::get_settings (void) const
  {
    return m_settings;
  }

  gui_settings * resource_manager::get_default_settings (void) const
  {
    return m_default_settings;
  }

  QString resource_manager::get_settings_directory (void)
  {
    return m_settings_directory;
  }

  QString resource_manager::get_settings_file (void)
  {
    return m_settings_file;
  }

  QString resource_manager::get_default_font_family (void)
  {
    // Get the default monospaced font
    QFont fixed_font;
    fixed_font.setStyleHint (QFont::Monospace);
    QString default_family = fixed_font.defaultFamily ();

    std::string env_default_family = sys::env::getenv ("OCTAVE_DEFAULT_FONT");
    if (! env_default_family.empty ())
      default_family = QString::fromStdString (env_default_family);

    return default_family;
  }

  void resource_manager::reload_settings (void)
  {
    QString default_family = get_default_font_family ();

    if (! QFile::exists (m_settings_file))
      {
        QDir ("/").mkpath (m_settings_directory);
        QFile qt_settings (default_qt_settings_file ());

        if (! qt_settings.open (QFile::ReadOnly))
          return;

        QTextStream in (&qt_settings);
        QString settings_text = in.readAll ();
        qt_settings.close ();

        default_family = get_default_font_family ();

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

    set_settings (m_settings_file);

    // Write the default monospace font into the settings for later use by
    // console and editor as fallbacks of their font preferences.
    if (m_settings)
      m_settings->setValue (global_mono_font.key, default_family);

  }

  void resource_manager::set_settings (const QString& file)
  {
    delete m_settings;
    m_settings = new gui_settings (file, QSettings::IniFormat);

    if (! (QFile::exists (m_settings->fileName ())
           && m_settings->isWritable ()
           && m_settings->status () == QSettings::NoError))
      {
        QString msg
          = QString (QT_TR_NOOP ("The settings file\n%1\n"
                                 "does not exist and can not be created.\n"
                                 "Make sure you have read and write permissions to\n%2\n\n"
                                 "Octave GUI must be closed now."));

        QMessageBox::critical (nullptr,
                               QString (QT_TR_NOOP ("Octave Critical Error")),
                               msg.arg (get_settings_file ()).arg (get_settings_directory ()));

        exit (1);
      }
  }

  bool resource_manager::update_settings_key (const QString& old_key,
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

  bool resource_manager::is_first_run (void) const
  {
    return ! QFile::exists (m_settings_file);
  }

  void resource_manager::update_network_settings (void)
  {
    if (! m_settings)
      return;

    QNetworkProxy proxy;

    // Assume no proxy and empty proxy data
    QNetworkProxy::ProxyType proxy_type = QNetworkProxy::NoProxy;
    QString scheme;
    QString host;
    int port = 0;
    QString user;
    QString pass;
    QUrl proxy_url = QUrl ();

    if (m_settings->value (global_use_proxy.key, global_use_proxy.def).toBool ())
      {
        // Use a proxy, collect all required information
        QString proxy_type_string
            = m_settings->value (global_proxy_type.key, global_proxy_type.def).toString ();

        // The proxy type for the Qt proxy settings
        if (proxy_type_string == "Socks5Proxy")
          proxy_type = QNetworkProxy::Socks5Proxy;
        else if (proxy_type_string == "HttpProxy")
          proxy_type = QNetworkProxy::HttpProxy;

        // The proxy data from the settings
        if (proxy_type_string == "HttpProxy"
            || proxy_type_string == "Socks5Proxy")
          {
            host = m_settings->value (global_proxy_host.key,
                                      global_proxy_host.def).toString ();
            port = m_settings->value (global_proxy_port.key,
                                      global_proxy_port.def).toInt ();
            user = m_settings->value (global_proxy_user.key,
                                      global_proxy_user.def).toString ();
            pass = m_settings->value (global_proxy_pass.key,
                                      global_proxy_pass.def).toString ();
            if (proxy_type_string == "HttpProxy")
              scheme = "http";
            else if (proxy_type_string == "Socks5Proxy")
              scheme = "socks5";

            QUrl env_var_url = QUrl ();
            proxy_url.setScheme (scheme);
            proxy_url.setHost (host);
            proxy_url.setPort (port);
            if (! user.isEmpty ())
              proxy_url.setUserName (user);
            if (! pass.isEmpty ())
              proxy_url.setPassword (pass);
          }

        // The proxy data from environment variables
        if (proxy_type_string == global_proxy_all_types.at (2))
          {
            const std::array<std::string, 6> env_vars =
                { "ALL_PROXY", "all_proxy",
                  "HTTP_PROXY", "http_proxy",
                  "HTTPS_PROXY", "https_proxy" };

            unsigned int count = 0;
            while (! proxy_url.isValid () && count < env_vars.size ())
              {
                proxy_url = QUrl (QString::fromStdString
                                    (sys::env::getenv (env_vars[count])));
                count++;
              }

            if (proxy_url.isValid ())
              {
                // Found an entry, get the data from the string
                scheme = proxy_url.scheme ();

                if (scheme.contains ("socks", Qt::CaseInsensitive))
                  proxy_type = QNetworkProxy::Socks5Proxy;
                else
                  proxy_type = QNetworkProxy::HttpProxy;

                host = proxy_url.host ();
                port = proxy_url.port ();
                user = proxy_url.userName ();
                pass = proxy_url.password ();
              }
          }
      }

    // Set proxy for Qt framework
    proxy.setType (proxy_type);
    proxy.setHostName (host);
    proxy.setPort (port);
    proxy.setUser (user);
    proxy.setPassword (pass);

    QNetworkProxy::setApplicationProxy (proxy);

    // Set proxy for curl library if not based on environment variables
    std::string proxy_url_str = proxy_url.toString().toStdString ();
    sys::env::putenv ("http_proxy", proxy_url_str);
    sys::env::putenv ("HTTP_PROXY", proxy_url_str);
    sys::env::putenv ("https_proxy", proxy_url_str);
    sys::env::putenv ("HTTPS_PROXY", proxy_url_str);
  }

  QIcon resource_manager::icon (const QString& icon_name, bool fallback)
  {
    // If system icon theme is not desired, take own icon files
    if (! m_settings->value (global_icon_theme).toBool ())
      return QIcon (":/actions/icons/" + icon_name + ".png");

    // Use system icon theme with own files as fallback except when the
    // fallback is explicitly disabled (fallback=false)
    if (fallback)
      return QIcon::fromTheme (icon_name,
                               QIcon (":/actions/icons/" + icon_name + ".png"));
    else
      return QIcon::fromTheme (icon_name);
  }

  // get a list of all available encodings
  void resource_manager::get_codecs (QStringList *codecs)
  {
    // get the codec name for each mib
    QList<int> all_mibs = QTextCodec::availableMibs ();
    for (auto mib : all_mibs)
      {
        QTextCodec *c = QTextCodec::codecForMib (mib);
        codecs->append (c->name ().toUpper ());
      }

    // Append SYSTEM
    codecs->append (QString ("SYSTEM (") +
                    QString (octave_locale_charset_wrapper ()).toUpper () +
                    QString (")"));

    // Clean up and sort list of codecs
    codecs->removeDuplicates ();
    qSort (*codecs);
  }

  // initialize a given combo box with available text encodings
  void resource_manager::combo_encoding (QComboBox *combo,
                                         const QString& current)
  {
    QStringList all_codecs;
    get_codecs (&all_codecs);

    // get the value from the settings file if no current encoding is given
    QString enc = current;

    // Check for valid codec for the default.  If this fails, "SYSTEM" (i.e.
    // locale_charset) will be chosen.
    // FIXME: The default is "SYSTEM" on all platforms.  So can this fallback
    // logic be removed completely?
    bool default_exists = false;
    if (QTextCodec::codecForName (ed_default_enc.def.toString ().toLatin1 ())
        || (ed_default_enc.def.toString ().startsWith ("SYSTEM")))
      default_exists = true;

    QString default_enc =
      QString ("SYSTEM (") +
      QString (octave_locale_charset_wrapper ()).toUpper () + QString (")");

    if (enc.isEmpty ())
      {
        enc = m_settings->value (ed_default_enc).toString ();

        if (enc.isEmpty ())  // still empty?
          {
            if (default_exists)
              enc = ed_default_enc.def.toString ();
            else
              enc = default_enc;
          }
      }

    // fill the combo box
    for (const auto& c : all_codecs)
      combo->addItem (c);

    // prepend the default item
    combo->insertSeparator (0);
    if (default_exists)
      combo->insertItem (0, ed_default_enc.def.toString ());
    else
      combo->insertItem (0, default_enc);

    // select the default or the current one
    int idx = combo->findText (enc, Qt::MatchExactly);
    if (idx >= 0)
      combo->setCurrentIndex (idx);
    else
      combo->setCurrentIndex (0);

    combo->setMaxVisibleItems (12);
  }

  QPointer<QTemporaryFile>
  resource_manager::create_tmp_file (const QString& extension,
                                     const QString& contents)
  {
    QString ext = extension;
    if ((! ext.isEmpty ()) && (! ext.startsWith ('.')))
      ext = QString (".") + ext;

    // Create octave dir within temp. dir
    QString tmp_dir = QDir::tempPath () + QDir::separator() + "octave";
    QDir::temp ().mkdir ("octave");

    // Create temp. file
    QPointer<QTemporaryFile> tmp_file
      = new QTemporaryFile (tmp_dir + QDir::separator() +
                            "octave_XXXXXX" + ext, this);

    if (tmp_file->open ())
      {
        tmp_file->write (contents.toUtf8 ());
        tmp_file->close ();

        m_temporary_files << tmp_file;
      }

    return tmp_file;
  }

  void resource_manager::remove_tmp_file (QPointer<QTemporaryFile> tmp_file)
  {
    if (tmp_file)
      {
        if (tmp_file->exists ())
          tmp_file->remove ();

        m_temporary_files.removeAll (tmp_file);
      }
  }
}
