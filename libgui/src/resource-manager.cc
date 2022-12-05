////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2011-2022 The Octave Project Developers
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

#include <unistd.h>

#include <algorithm>
#include <array>
#include <string>

#include <QApplication>
#include <QDir>
#include <QFile>
#include <QLibraryInfo>
#include <QNetworkProxy>
#include <QStandardPaths>

#include <QTextCodec>

#include "QTerminal.h"
#include "gui-preferences-cs.h"
#include "gui-preferences-ed.h"
#include "gui-preferences-global.h"
#include "gui-settings.h"
#include "resource-manager.h"
#include "variable-editor.h"
#include "workspace-model.h"

#include "file-ops.h"
#include "localcharset-wrapper.h"
#include "oct-env.h"

#include "defaults.h"

namespace octave
{
  resource_manager::resource_manager (void)
    : m_temporary_files ()
  { }

  resource_manager::~resource_manager (void)
  {
    for (int i = m_temporary_files.count () - 1; i >=0; i--)
      remove_tmp_file (m_temporary_files.at (i));
  }

  QString resource_manager::get_gui_translation_dir (void)
  {
    // get environment variable for the locale dir (e.g. from run-octave)
    std::string dldir = sys::env::getenv ("OCTAVE_LOCALE_DIR");
    if (dldir.empty ())
      dldir = config::oct_locale_dir ();  // env-var empty, load the default location
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

    gui_settings settings;

    // get the locale from the settings if already available
    language = settings.value (global_language.key,
                               global_language.def).toString ();

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
        if (! loaded)  // try lower case
          qt_tr->load ("qt_" + language.toLower (), qt_trans_dir);

        loaded = qsci_tr->load ("qscintilla_" + language, qt_trans_dir);
        if (! loaded)  // try lower case
          qsci_tr->load ("qscintilla_" + language.toLower (), qt_trans_dir);

        gui_tr->load (language, get_gui_translation_dir ());
      }

  }

#if defined (HAVE_QSCINTILLA)
  int resource_manager::get_valid_lexer_styles (QsciLexer *lexer, int *styles)
  {
    int max_style = 0;
    int actual_style = 0;
    while (actual_style < ed_max_style_number && max_style < ed_max_lexer_styles)
      {
        if ((lexer->description (actual_style)) != "")  // valid style
          styles[max_style++] = actual_style;
        actual_style++;
      }
    return max_style;
  }
#endif

  QFont resource_manager::copy_font_attributes (const QFont& attr,
                                                const QFont& base) const
  {
    QFont dest (base);

    dest.setBold (attr.bold ());
    dest.setItalic (attr.italic ());
    dest.setUnderline (attr.underline ());

    return dest;
  }

#if defined (HAVE_QSCINTILLA)
  void resource_manager::read_lexer_settings (QsciLexer *lexer,
                                              int mode, int def)
  {
    gui_settings settings;

    // Test whether the settings for lexer is already contained in the
    // given gui settings file. If yes, load them, if not copy them from the
    // default settings file.
    // This is useful when a new language support is implemented and the
    // existing settings file is used (which is of course the common case).
    int m = mode;
    if (m > 1)
      m = 1;

    QString group ("Scintilla" + settings_color_modes_ext[m]);

    settings.beginGroup (group);
    settings.beginGroup (lexer->language ());

    QStringList lexer_keys = settings.allKeys ();

    settings.endGroup ();
    settings.endGroup ();

    if (def == settings_reload_default_colors_flag || lexer_keys.count () == 0)
      {
        // We have to reload the default values or no Lexer keys found:
        // If mode == 0, take all settings except font from default lexer
        // If Mode == 1, take all settings except font from default lexer
        //               and convert the color by inverting the lightness

        // Get the default font
        QStringList def_font = settings.get_default_font ();
        QFont df (def_font[0], def_font[1].toInt ());
        QFont dfa = copy_font_attributes (lexer->defaultFont (), df);
        lexer->setDefaultFont (dfa);

        QColor c, p;

        int styles[ed_max_lexer_styles];  // array for saving valid styles
        int max_style = get_valid_lexer_styles (lexer, styles);

        for (int i = 0; i < max_style; i++)
          {
            c = settings.get_color_value (QVariant (lexer->color (styles[i])), m);
            lexer->setColor (c, styles[i]);
            p = settings.get_color_value (QVariant (lexer->paper (styles[i])), m);
            lexer->setPaper (p, styles[i]);
            dfa = copy_font_attributes (lexer->font (styles[i]), df);
            lexer->setFont (dfa, styles[i]);
          }
        // Set defaults last for not changing the defaults of the styles
        lexer->setDefaultColor (lexer->color (styles[0]));
        lexer->setDefaultPaper (lexer->paper (styles[0]));

        // Write settings if not just reload the default values
        if (def != settings_reload_default_colors_flag)
          {
            const std::string group_str = group.toStdString ();
            lexer->writeSettings (settings, group_str.c_str ());
            settings.sync ();
          }
      }
    else
      {
        // Found lexer keys, read the settings
        const std::string group_str = group.toStdString ();
        lexer->readSettings (settings, group_str.c_str ());
      }
  }
#endif

  bool resource_manager::update_settings_key (const QString& old_key,
                                              const QString& new_key)
  {
    gui_settings settings;

    if (settings.contains (old_key))
      {
        QVariant preference = settings.value (old_key);
        settings.setValue (new_key, preference);
        settings.remove (old_key);
        return true;
      }

    return false;
  }

  void resource_manager::update_network_settings (void)
  {
    QNetworkProxy proxy;

    // Assume no proxy and empty proxy data
    QNetworkProxy::ProxyType proxy_type = QNetworkProxy::NoProxy;
    QString scheme;
    QString host;
    int port = 0;
    QString user;
    QString pass;
    QUrl proxy_url = QUrl ();

    gui_settings settings;

    if (settings.value (global_use_proxy.key, global_use_proxy.def).toBool ())
      {
        // Use a proxy, collect all required information
        QString proxy_type_string
          = settings.value (global_proxy_type.key, global_proxy_type.def).toString ();

        // The proxy type for the Qt proxy settings
        if (proxy_type_string == "Socks5Proxy")
          proxy_type = QNetworkProxy::Socks5Proxy;
        else if (proxy_type_string == "HttpProxy")
          proxy_type = QNetworkProxy::HttpProxy;

        // The proxy data from the settings
        if (proxy_type_string == "HttpProxy"
            || proxy_type_string == "Socks5Proxy")
          {
            host = settings.value (global_proxy_host.key,
                                   global_proxy_host.def).toString ();
            port = settings.value (global_proxy_port.key,
                                   global_proxy_port.def).toInt ();
            user = settings.value (global_proxy_user.key,
                                   global_proxy_user.def).toString ();
            pass = settings.value (global_proxy_pass.key,
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
            {
              "ALL_PROXY", "all_proxy",
              "HTTP_PROXY", "http_proxy",
              "HTTPS_PROXY", "https_proxy"
            };

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
    std::sort (codecs->begin (), codecs->end ());
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
    bool show_system = false;
    if (ed_default_enc.def.toString ().startsWith ("SYSTEM"))
      show_system = true;
    else if (QTextCodec::codecForName (ed_default_enc.def.toString ().toLatin1 ()))
      default_exists = true;

    QString default_enc =
      QString ("SYSTEM (") +
      QString (octave_locale_charset_wrapper ()).toUpper () + QString (")");

    if (enc.isEmpty ())
      {
        gui_settings settings;

        enc = settings.value (ed_default_enc).toString ();

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
    if (show_system || ! default_exists)
      combo->insertItem (0, default_enc);
    else
      combo->insertItem (0, ed_default_enc.def.toString ());

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
    QString tmp_dir = QString::fromStdString (sys::env::get_temp_directory ());

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
