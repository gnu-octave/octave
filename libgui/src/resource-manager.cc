////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2011-2023 The Octave Project Developers
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

#include <QDir>
#include <QFile>
#include <QFontComboBox>
#include <QFontDatabase>
#include <QLibraryInfo>
#include <QMessageBox>
#include <QNetworkProxy>
#include <QStandardPaths>

#include <QTextCodec>

#include "QTerminal.h"
#include "gui-preferences-cs.h"
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

OCTAVE_BEGIN_NAMESPACE(octave)

resource_manager::resource_manager (void)
: m_settings_directory (), m_settings_file (), m_settings (nullptr),
  m_default_settings (nullptr), m_temporary_files (), m_icon_fallbacks ()
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

void resource_manager::config_icon_theme (void)
{
  m_icon_fallbacks.clear ();

  int theme = global_icon_theme_index.def.toInt ();

  if (m_settings)
    {
      // check for new and old setting and use old if required
      if (! m_settings->contains (global_icon_theme_index.key))
        {
          // new pref does not exist
          if (m_settings->value (global_icon_theme).toBool ())
            theme = ICON_THEME_SYSTEM;
          else
            theme = ICON_THEME_OCTAVE;
          m_settings->setValue (global_icon_theme_index.key, theme);  // add new
          m_settings->remove (global_icon_theme.key); // remove deprecated key
        }
      else
        {
          // get new settings
          theme = m_settings->value (global_icon_theme_index).toInt ();
        }
    }

  QIcon::setThemeName (global_all_icon_themes.at (theme));

  // set the required fallback search paths
  switch (theme)
    {
    case ICON_THEME_SYSTEM:
      m_icon_fallbacks << global_icon_paths.at (ICON_THEME_OCTAVE);
      m_icon_fallbacks << global_icon_paths.at (ICON_THEME_TANGO);
      break;
    case ICON_THEME_TANGO:
      m_icon_fallbacks << global_icon_paths.at (ICON_THEME_OCTAVE);
      break;
    case ICON_THEME_OCTAVE:
      m_icon_fallbacks << global_icon_paths.at (ICON_THEME_TANGO);
      break;
    }

  m_icon_fallbacks << global_icon_paths.at (ICON_THEME_CURSORS);
}

gui_settings * resource_manager::get_settings (void) const
{
  if (! m_settings)
    {
      QString msg (QT_TR_NOOP ("Octave has lost its settings.\n"
                               "This should not happen.\n\n"
                               "Please report this bug.\n\n"
                               "Octave GUI must be closed now."));

      QMessageBox::critical (nullptr,
                             QString (QT_TR_NOOP ("Octave Critical Error")),
                             msg);
      exit (1);
    }

  return m_settings;
}

gui_settings * resource_manager::get_default_settings (void) const
{
  if (! m_default_settings)
    {
      QString msg (QT_TR_NOOP ("Octave has lost its default settings.\n"
                               "This should not happen.\n"
                               "Please report this bug.\n\n"
                               "Octave GUI must be closed now."));

      QMessageBox::critical (nullptr,
                             QString (QT_TR_NOOP ("Octave Critical Error")),
                             msg);
      exit (1);
    }

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
  QString default_family;

  // Get all available fixed width fonts via a font combobox
  QFontComboBox font_combo_box;
  font_combo_box.setFontFilters (QFontComboBox::MonospacedFonts);
  QStringList fonts;

  for (int index = 0; index < font_combo_box.count(); index++)
    fonts << font_combo_box.itemText(index);

#if defined (Q_OS_MAC)
  // Use hard coded default on macOS, since selection of fixed width
  // default font is unreliable (see bug #59128).
  // Test for macOS default fixed width font
  if (fonts.contains (global_mono_font.def.toString ()))
    default_family = global_mono_font.def.toString ();
#endif

  // If default font is still empty (on all other platforms or
  // if macOS default font is not available): use QFontDatabase
  if (default_family.isEmpty ())
    {
      // Get the system's default monospaced font
      QFont fixed_font = QFontDatabase::systemFont (QFontDatabase::FixedFont);
      default_family = fixed_font.defaultFamily ();

      // Since this might be unreliable, test all available fixed width fonts
      if (! fonts.contains (default_family))
        {
          // Font returned by QFontDatabase is not in fixed fonts list.
          // Fallback: take first from this list
          default_family = fonts[0];
        }
    }

  // Test env variable which has preference
  std::string env_default_family = sys::env::getenv ("OCTAVE_DEFAULT_FONT");
  if (! env_default_family.empty ())
    default_family = QString::fromStdString (env_default_family);

  return default_family;
}

QStringList resource_manager::get_default_font (void)
{
  QString default_family = get_default_font_family ();

  // determine the fefault font size of the system
  // FIXME: QApplication::font () does not return the monospace font,
  //        but the size should be probably near to the monospace font
  QFont font = QApplication::font ();

  int font_size = font.pointSize ();
  if (font_size == -1)
    font_size = static_cast <int> (std::floor(font.pointSizeF ()));

  // check for valid font size, otherwise take default 10
  QString default_font_size = "10";
  if (font_size > 0)
    default_font_size = QString::number (font_size);

  std::string env_default_font_size
    = sys::env::getenv ("OCTAVE_DEFAULT_FONT_SIZE");

  if (! env_default_font_size.empty ())
    default_font_size = QString::fromStdString (env_default_font_size);

  QStringList result;
  result << default_family;
  result << default_font_size;
  return result;
}

void resource_manager::reload_settings (void)
{
  // Declare some empty options, which may be set at first startup for
  // writing them into the newly created settings file
  QString custom_editor;
  QStringList def_font;

  // Check whether the settings file does not yet exist
  if (! QFile::exists (m_settings_file))
    {
      // Get the default font (for terminal)
      def_font = get_default_font ();

      // Get a custom editor defined as env variable
      std::string env_default_editor
        = sys::env::getenv ("OCTAVE_DEFAULT_EDITOR");

      if (! env_default_editor.empty ())
        custom_editor = QString::fromStdString (env_default_editor);
    }

  set_settings (m_settings_file);

  // Write some settings that were dynamically determined at first startup
  if (m_settings)
    {
      // Custom editor
      if (! custom_editor.isEmpty ())
        m_settings->setValue (global_custom_editor.key, custom_editor);

      // Default monospace font for the terminal
      if (def_font.count () > 1)
        {
          m_settings->setValue (cs_font.key, def_font[0]);
          m_settings->setValue (cs_font_size.key, def_font[1].toInt ());
        }

      // Write the default monospace font into the settings for later use by
      // console and editor as fallbacks of their font preferences.
      m_settings->setValue (global_mono_font.key, get_default_font_family ());
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
                                            gui_settings *settings,
                                            int mode, int def)
{
  // Test whether the settings for lexer is already contained in the
  // given gui settings file. If yes, load them, if not copy them from the
  // default settings file.
  // This is useful when a new language support is implemented and the
  // existing settings file is used (which is of course the common case).
  int m = mode;
  if (m > 1)
    m = 1;

  QString group ("Scintilla" + settings_color_modes_ext[m]);

  settings->beginGroup (group);
  settings->beginGroup (lexer->language ());

  QStringList lexer_keys = settings->allKeys ();

  settings->endGroup ();
  settings->endGroup ();

  if (def == settings_reload_default_colors_flag || lexer_keys.count () == 0)
    {
      // We have to reload the default values or no Lexer keys found:
      // If mode == 0, take all settings except font from default lexer
      // If Mode == 1, take all settings except font from default lexer
      //               and convert the color by inverting the lightness

      // Get the default font
      QStringList def_font = get_default_font ();
      QFont df (def_font[0], def_font[1].toInt ());
      QFont dfa = copy_font_attributes (lexer->defaultFont (), df);
      lexer->setDefaultFont (dfa);

      QColor c, p;

      int styles[ed_max_lexer_styles];  // array for saving valid styles
      int max_style = get_valid_lexer_styles (lexer, styles);

      for (int i = 0; i < max_style; i++)
        {
          c = settings->get_color_value (QVariant (lexer->color (styles[i])), m);
          lexer->setColor (c, styles[i]);
          p = settings->get_color_value (QVariant (lexer->paper (styles[i])), m);
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
          lexer->writeSettings (*settings, group_str.c_str ());
          settings->sync ();
        }
    }
  else
    {
      // Found lexer keys, read the settings
      const std::string group_str = group.toStdString ();
      lexer->readSettings (*settings, group_str.c_str ());
    }
}
#endif

void resource_manager::set_settings (const QString& file)
{
  delete m_settings;
  m_settings = new gui_settings (file, QSettings::IniFormat);

  if (m_settings->status () == QSettings::NoError)
    {
      // Test usability (force file to be really created)
      m_settings->setValue ("dummy", 0);
      m_settings->sync ();
    }

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
  else
    m_settings->remove ("dummy"); // Remove test entry
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

QIcon resource_manager::icon (const QString& icon_name, bool octave_only,
                              const QString& icon_alt_name)
{
  if (octave_only)
    return QIcon (global_icon_paths.at (ICON_THEME_OCTAVE) + icon_name + ".png");

  if (QIcon::hasThemeIcon (icon_name))
    return QIcon (QIcon::fromTheme (icon_name));
  else if ((! icon_alt_name.isEmpty ()) && QIcon::hasThemeIcon (icon_alt_name))
    return QIcon (QIcon::fromTheme (icon_alt_name));

  for (int i = 0; i < m_icon_fallbacks.length (); i++ )
    {
      QString icon_file (m_icon_fallbacks.at (i) + icon_name + ".png");
      if (QFile (icon_file).exists ())
        return QIcon (icon_file);
    }

  //QIcon::setThemeName (current_theme);
  return QIcon ();
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

OCTAVE_END_NAMESPACE(octave)
