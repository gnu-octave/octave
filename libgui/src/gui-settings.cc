////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2019-2022 The Octave Project Developers
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

#include <cmath>

#include <QApplication>
#include <QFile>
#include <QFileInfo>
#include <QFontDatabase>
#include <QMessageBox>
#include <QSettings>
#include <QString>
#include <QStringList>

#include "gui-preferences-cs.h"
#include "gui-preferences-global.h"
#include "gui-settings.h"

#include "oct-env.h"

OCTAVE_BEGIN_NAMESPACE(octave)

  QString gui_settings::file_name (void) const
  {
    return fileName ();
  }

  QString gui_settings::directory_name (void) const
  {
    QFileInfo sfile (fileName ());

    return sfile.absolutePath ();
  }

  QColor gui_settings::get_color_value (const QVariant& def, int mode) const
  {
    QColor default_color;

    // Determine whether the default value in pref is given as
    // QPalette::ColorRole or as QColor
    if (def.canConvert (QMetaType::QColor))
      default_color = def.value<QColor> ();
    else
      {
        // The default colors are given as color roles for
        // the application's palette
        default_color = QApplication::palette ().color
                        (static_cast<QPalette::ColorRole> (def.toInt ()));
                  // FIXME: use value<QPalette::ColorRole> instead of static cast after
                  //        dropping support of Qt 5.4
      }

    if ((mode == 1) && (default_color != settings_color_no_change))
      {
        // In second mode, determine the default color from the first mode
        qreal h, s, l, a;
        default_color.getHslF (&h, &s, &l, &a);
        qreal l_new = 1.0-l*0.85;
        if (l < 0.3)
          l_new = 1.0-l*0.7;  // convert darker into lighter colors
        default_color.setHslF (h, s, l_new, a);
      }

    return default_color;
  }

  QColor gui_settings::color_value (const gui_pref& pref, int mode) const
  {
    QColor default_color = get_color_value (pref.def, mode);

    return value (pref.key + settings_color_modes_ext[mode],
                  QVariant (default_color)).value<QColor> ();
  }

  void gui_settings::set_color_value (const gui_pref& pref,
                                      const QColor& color, int mode)
  {
    int m = mode;
    if (m > 1)
      m = 1;

    setValue (pref.key + settings_color_modes_ext[m], QVariant (color));
  }

  QString gui_settings::sc_value (const sc_pref& pref) const
  {
    QKeySequence key_seq = sc_def_value (pref);

    // Get the value from the settings where the key sequences are stored
    // as strings
    return value (sc_group + pref.key, key_seq.toString ()).toString ();
  }

  QKeySequence gui_settings::sc_def_value (const sc_pref& pref) const
  {
    QKeySequence key_seq = QKeySequence ();

    // Check, which of the elements for the default value in the sc_pref
    // structure has a valid value and take this as default.  If both
    // elements are not valid, leave the key sequence empty
    if (pref.def)
      key_seq = QKeySequence (pref.def);
    else if (pref.def_std != QKeySequence::UnknownKey)
      key_seq = QKeySequence (pref.def_std);

    return key_seq;
  }

  void gui_settings::config_icon_theme (void)
  {
    int theme_index;

    if (contains (global_icon_theme_index.key))
      theme_index = value (global_icon_theme_index).toInt ();
    else
      {
        // New pref does not exist.  Use old if required.  Add new and
        // remove deprecated key.

        if (value (global_icon_theme).toBool ())
          theme_index = ICON_THEME_SYSTEM;
        else
          theme_index = ICON_THEME_OCTAVE;

        setValue (global_icon_theme_index.key, theme_index);
        remove (global_icon_theme.key);
      }

    QIcon::setThemeName (global_all_icon_themes.at (theme_index));

    QStringList icon_fallbacks;

    // Set the required fallback search paths.

    switch (theme_index)
      {
      case ICON_THEME_SYSTEM:
        icon_fallbacks << global_icon_paths.at (ICON_THEME_OCTAVE);
        icon_fallbacks << global_icon_paths.at (ICON_THEME_TANGO);
        break;
      case ICON_THEME_TANGO:
        icon_fallbacks << global_icon_paths.at (ICON_THEME_OCTAVE);
        break;
      case ICON_THEME_OCTAVE:
        icon_fallbacks << global_icon_paths.at (ICON_THEME_TANGO);
        break;
      }

    icon_fallbacks << global_icon_paths.at (ICON_THEME_CURSORS);

    setValue (global_icon_fallbacks.key, icon_fallbacks);
  }

  QIcon gui_settings::icon (const QString& icon_name, bool octave_only,
                            const QString& icon_alt_name)
  {
    if (octave_only)
      return QIcon (global_icon_paths.at (ICON_THEME_OCTAVE) + icon_name + ".png");

    if (QIcon::hasThemeIcon (icon_name))
      return QIcon (QIcon::fromTheme (icon_name));
    else if ((! icon_alt_name.isEmpty ()) && QIcon::hasThemeIcon (icon_alt_name))
      return QIcon (QIcon::fromTheme (icon_alt_name));

    QStringList icon_fallbacks
      = value (global_icon_fallbacks.key).toStringList ();

    for (int i = 0; i < icon_fallbacks.length (); i++ )
      {
        QString icon_file (icon_fallbacks.at (i) + icon_name + ".png");
        if (QFile (icon_file).exists ())
          return QIcon (icon_file);
      }

      //QIcon::setThemeName (current_theme);
      return QIcon ();
  }

  QString gui_settings::get_default_font_family (void)
  {
    // Get all available fixed width fonts from the Qt font database.

    QFontDatabase font_database;
    QStringList fonts;

    for (QString font : font_database.families ())
      {
        if (font_database.isFixedPitch (font))
          fonts << font;
      }

    QString default_family;

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

  QStringList gui_settings::get_default_font (void)
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

  void gui_settings::reload (void)
  {
    // Declare some empty options, which may be set at first startup for
    // writing them into the newly created settings file
    QString custom_editor;
    QStringList def_font;

    // Check whether the settings file does not yet exist
    if (! QFile::exists (file_name ()))
      {
        // Get the default font (for terminal)
        def_font = get_default_font ();

        // Get a custom editor defined as env variable
        std::string env_default_editor
          = sys::env::getenv ("OCTAVE_DEFAULT_EDITOR");

        if (! env_default_editor.empty ())
          custom_editor = QString::fromStdString (env_default_editor);
      }

    check ();

    // Write some settings that were dynamically determined at first startup

    // Custom editor
    if (! custom_editor.isEmpty ())
      setValue (global_custom_editor.key, custom_editor);

    // Default monospace font for the terminal
    if (def_font.count () > 1)
      {
        setValue (cs_font.key, def_font[0]);
        setValue (cs_font_size.key, def_font[1].toInt ());
      }

    // Write the default monospace font into the settings for later use by
    // console and editor as fallbacks of their font preferences.
    setValue (global_mono_font.key, get_default_font_family ());
  }

  void gui_settings::check (void)
  {
    if (status () == QSettings::NoError)
      {
        // Test usability (force file to be really created)
        setValue ("dummy", 0);
        sync ();
      }

    if (! (QFile::exists (file_name ())
           && isWritable ()
           && status () == QSettings::NoError))
      {
        QString msg
          = QString (QT_TR_NOOP ("Error %1 creating the settings file\n%2\n"
                                 "Make sure you have read and write permissions to\n%3\n\n"
                                 "Octave GUI must be closed now."));

        QMessageBox::critical (nullptr,
                               QString (QT_TR_NOOP ("Octave Critical Error")),
                               msg.arg (status ())
                                  .arg (file_name ())
                                  .arg (directory_name ()));

        exit (1);
      }
    else
      remove ("dummy");  // Remove test entry
  }

OCTAVE_END_NAMESPACE(octave)
