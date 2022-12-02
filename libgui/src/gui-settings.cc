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

#include <QApplication>
#include <QFile>
#include <QSettings>

#include "gui-preferences-global.h"
#include "gui-settings.h"

namespace octave
{

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
    int theme = global_icon_theme_index.def.toInt ();

    // check for new and old setting and use old if required
    if (! contains (global_icon_theme_index.key))
      {
        // new pref does not exist
        if (value (global_icon_theme).toBool ())
          theme = ICON_THEME_SYSTEM;
        else
          theme = ICON_THEME_OCTAVE;
        setValue (global_icon_theme_index.key, theme);  // add new
        remove (global_icon_theme.key); // remove deprecated key
      }

   QIcon::setThemeName (global_all_icon_themes.at (theme));

   QStringList icon_fallbacks;

   // set the required fallback search paths
   switch (theme)
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
}
