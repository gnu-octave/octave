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

#if ! defined (octave_gui_settings_h)
#define octave_gui_settings_h 1

#include <QIcon>
#include <QString>
#include <QSettings>

#include "gui-preferences.h"

namespace octave
{
  class gui_settings : public QSettings
  {
    Q_OBJECT

  public:

    // Location, name, and format of settings file determined by
    // settings in qt_application class construtor.

    gui_settings (QObject *parent = nullptr)
      : QSettings (parent)
    { }

    gui_settings (const QString& file_name, QSettings::Format format,
                  QObject *parent = nullptr)
      : QSettings (file_name, format, parent)
    { }

    gui_settings (QSettings::Format format, QSettings::Scope scope,
                  const QString& organization,
                  const QString& application = QString (),
                  QObject *parent = nullptr)
      : QSettings (format, scope, organization, application, parent)
    { }

    // No copying!

    gui_settings (const gui_settings&) = delete;

    gui_settings& operator = (const gui_settings&) = delete;

    ~gui_settings (void) = default;

    QString file_name (void) const;

    QString directory_name (void) const;

    using QSettings::value;

    QVariant value (const gui_pref& pref) const
    {
      if (pref.ignore)
        return pref.def;  // ignore the current pref and always use default

      return value (pref.key, pref.def);
    }

    /*!
      Reading a color from the given QVaraitn @p def taking different
      color modes into account. The default value for a second color mode
      @p mode=1 is deterimined from the standard default value @p mode=0
      by inverting the lightness
        \f{eqnarray*}{
           H_1 &=& H_0\\
           S_1 &=& S_0\\
           L_1 &=& 1.0 - 0.85 L_0    L_0 > 0.3
           L_1 &=& 1.0 - 0.70 L_0    L_0 < 0.3
        \f}

      @param def  Color default value given by a QVariant of QColor
                  or QPalette::ColorRole
      @param mode Color mode (currently 0 or 1, default is 0)

      @return Color as QColor
    */
    QColor get_color_value (const QVariant& def, int mode = 0) const;

    /*!
      Reading a color from the gui_settings taking possible color modes
      into account. The default value for a second color mode @p mode=1 is
      deterimined from the standard default value @p mode=0 by inverting
      the lightness (see get_color_value())

      @param pref gui preference (key string, default value); the default
                  value can be given by QColor or QPalette::ColorRole
      @param mode Color mode (currently 0 or 1, default is 0)

      @return Color as QColor
    */
    QColor color_value (const gui_pref& pref, int mode = 0) const;

    /*!
      Writing a color to the gui_settings taking possible color modes
      into account. When @p mode is not zero (standard mode), the
      extension related to the mode is appended to the settings key string

      @param pref gui preference where the color should be written
      @param color QColor to write to the settings
      @param mode Color mode (currently 0 or 1, default is 0)

    */
    void set_color_value (const gui_pref& pref, const QColor& color,
                          int mode = 0);

    QString sc_value (const sc_pref& pref) const;

    QKeySequence sc_def_value (const sc_pref& pref) const;

    // config_icon_theme, icon, get_default_font_family,
    // get_default_font, and possibly reload and check could be global
    // functions instead of member functions.  But at least for the icon
    // function, defining it as a member function means that we can
    // create a single gui_settings object and access multiple icon
    // objects rather than having to create a separate settings object
    // each time that an icon is needed.  OTOH, creating the base
    // QSettings object is supposed to be fast, so that may not matter.
    // Hmm.

    void config_icon_theme (void);

    QIcon icon (const QString& icon_name, bool octave_only = false,
                const QString& icon_alt_name = QString ());

    QString get_default_font_family (void);

    QStringList get_default_font (void);

    void reload (void);

  private:

    void check (void);
  };

}

// Some constants used in the preferences of several widgets and therefore
// defined globally here

// Constants for handling different color schemes
const QColor settings_color_no_change (255, 0, 255);
const QStringList settings_color_modes_ext (QStringList () << "" << "_2");
const int settings_reload_default_colors_flag = -1;

#endif

