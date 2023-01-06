////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2019-2023 The Octave Project Developers
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

#include "octave-config.h"

#include <QSettings>

#include "gui-preferences.h"

OCTAVE_BEGIN_NAMESPACE(octave)

class gui_settings : public QSettings
{
  Q_OBJECT

public:

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

};

OCTAVE_END_NAMESPACE(octave)

// Some constants used in the preferences of several widgets and therefore
// defined globally here

// Constants for handling different color schemes
const QColor settings_color_no_change (255, 0, 255);
const QStringList settings_color_modes_ext (QStringList () << "" << "_2");
const int settings_reload_default_colors_flag = -1;

#endif
