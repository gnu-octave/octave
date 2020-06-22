////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2019-2020 The Octave Project Developers
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

#include <QSettings>

#include "gui-preferences.h"

namespace octave
{
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

    QString sc_value (const sc_pref& pref) const;

    QKeySequence sc_def_value (const sc_pref& pref) const;

  };
}

#endif

