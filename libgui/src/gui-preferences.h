////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2017-2023 The Octave Project Developers
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

#if ! defined (octave_gui_preferences_h)
#define octave_gui_preferences_h 1

#include <QStringList>
#include <QStyle>
#include <QTabWidget>
#include <QTextCodec>
#include <QVariant>

// Structure for the definition of pairs: key and default value

struct gui_pref
{
  gui_pref (const QString& key_arg, const QVariant& def_arg,
            const bool ignore_arg = false)
    : key (key_arg), def (def_arg), ignore (ignore_arg)
  { }

  // No copying!

  gui_pref (const gui_pref&) = delete;

  gui_pref& operator = (const gui_pref&) = delete;

  ~gui_pref (void) = default;

  const QString key;   // the key name
  const QVariant def;  // the default value
  const bool ignore;   // when true, ignore, i.e. always take default
};

// The version for shortcuts, where the default value is stored as a
// combination of Qt:Keys (resutling in an unsigend int, when added)
// or as one of the predefined standard key sequences.

const QString sc_group ("shortcuts/");  // group name is handled separately

struct sc_pref
{
  sc_pref (const QString& key_arg, Qt::Key def_arg)
    : key (key_arg), def (static_cast<unsigned int> (def_arg)),
      def_std (QKeySequence::UnknownKey)
  { }

  sc_pref (const QString& key_arg, unsigned int def_arg)
    : key (key_arg), def (def_arg), def_std (QKeySequence::UnknownKey)
  { }

  sc_pref (const QString& key_arg, QKeySequence::StandardKey def_std_arg)
    : key (key_arg), def (0), def_std (def_std_arg)
  { }

  // No copying!

  sc_pref (const sc_pref&) = delete;

  sc_pref& operator = (const sc_pref&) = delete;

  ~sc_pref (void) = default;

  const QString key;                        // the key name
  const unsigned int def;                   // the default as key
  const QKeySequence::StandardKey def_std;  // the default as standard key
};

#endif
