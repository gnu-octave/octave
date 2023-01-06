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

// FIXME: Would it be possible/make sense to merge gui_pref and sc_pref
// into a single class or to at least have them derived from a common
// base class so we could have one global hash table that contains all
// of them?

// Structure for the definition of pairs: key and default value

class gui_pref
{
public:

  // Default constructed gui_pref objects are invalid, but we need this
  // to create QHash objects that contain gui_pref objects.  No invalid
  // gui_pref objects should acutally be used.

  gui_pref (void) = default;

  gui_pref (const QString& settings_key, const QVariant& def,
            bool ignore = false);

  gui_pref (const gui_pref&) = default;

  gui_pref& operator = (const gui_pref&) = default;

  ~gui_pref (void) = default;

  QString settings_key (void) const { return m_settings_key; }
  QVariant def (void) const { return m_def; }
  bool ignore (void) const { return m_ignore; }

private:

  // The settings key name.
  QString m_settings_key;

  // The default value.
  QVariant m_def;

  // TRUE means always take the default.
  bool m_ignore;
};

// FIXME: Is there a better/more modern way to manage this data than to
// have this style of singleton class?

// Allow lookup of gui_pref objects when we don't know the name at
// compile time.

class all_gui_preferences
{
public:

  all_gui_preferences (void) = default;

  // No copying!

  all_gui_preferences (const all_gui_preferences&) = delete;

  all_gui_preferences&
  operator = (const all_gui_preferences&) = delete;

  ~all_gui_preferences (void) = default;

  static void insert (const QString& settings_key, const gui_pref& pref);

  static const gui_pref value (const QString& settings_key);

  static QStringList keys (void);

private:

  // Map from shortcut identifier (settings key) to gui_pref object.
  QHash <QString, gui_pref> m_hash;

  void do_insert (const QString& settings_key, const gui_pref& pref);

  const gui_pref do_value (const QString& settings_key) const;

  QStringList do_keys (void) const;

  static void ensure_instance (void);

  // Map from shortcut identifier (settings key) to sc_pref object.
  static all_gui_preferences *s_instance;
};

// The version for shortcuts, where the default value is stored as a
// combination of Qt:Keys (resutling in an unsigend int, when added)
// or as one of the predefined standard key sequences.

class sc_pref
{
public:

  // Default constructed sc_pref objects are invalid, but we need this
  // to create QHash objects that contain sc_pref objects.  No invalid
  // sc_pref objects should acutally be used.

  sc_pref (void) = default;

  sc_pref (const QString& description, const QString& settings_key,
           Qt::Key def);

  sc_pref (const QString& description_arg, const QString& settings_key,
           unsigned int def);

  sc_pref (const QString& description_arg, const QString& settings_key,
           QKeySequence::StandardKey def_std);

  sc_pref (const sc_pref&) = default;

  sc_pref& operator = (const sc_pref&) = default;

  ~sc_pref (void) = default;

  QString description (void) const { return m_description; }

  QString settings_key (void) const { return m_settings_key; }

  unsigned int def (void) const { return m_def; }

  QKeySequence::StandardKey def_std (void) const { return m_def_std; }

  QKeySequence def_value (void) const;

  QString def_text (void) const;

private:

  // Description of the shortcut.
  QString m_description;

  // The settings key name.
  QString m_settings_key;

  // The default as key.
  unsigned int m_def;

  // The default as standard key.
  QKeySequence::StandardKey m_def_std;
};

// FIXME: Is there a better/more modern way to manage this data than to
// have this style of singleton class?

// Allow lookup of sc_pref objects when we don't know the name at
// compile time.

class all_shortcut_preferences
{
public:

  all_shortcut_preferences (void) = default;

  // No copying!

  all_shortcut_preferences (const all_shortcut_preferences&) = delete;

  all_shortcut_preferences&
  operator = (const all_shortcut_preferences&) = delete;

  ~all_shortcut_preferences (void) = default;

  static void insert (const QString& settings_key, const sc_pref& scpref);

  static const sc_pref value (const QString& settings_key);

  static QStringList keys (void);

private:

  // Map from shortcut identifier (settings key) to sc_pref object.
  QHash <QString, sc_pref> m_hash;

  void do_insert (const QString& settings_key, const sc_pref& scpref);

  const sc_pref do_value (const QString& settings_key) const;

  QStringList do_keys (void) const;

  static void ensure_instance (void);

  // Map from shortcut identifier (settings key) to sc_pref object.
  static all_shortcut_preferences *s_instance;
};

#endif
