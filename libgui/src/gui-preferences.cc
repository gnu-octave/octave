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

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include "gui-preferences-sc.h"
#include "gui-preferences.h"

gui_pref::gui_pref (const QString& settings_key, const QVariant& def,
                    bool ignore)
  : m_settings_key (settings_key), m_def (def), m_ignore (ignore)
{
  all_gui_preferences::insert (settings_key, *this);
}

all_gui_preferences *all_gui_preferences::s_instance = nullptr;

void all_gui_preferences::insert (const QString& settings_key,
                                  const gui_pref& pref)
{
  ensure_instance ();

  s_instance->do_insert (settings_key, pref);
}

const gui_pref all_gui_preferences::value (const QString& settings_key)
{
  ensure_instance ();

  return s_instance->do_value (settings_key);
}

QStringList all_gui_preferences::keys (void)
{
  ensure_instance ();

  return s_instance->do_keys ();
}

void all_gui_preferences::do_insert (const QString& settings_key,
                                     const gui_pref& pref)
{
  m_hash.insert (settings_key, pref);
}

const gui_pref
all_gui_preferences::do_value (const QString& settings_key) const
{
  return m_hash.value (settings_key);
}

QStringList all_gui_preferences::do_keys (void) const
{
  return m_hash.keys ();
}

void all_gui_preferences::ensure_instance (void)
{
  if (! s_instance)
    s_instance = new all_gui_preferences ();
}

sc_pref::sc_pref (const QString& description, const QString& settings_key,
                  Qt::Key def)
  : m_description (description), m_settings_key (settings_key), m_def (def),
    m_def_std (QKeySequence::UnknownKey)
{
  all_shortcut_preferences::insert (settings_key, *this);
}

sc_pref::sc_pref (const QString& description, const QString& settings_key,
                  unsigned int def)
  : m_description (description), m_settings_key (settings_key), m_def (def),
    m_def_std (QKeySequence::UnknownKey)
{
  all_shortcut_preferences::insert (settings_key, *this);
}

sc_pref::sc_pref (const QString& description, const QString& settings_key,
                  QKeySequence::StandardKey def_std)
  : m_description (description), m_settings_key (settings_key), m_def (0),
    m_def_std (def_std)
{
  all_shortcut_preferences::insert (settings_key, *this);
}

QKeySequence sc_pref::def_value (void) const
{
  QKeySequence key_seq = QKeySequence ();

  if (m_def)
    key_seq = QKeySequence (m_def);
  else if (m_def_std != QKeySequence::UnknownKey)
    key_seq = QKeySequence (m_def_std);

  return key_seq;
}

QString sc_pref::def_text (void) const
{
  return def_value ().toString ();
}

all_shortcut_preferences *all_shortcut_preferences::s_instance = nullptr;

void all_shortcut_preferences::insert (const QString& settings_key,
                                       const sc_pref& scpref)
{
  ensure_instance ();

  s_instance->do_insert (settings_key, scpref);
}

const sc_pref all_shortcut_preferences::value (const QString& settings_key)
{
  ensure_instance ();

  return s_instance->do_value (settings_key);
}

QStringList all_shortcut_preferences::keys (void)
{
  ensure_instance ();

  return s_instance->do_keys ();
}

void all_shortcut_preferences::do_insert (const QString& settings_key,
                                          const sc_pref& scpref)
{
  m_hash.insert (settings_key, scpref);
}

const sc_pref
all_shortcut_preferences::do_value (const QString& settings_key) const
{
  return m_hash.value (settings_key);
}

QStringList all_shortcut_preferences::do_keys (void) const
{
  return m_hash.keys ();
}

void all_shortcut_preferences::ensure_instance (void)
{
  if (! s_instance)
    s_instance = new all_shortcut_preferences ();
}
