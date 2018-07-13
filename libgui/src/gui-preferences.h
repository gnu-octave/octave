/*

Copyright (C) 2017-2018 Torsten <mttl@mailbox.de>

This file is part of Octave.

Octave is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<https://www.gnu.org/licenses/>.

*/

#if ! defined (octave_editor_settings_h)
#define octave_editor_settings_h 1

//#if defined (HAVE_CONFIG_H)
//#  include "config.h"
//#endif

#include <QStringList>
#include <QVariant>

// Structure for the definition of pairs: key and default value

struct gui_pref
{
  gui_pref (const QString& key_, QVariant def_): key (key_), def (def_) {}
  QString   key;  // the key name
  QVariant  def;  // the default value
};

// Editor preferences

// Octave comment strings
const gui_pref ed_comment_str_old = gui_pref ("editor/octave_comment_string", QVariant (0));
const gui_pref ed_comment_str ("editor/oct_comment_str", QVariant (0));
const gui_pref ed_uncomment_str ("editor/oct_uncomment_str", QVariant (1 + 2 + 4 + 8));

const QString ed_last_comment_str ("editor/oct_last_comment_str");
const QStringList ed_comment_strings (QStringList () << "##" << "#" << "%"<< "%%" << "%!");
const int ed_comment_strings_count = 5;

// File handling


#endif
