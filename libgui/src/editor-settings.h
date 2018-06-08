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

// Octave comment strings
const QString oct_comment_str_old ("editor/octave_comment_string");
const int oct_comment_str_old_d = 0;

const QString oct_comment_str ("editor/oct_comment_str");
const QString oct_uncomment_str ("editor/oct_uncomment_str");
const QString oct_last_comment_str ("editor/oct_last_comment_str");
const QStringList oct_comment_strings (QStringList () << "##" << "#" << "%"<< "%%" << "%!");
const int oct_comment_strings_count = 5;
const int oct_comment_str_d = 0;
const int oct_uncomment_str_d = 1 + 2 + 4 + 8;

#endif
