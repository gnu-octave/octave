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

#if ! defined (octave_external_editor_interface_h)
#define octave_external_editor_interface_h 1

#include <QString>
#include <QWidget>

OCTAVE_BEGIN_NAMESPACE(octave)

class base_qobject;

class external_editor_interface : public QWidget
{
  Q_OBJECT

public:

  external_editor_interface (QWidget *main_win, base_qobject& oct_qobj);

  ~external_editor_interface (void) = default;

signals:

  void request_settings_dialog (const QString&);

public slots:

  bool call_custom_editor (const QString& file = QString (), int line = -1);

  void request_open_file (const QString& fileName,
                          const QString& encoding = QString (),
                          int line = -1, bool debug_pointer = false,
                          bool breakpoint_marker = false, bool insert = true,
                          const QString& cond = "");

  void request_new_file (const QString&);

  void handle_edit_file_request (const QString& file);

private:

  QString external_editor (void);

  base_qobject& m_octave_qobj;
};

OCTAVE_END_NAMESPACE(octave)

#endif
