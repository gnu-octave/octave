////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2016-2023 The Octave Project Developers
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

#if ! defined (octave_annotation_dialog_h)
#define octave_annotation_dialog_h 1

#include <QDialog>
#include <QLineEdit>
#include <QAbstractButton>

#include "ovl.h"

OCTAVE_BEGIN_NAMESPACE(octave)
class base_qobject;
OCTAVE_END_NAMESPACE(octave)

OCTAVE_BEGIN_NAMESPACE(Ui)
class annotation_dialog;
OCTAVE_END_NAMESPACE(Ui)

class annotation_dialog : public QDialog
{
  Q_OBJECT
public:
  annotation_dialog (octave::base_qobject& oct_qobj, QWidget *parent,
                     const octave_value_list& pr);
  ~annotation_dialog ();

  octave_value_list get_properties () const;

private slots:
  // slots for dialog's buttons
  void button_clicked (QAbstractButton *button);
  void edit_string_changed (const QString& str);
  void prompt_for_color ();

private:
  void init ();

  void get_gui_props ();
  void set_gui_props ();

  octave::base_qobject& m_octave_qobj;
  Ui::annotation_dialog *ui;
  octave_value_list props;
};

#endif
