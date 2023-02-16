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

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <QColorDialog>
#include <QPushButton>
#include <QPalette>

#include "gui-settings.h"
#include "gui-preferences-gp.h"
#include "gui-settings.h"

#include "QtHandlesUtils.h"
#include "annotation-dialog.h"
#include "ui-annotation-dialog.h"

annotation_dialog::annotation_dialog (QWidget *p, const octave_value_list& pr):
  QDialog (p), m_ui (new Ui::annotation_dialog)
{
  m_props = pr;

  init ();
}

void
annotation_dialog::init ()
{
  m_ui->setupUi (this);

  octave::gui_settings settings;

  // restore last geometry
  if (settings.contains (gp_annotation_geometry.settings_key ()))
    restoreGeometry (settings.byte_array_value (gp_annotation_geometry));

  // connect signals
  connect (m_ui->button_box, &QDialogButtonBox::clicked,
           this, &annotation_dialog::button_clicked);

  connect (m_ui->edit_string, &QLineEdit::textChanged,
           this, &annotation_dialog::edit_string_changed);

  connect (m_ui->btn_color, &QPushButton::clicked,
           this, &annotation_dialog::prompt_for_color);

  connect (m_ui->btn_background_color, &QPushButton::clicked,
           this, &annotation_dialog::prompt_for_color);

  connect (m_ui->btn_edge_color, &QPushButton::clicked,
           this, &annotation_dialog::prompt_for_color);

  // set gui element to default values
  m_ui->cb_fit_box_to_text->setChecked (true);
  m_ui->cb_horz_align->setCurrentIndex (m_ui->cb_horz_align->findText ("left"));
  m_ui->cb_vert_align->setCurrentIndex (m_ui->cb_vert_align->findText ("middle"));

  // set gui elements to any values from input properties
  set_gui_props ();
}

annotation_dialog::~annotation_dialog ()
{
  delete m_ui;
}

// internal slots

void
annotation_dialog::button_clicked (QAbstractButton *button)
{
  QDialogButtonBox::ButtonRole button_role
    = m_ui->button_box->buttonRole (button);

  octave::gui_settings settings;

  // save position
  settings.setValue (gp_annotation_geometry.settings_key (), saveGeometry ());

  if (button_role == QDialogButtonBox::ApplyRole
      || button_role == QDialogButtonBox::AcceptRole)
    {
      get_gui_props ();
    }

  if (button_role == QDialogButtonBox::RejectRole
      || button_role == QDialogButtonBox::AcceptRole)
    close ();
}

octave_value_list
annotation_dialog::get_properties () const
{
  return m_props;
}

void
annotation_dialog::get_gui_props ()
{
  // set props to the values of the gui
  m_props = octave_value_list ();

  Matrix position(1, 4);
  position(0) = m_ui->sb_x->value ();
  position(1) = m_ui->sb_y->value ();
  position(2) = m_ui->sb_width->value ();
  position(3) = m_ui->sb_height->value ();
  m_props.append (ovl ("textbox", position));

  m_props.append (ovl ("string", m_ui->edit_string->text ().toStdString ()));
  m_props.append (ovl ("fitboxtotext",
                     m_ui->cb_fit_box_to_text->isChecked () ? "on" : "off"));

  // FIXME: only "normalized" units is selectable, change the code below
  //        once more units are added in the UI.
  std::string tmpstr;
  m_props.append (ovl ("units", "normalized"));

  tmpstr = (m_ui->cb_horz_align->currentIndex () == 0 ? "left" :
            (m_ui->cb_horz_align->currentIndex () == 1 ? "center" : "right"));
  m_props.append (ovl ("horizontalalignment", tmpstr));

  tmpstr = (m_ui->cb_vert_align->currentIndex () == 0 ? "top" :
            (m_ui->cb_horz_align->currentIndex () == 1 ? "middle" : "bottom"));
  m_props.append (ovl ("verticalalignment", tmpstr));

  tmpstr = m_ui->cb_font_name->currentText ().toStdString ();
  m_props.append (ovl ("fontname", tmpstr));

  m_props.append (ovl ("fontsize", m_ui->sb_font_size->value ()));
  m_props.append (ovl ("fontweight",
                     m_ui->cb_font_bold->isChecked () ? "bold" : "normal"));
  m_props.append (ovl ("fontangle",
                     m_ui->cb_font_italic->isChecked () ? "italic" : "normal"));
  m_props.append (ovl ("color", octave::Utils::toRgb (m_ui->btn_color->palette ().
                     color (QPalette::Button))));

  // FIXME: only "none" linestyle is selectable, change the code bellow
  //        once more linestyles are added in the UI.
  m_props.append (ovl ("linestyle", "none"));
}

void
annotation_dialog::set_gui_props ()
{
  // set the gui to the values from the props
  octave_idx_type len = m_props.length ();

  for (int i=0; i<len/2; i++)
    {
      std::string name = m_props(i*2).string_value ();

      if (name == "textbox")
        {
          Matrix position = m_props(2*i +1).matrix_value ();
          int nels = position.numel ();
          if (nels >= 2)
            {
              m_ui->sb_x->setValue (position(0));
              m_ui->sb_y->setValue (position(1));
            }
          else
            {
              m_ui->sb_x->setValue (0);
              m_ui->sb_y->setValue (0);
            }
          if (nels >= 4)
            {
              m_ui->sb_width->setValue (position(2));
              m_ui->sb_height->setValue (position(3));
            }
          // FIXME: Should there be an else branch here?
          // In annotation.m "textbox" is forced to have a 4-elem vector.
        }
      else if (name == "string")
        {
          // FIXME: handle if is array of strings ?
          m_ui->edit_string->setText (m_props(2*i +1).string_value ().c_str ());
        }
      else if (name == "fitboxtotext")
        {
          m_ui->cb_fit_box_to_text->setChecked (m_props(1*i +1).string_value () == "on");
        }
      else if (name == "units")
        {
          m_ui->cb_units->setCurrentIndex
            (m_ui->cb_units->findText (m_props(1*i +1).string_value ().c_str ()));
        }
      else if (name == "horizontalalignment")
        {
          m_ui->cb_horz_align->setCurrentIndex
            (m_ui->cb_horz_align->findText (m_props(1*i +1).string_value ().c_str ()));
        }
      else if (name == "verticalalignment")
        {
          m_ui->cb_vert_align->setCurrentIndex
            (m_ui->cb_vert_align->findText (m_props(1*i +1).string_value ().c_str ()));
        }
      else if (name == "fontname")
        {
          m_ui->cb_vert_align->setCurrentIndex
            (m_ui->cb_font_name->findText (m_props(1*i +1).string_value ().c_str ()));
        }
      else if (name == "fontsize")
        {
          m_ui->sb_font_size->setValue (m_props(1*i +1).float_value ());
        }
      else if (name == "fontweight")
        {
          m_ui->cb_font_bold->setChecked (m_props(1*i +1).string_value () == "bold");
        }
      else if (name == "fontangle")
        {
          m_ui->cb_font_italic->setChecked (m_props(1*i +1).string_value () == "italic");
        }
      else if (name == "color")
        {
          QColor color;
          if (m_props(1*i +1).is_matrix_type ())
            color = octave::Utils::fromRgb (m_props(2*i +1).matrix_value ());
          else
            color.setNamedColor (m_props(2*i +1).string_value ().c_str ());

          if (color.isValid ())
            m_ui->btn_color->setPalette (QPalette (color));
        }

    }

  edit_string_changed (m_ui->edit_string->text ());
}

void
annotation_dialog::edit_string_changed (const QString& str)
{
  m_ui->button_box->button (QDialogButtonBox::Ok)->setEnabled (str.length () > 0);
}

void
annotation_dialog::prompt_for_color ()
{
  QWidget *widg = dynamic_cast<QWidget *> (sender ());
  if (widg)
    {
      QColor color = widg->palette ().color (QPalette::Button);

      color = QColorDialog::getColor (color, this);

      if (color.isValid ())
        {
          widg->setPalette (QPalette (color));

          QString css = QString ("background-color: %1; border: 1px solid %2;")
                        .arg (color.name ())
                        .arg ("#000000");

          widg->setStyleSheet (css);
          widg->update ();
        }
    }
}
