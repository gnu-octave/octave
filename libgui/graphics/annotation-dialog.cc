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
#include "octave-qobject.h"

#include "QtHandlesUtils.h"
#include "annotation-dialog.h"
#include "ui-annotation-dialog.h"

using namespace octave;

annotation_dialog::annotation_dialog (octave::base_qobject& oct_qobj,
                                      QWidget *p, const octave_value_list& pr):
  QDialog (p), m_octave_qobj (oct_qobj), ui (new Ui::annotation_dialog)
{
  props = pr;

  init ();
}

void
annotation_dialog::init ()
{
  ui->setupUi (this);

  octave::resource_manager& rmgr = m_octave_qobj.get_resource_manager ();

  octave::gui_settings *settings = rmgr.get_settings ();

  // restore last geometry
  if (settings && settings->contains (gp_annotation_geometry.key))
    restoreGeometry (settings->value (gp_annotation_geometry).toByteArray ());

  // connect signals
  connect (ui->button_box, &QDialogButtonBox::clicked,
           this, &annotation_dialog::button_clicked);

  connect (ui->edit_string, &QLineEdit::textChanged,
           this, &annotation_dialog::edit_string_changed);

  connect (ui->btn_color, &QPushButton::clicked,
           this, &annotation_dialog::prompt_for_color);

  connect (ui->btn_background_color, &QPushButton::clicked,
           this, &annotation_dialog::prompt_for_color);

  connect (ui->btn_edge_color, &QPushButton::clicked,
           this, &annotation_dialog::prompt_for_color);

  // set gui element to default values
  ui->cb_fit_box_to_text->setChecked (true);
  ui->cb_horz_align->setCurrentIndex (ui->cb_horz_align->findText ("left"));
  ui->cb_vert_align->setCurrentIndex (ui->cb_vert_align->findText ("middle"));

  // set gui elements to any values from input properties
  set_gui_props ();
}

annotation_dialog::~annotation_dialog ()
{
  delete ui;
}

// internal slots

void
annotation_dialog::button_clicked (QAbstractButton *button)
{
  QDialogButtonBox::ButtonRole button_role
    = ui->button_box->buttonRole (button);

  octave::resource_manager& rmgr = m_octave_qobj.get_resource_manager ();

  octave::gui_settings *settings = rmgr.get_settings ();

  // save position
  if (settings)
    settings->setValue (gp_annotation_geometry.key, saveGeometry ());

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
  return props;
}

void
annotation_dialog::get_gui_props ()
{
  // set props to the values of the gui
  props = octave_value_list ();

  Matrix position(1, 4);
  position(0) = ui->sb_x->value ();
  position(1) = ui->sb_y->value ();
  position(2) = ui->sb_width->value ();
  position(3) = ui->sb_height->value ();
  props.append (ovl ("textbox", position));

  props.append (ovl ("string", ui->edit_string->text ().toStdString ()));
  props.append (ovl ("fitboxtotext",
                     ui->cb_fit_box_to_text->isChecked () ? "on" : "off"));

  // FIXME: only "normalized" units is selectable, change the code below
  //        once more units are added in the UI.
  std::string tmpstr;
  props.append (ovl ("units", "normalized"));

  tmpstr = (ui->cb_horz_align->currentIndex () == 0 ? "left" :
            (ui->cb_horz_align->currentIndex () == 1 ? "center" : "right"));
  props.append (ovl ("horizontalalignment", tmpstr));

  tmpstr = (ui->cb_vert_align->currentIndex () == 0 ? "top" :
            (ui->cb_horz_align->currentIndex () == 1 ? "middle" : "bottom"));
  props.append (ovl ("verticalalignment", tmpstr));

  tmpstr = ui->cb_font_name->currentText ().toStdString ();
  props.append (ovl ("fontname", tmpstr));

  props.append (ovl ("fontsize", ui->sb_font_size->value ()));
  props.append (ovl ("fontweight",
                     ui->cb_font_bold->isChecked () ? "bold" : "normal"));
  props.append (ovl ("fontangle",
                     ui->cb_font_italic->isChecked () ? "italic" : "normal"));
  props.append (ovl ("color", Utils::toRgb (ui->btn_color->palette ().
                     color (QPalette::Button))));

  // FIXME: only "none" linestyle is selectable, change the code bellow
  //        once more linestyles are added in the UI.
  props.append (ovl ("linestyle", "none"));
}

void
annotation_dialog::set_gui_props ()
{
  // set the gui to the values from the props
  octave_idx_type len = props.length ();

  for (int i=0; i<len/2; i++)
    {
      std::string name = props(i*2).string_value ();

      if (name == "textbox")
        {
          Matrix position = props(2*i +1).matrix_value ();
          int nels = position.numel ();
          if (nels >= 2)
            {
              ui->sb_x->setValue (position(0));
              ui->sb_y->setValue (position(1));
            }
          else
            {
              ui->sb_x->setValue (0);
              ui->sb_y->setValue (0);
            }
          if (nels >= 4)
            {
              ui->sb_width->setValue (position(2));
              ui->sb_height->setValue (position(3));
            }
          // FIXME: Should there be an else branch here?
          // In annotation.m "textbox" is forced to have a 4-elem vector.
        }
      else if (name == "string")
        {
          // FIXME: handle if is array of strings ?
          ui->edit_string->setText (props(2*i +1).string_value ().c_str ());
        }
      else if (name == "fitboxtotext")
        {
          ui->cb_fit_box_to_text->setChecked (props(1*i +1).string_value () == "on");
        }
      else if (name == "units")
        {
          ui->cb_units->setCurrentIndex
            (ui->cb_units->findText (props(1*i +1).string_value ().c_str ()));
        }
      else if (name == "horizontalalignment")
        {
          ui->cb_horz_align->setCurrentIndex
            (ui->cb_horz_align->findText (props(1*i +1).string_value ().c_str ()));
        }
      else if (name == "verticalalignment")
        {
          ui->cb_vert_align->setCurrentIndex
            (ui->cb_vert_align->findText (props(1*i +1).string_value ().c_str ()));
        }
      else if (name == "fontname")
        {
          ui->cb_vert_align->setCurrentIndex
            (ui->cb_font_name->findText (props(1*i +1).string_value ().c_str ()));
        }
      else if (name == "fontsize")
        {
          ui->sb_font_size->setValue (props(1*i +1).float_value ());
        }
      else if (name == "fontweight")
        {
          ui->cb_font_bold->setChecked (props(1*i +1).string_value () == "bold");
        }
      else if (name == "fontangle")
        {
          ui->cb_font_italic->setChecked (props(1*i +1).string_value () == "italic");
        }
      else if (name == "color")
        {
          QColor color;
          if (props(1*i +1).is_matrix_type ())
            color = Utils::fromRgb (props(2*i +1).matrix_value ());
          else
            color.setNamedColor (props(2*i +1).string_value ().c_str ());

          if (color.isValid ())
            ui->btn_color->setPalette (QPalette (color));
        }

    }

  edit_string_changed (ui->edit_string->text ());
}

void
annotation_dialog::edit_string_changed (const QString& str)
{
  ui->button_box->button (QDialogButtonBox::Ok)->setEnabled (str.length () > 0);
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
