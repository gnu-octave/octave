////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2019-2023 The Octave Project Developers
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

#include <QCheckBox>
#include <QComboBox>
#include <QDialogButtonBox>
#include <QDirIterator>
#include <QFileDialog>
#include <QFileDialog>
#include <QFileInfo>
#include <QGridLayout>
#include <QGroupBox>
#include <QHBoxLayout>
#include <QHeaderView>
#include <QIcon>
#include <QLabel>
#include <QLineEdit>
#include <QListView>
#include <QMenu>
#include <QPushButton>
#include <QStatusBar>
#include <QTextStream>
#include <QTimer>
#include <QVBoxLayout>

#include "gui-preferences-pd.h"
#include "octave-qobject.h"
#include "octave-qtutils.h"
#include "set-path-dialog.h"
#include "set-path-model.h"

#include "ovl.h"

OCTAVE_BEGIN_NAMESPACE(octave)

set_path_dialog::set_path_dialog (QWidget *parent, base_qobject& oct_qobj)
: QDialog (parent), m_octave_qobj (oct_qobj)
{
  setWindowTitle (tr ("Set Path"));

  set_path_model *model = new set_path_model (this);

  m_info_label = new QLabel (tr ("All changes take effect immediately."));

  m_add_folder_button = new QPushButton (tr ("Add Folder..."));

  QMenu *add_dir_menu = new QMenu ();
  m_add_folder_button->setMenu (add_dir_menu);
  add_dir_menu->addAction (tr ("Single Folder"),
                           this, &set_path_dialog::add_dir);
  add_dir_menu->addAction (tr ("Folder With Subfolders"),
                           this, &set_path_dialog::add_dir_subdirs);

  m_move_to_top_button = new QPushButton (tr ("Move to Top"));
  m_move_to_bottom_button = new QPushButton (tr ("Move to Bottom"));
  m_move_up_button = new QPushButton (tr ("Move Up"));
  m_move_down_button = new QPushButton (tr ("Move Down"));
  m_remove_button = new QPushButton (tr ("Remove"));

  m_reload_button = new QPushButton (tr ("Reload"));
  m_save_button = new QPushButton (tr ("Save"));

  m_revert_button = new QPushButton (tr ("Revert"));

  QMenu *revert_menu = new QMenu ();
  m_revert_button->setMenu (revert_menu);
  revert_menu->addAction (tr ("Revert Last Change"),
                          model, &set_path_model::revert_last);
  revert_menu->addAction (tr ("Revert All Changes"),
                          model, &set_path_model::revert);

  m_save_button->setFocus ();

  connect (m_remove_button, &QPushButton::clicked,
           this, &set_path_dialog::rm_dir);

  connect (m_move_to_top_button, &QPushButton::clicked,
           this, &set_path_dialog::move_dir_top);

  connect (m_move_to_bottom_button, &QPushButton::clicked,
           this, &set_path_dialog::move_dir_bottom);

  connect (m_move_up_button, &QPushButton::clicked,
           this, &set_path_dialog::move_dir_up);

  connect (m_move_down_button, &QPushButton::clicked,
           this, &set_path_dialog::move_dir_down);

  connect (m_reload_button, &QPushButton::clicked,
           model, &set_path_model::path_to_model);

  connect (m_save_button, &QPushButton::clicked,
           model, &set_path_model::save);

  // Any interpreter_event signal from a set_path_model object is
  // handled the same as for the parent set_path_dialog object.

  connect (model, QOverload<const fcn_callback&>::of (&set_path_model::interpreter_event),
           this, QOverload<const fcn_callback&>::of (&set_path_dialog::interpreter_event));

  connect (model, QOverload<const meth_callback&>::of (&set_path_model::interpreter_event),
           this, QOverload<const meth_callback&>::of (&set_path_dialog::interpreter_event));

  m_path_list = new QListView (this);
  m_path_list->setWordWrap (false);
  m_path_list->setModel (model);
  m_path_list->setSelectionBehavior (QAbstractItemView::SelectRows);
  m_path_list->setSelectionMode (QAbstractItemView::ExtendedSelection);
  m_path_list->setAlternatingRowColors (true);
  m_path_list->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);

  // layout everything
  QDialogButtonBox *button_box = new QDialogButtonBox (Qt::Horizontal);
  button_box->addButton (m_save_button, QDialogButtonBox::ActionRole);
  button_box->addButton (m_reload_button, QDialogButtonBox::ActionRole);

  // add dialog close button
  m_close_button = button_box->addButton (QDialogButtonBox::Close);
  connect (button_box, &QDialogButtonBox::rejected,
           this, &set_path_dialog::close);

  button_box->addButton (m_revert_button, QDialogButtonBox::ActionRole);

  // path edit options
  QDialogButtonBox *path_edit_layout = new QDialogButtonBox (Qt::Vertical);
  path_edit_layout->addButton (m_add_folder_button, QDialogButtonBox::ActionRole);
  path_edit_layout->addButton (m_move_to_top_button, QDialogButtonBox::ActionRole);
  path_edit_layout->addButton (m_move_up_button, QDialogButtonBox::ActionRole);
  path_edit_layout->addButton (m_move_down_button, QDialogButtonBox::ActionRole);
  path_edit_layout->addButton (m_move_to_bottom_button, QDialogButtonBox::ActionRole);
  path_edit_layout->addButton (m_remove_button, QDialogButtonBox::ActionRole);

  // main layout
  QHBoxLayout *main_hboxlayout = new QHBoxLayout;
  main_hboxlayout->addWidget(path_edit_layout);
  main_hboxlayout->addWidget(m_path_list);

  QGridLayout *main_layout = new QGridLayout;
  main_layout->addWidget (m_info_label, 0, 0);
  main_layout->addLayout (main_hboxlayout, 1, 0);
  main_layout->addWidget (button_box, 2, 0);

  setLayout (main_layout);

  resource_manager& rmgr = m_octave_qobj.get_resource_manager ();
  gui_settings *settings = rmgr.get_settings ();
  restoreGeometry (
                   settings->value(pd_geometry.key).toByteArray());
}

void set_path_dialog::update_model (void)
{
  set_path_model *m = static_cast<set_path_model *> (m_path_list->model ());
  m->path_to_model ();
}

void set_path_dialog::add_dir_common (bool subdirs)
{
  QString dir
    = QFileDialog::getExistingDirectory (this, tr ("Open Directory"),
                                         "",
                                         (QFileDialog::ShowDirsOnly
                                          | QFileDialog::DontResolveSymlinks));

  if (! dir.isEmpty ())
    {
      if (subdirs)
        {
          // Use existing method mofifying load path and updating dialog
          // instead of adding string and updating load path
          emit modify_path_signal (QStringList (dir), false, true);
        }
      else
        {
          set_path_model *m
            = static_cast<set_path_model *> (m_path_list->model ());
          m->add_dir (dir);
        }
    }
}

void set_path_dialog::add_dir(void)
{
  add_dir_common (false);
}

void set_path_dialog::add_dir_subdirs (void)
{
  add_dir_common (true);
}

void set_path_dialog::rm_dir (void)
{
  set_path_model *m = static_cast<set_path_model *> (m_path_list->model ());
  QItemSelectionModel *selmodel = m_path_list->selectionModel ();
  QModelIndexList indexlist = selmodel->selectedIndexes();
  m->rm_dir (indexlist);

  selmodel->clearSelection ();
}

void set_path_dialog::move_dir_up (void)
{
  set_path_model *m = static_cast<set_path_model *> (m_path_list->model ());
  QItemSelectionModel *selmodel = m_path_list->selectionModel ();
  QModelIndexList indexlist = selmodel->selectedIndexes();
  m->move_dir_up (indexlist);

  // Update selection and view
  selmodel->clearSelection ();
  int min_row = m->rowCount () - 1;
  for (int i = 0; i < indexlist.length (); i++)
    {
      int new_row = std::max (indexlist.at (i).row () - 1, 0);
      min_row = std::min (min_row, new_row);
      selmodel->select (m->index (new_row), QItemSelectionModel::Select);
    }

  m_path_list->scrollTo (m->index (min_row));
}

void set_path_dialog::move_dir_down (void)
{
  set_path_model *m = static_cast<set_path_model *> (m_path_list->model ());
  QItemSelectionModel *selmodel = m_path_list->selectionModel ();
  QModelIndexList indexlist = selmodel->selectedIndexes();
  m->move_dir_down (indexlist);

  // Update selection and view
  selmodel->clearSelection ();
  int max_row = 0;
  for (int i = 0; i < indexlist.length (); i++)
    {
      int new_row = std::min (indexlist.at (i).row () + 1, m->rowCount () - 1);
      max_row = std::max (max_row, new_row);
      selmodel->select (m->index (new_row), QItemSelectionModel::Select);
    }

  m_path_list->scrollTo (m->index (max_row));
}

void set_path_dialog::move_dir_top (void)
{
  set_path_model *m = static_cast<set_path_model *> (m_path_list->model ());
  QItemSelectionModel *selmodel = m_path_list->selectionModel ();
  QModelIndexList indexlist = selmodel->selectedIndexes();
  m->move_dir_top (indexlist);

  // Update selection and view
  selmodel->clearSelection ();
  for (int i = 0; i < indexlist.length (); i++)
    selmodel->select (m->index (i), QItemSelectionModel::Select);

  m_path_list->scrollTo (m->index (0));
}

void set_path_dialog::move_dir_bottom (void)
{
  set_path_model *m = static_cast<set_path_model *> (m_path_list->model ());
  QItemSelectionModel *selmodel = m_path_list->selectionModel ();
  QModelIndexList indexlist = selmodel->selectedIndexes();
  m->move_dir_bottom (indexlist);

  // Update selection and view
  selmodel->clearSelection ();
  int row_count = m->rowCount ();
  for (int i = 0; i < indexlist.length (); i++)
    selmodel->select (m->index (row_count - 1 - i),
                      QItemSelectionModel::Select);

  m_path_list->scrollTo (m->index (row_count - 1));
}

void set_path_dialog::save_settings ()
{
  resource_manager& rmgr = m_octave_qobj.get_resource_manager ();
  gui_settings *settings = rmgr.get_settings ();
  settings->setValue (pd_geometry.key, saveGeometry ());
}

void set_path_dialog::closeEvent (QCloseEvent *e)
{
  save_settings ();

  QWidget::closeEvent (e);
}

OCTAVE_END_NAMESPACE(octave)
