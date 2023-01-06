////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2011-2023 The Octave Project Developers
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

#include <QApplication>
#include <QClipboard>
#include <QCompleter>
#include <QHBoxLayout>
#include <QHeaderView>
#include <QInputDialog>
#include <QLabel>
#include <QLineEdit>
#include <QMenu>
#include <QMessageBox>
#include <QPushButton>
#include <QSignalMapper>
#include <QVBoxLayout>

#include "gui-preferences-ws.h"
#include "octave-qobject.h"
#include "octave-qtutils.h"
#include "workspace-view.h"

OCTAVE_BEGIN_NAMESPACE(octave)

workspace_view::workspace_view (QWidget *p, base_qobject& oct_qobj)
: octave_dock_widget ("WorkspaceView", p, oct_qobj),
  m_view (new QTableView (this)),
  m_filter_checkbox (new QCheckBox ()),
  m_filter (new QComboBox (this)),
  m_filter_widget (new QWidget (this))
{
  set_title (tr ("Workspace"));
  setStatusTip (tr ("View the variables in the active workspace."));

  m_filter->setToolTip (tr ("Enter text to filter the workspace"));
  m_filter->setEditable (true);
  m_filter->setMaxCount (ws_max_filter_history.def.toInt ());
  m_filter->setInsertPolicy (QComboBox::NoInsert);
  m_filter->setSizeAdjustPolicy (QComboBox::AdjustToMinimumContentsLengthWithIcon);
  QSizePolicy sizePol (QSizePolicy::Expanding, QSizePolicy::Preferred);
  m_filter->setSizePolicy (sizePol);
  m_filter->completer ()->setCaseSensitivity (Qt::CaseSensitive);

  QLabel *filter_label = new QLabel (tr ("Filter"));

  m_view->setWordWrap (false);
  m_view->setContextMenuPolicy (Qt::CustomContextMenu);
  m_view->setShowGrid (false);
  (m_view->verticalHeader) ()->hide ();
  m_view->setAlternatingRowColors (true);
  m_view_previous_row_count = 0;

  // Set an empty widget, so we can assign a layout to it.
  setWidget (new QWidget (this));

  // Create the layouts
  QHBoxLayout *filter_layout = new QHBoxLayout ();

  filter_layout->addWidget (filter_label);
  filter_layout->addWidget (m_filter_checkbox);
  filter_layout->addWidget (m_filter);
  filter_layout->setMargin (0);

  m_filter_widget->setLayout (filter_layout);

  QVBoxLayout *ws_layout = new QVBoxLayout ();
  ws_layout->addWidget (m_filter_widget);
  ws_layout->addWidget (m_view);
  ws_layout->setSpacing (0);

  resource_manager& rmgr = m_octave_qobj.get_resource_manager ();
  gui_settings *settings = rmgr.get_settings ();

  if (settings)
    {
      m_filter_shown = settings->value (ws_filter_shown).toBool ();
      m_filter_widget->setVisible (m_filter_shown);

      ws_layout->setMargin (2);

      // Set the empty widget to have our layout.
      widget ()->setLayout (ws_layout);

      // Initialize collapse/expand state of the workspace subcategories.

      //enable sorting (setting column and order after model was set)
      m_view->setSortingEnabled (true);
      // Initialize column order and width of the workspace
      m_view->horizontalHeader ()->restoreState
        (settings->value (ws_column_state.key).toByteArray ());

      // Set header properties for sorting
      m_view->horizontalHeader ()->setSectionsClickable (true);
      m_view->horizontalHeader ()->setSectionsMovable (true);
      m_view->horizontalHeader ()->setSortIndicator (
                                                     settings->value (ws_sort_column).toInt (),
                                                     static_cast<Qt::SortOrder> (settings->value (ws_sort_order).toUInt ()));
      // FIXME: use value<Qt::SortOrder> instead of static cast after
      //        dropping support of Qt 5.4

      m_view->horizontalHeader ()->setSortIndicatorShown (true);

      m_view->horizontalHeader ()->setContextMenuPolicy (Qt::CustomContextMenu);
      connect (m_view->horizontalHeader (),
               &QTableView::customContextMenuRequested,
               this, &workspace_view::header_contextmenu_requested);

      // Init state of the filter
      m_filter->addItems (settings->value (ws_mru_list.key).toStringList ());

      bool filter_state =
        settings->value (ws_filter_active).toBool ();
      m_filter_checkbox->setChecked (filter_state);
      filter_activate (filter_state);
    }

  // Connect signals and slots.

  connect (m_filter, &QComboBox::editTextChanged,
           this, &workspace_view::filter_update);
  connect (m_filter_checkbox, &QCheckBox::toggled,
           this, &workspace_view::filter_activate);
  connect (m_filter->lineEdit (), &QLineEdit::editingFinished,
           this, &workspace_view::update_filter_history);

  connect (m_view, &QTableView::customContextMenuRequested,
           this, &workspace_view::contextmenu_requested);

  connect (m_view, &QTableView::activated,
           this, &workspace_view::handle_contextmenu_edit);

  if (! p)
    make_window ();
}

void workspace_view::setModel (workspace_model *model)
{
  m_filter_model.setSourceModel (model);
  m_filter_model.setFilterKeyColumn(0);

  m_view->setModel (&m_filter_model);

  // set the sorting after the model is set, it would be ignored otherwise
  resource_manager& rmgr = m_octave_qobj.get_resource_manager ();
  gui_settings *settings = rmgr.get_settings ();
  m_view->sortByColumn (
                        settings->value (ws_sort_column).toInt (),
                        static_cast<Qt::SortOrder> (settings->value (ws_sort_order).toUInt ()));
  // FIXME: use value<Qt::SortOrder> instead of static cast after
  //        dropping support of Qt 5.4

  m_model = model;
}

void
workspace_view::notice_settings (const gui_settings *settings)
{
  m_model->notice_settings (settings); // update colors of model first

  for (int i = 0; i < ws_columns_shown.length (); i++)
    m_view->setColumnHidden (i + 1, ! settings->value (ws_columns_shown_keys.at (i), true).toBool ());

  QString tool_tip;

  if (settings->value (ws_enable_colors).toBool ()
      && ! settings->value (ws_hide_tool_tips).toBool ())
    {
      tool_tip  = QString (tr ("View the variables in the active workspace.<br>"));
      tool_tip += QString (tr ("Colors for variable attributes:"));

      for (int i = 0; i < ws_colors_count; i++)
        {
          tool_tip +=
            QString (R"(<div style="background-color:%1;color:%2">%3</div>)")
            .arg (m_model->storage_class_color (i).name ())
            .arg (m_model->storage_class_color (i + ws_colors_count).name ())
            .arg (QCoreApplication::translate ("octave::settings_dialog",
                                               ws_color_names.at (i).toStdString ().data ()));
        }
    }

  setToolTip (tool_tip);
}

void
workspace_view::save_settings (void)
{
  resource_manager& rmgr = m_octave_qobj.get_resource_manager ();
  gui_settings *settings = rmgr.get_settings ();

  if (! settings)
    return;

  settings->setValue (ws_column_state.key,
                      m_view->horizontalHeader ()->saveState ());

  int sort_column = m_view->horizontalHeader ()->sortIndicatorSection ();
  Qt::SortOrder sort_order = m_view->horizontalHeader ()->sortIndicatorOrder ();
  settings->setValue (ws_sort_column.key, sort_column);
  settings->setValue (ws_sort_order.key, sort_order);

  settings->setValue (ws_filter_active.key, m_filter_checkbox->isChecked ());
  settings->setValue (ws_filter_shown.key, m_filter_shown);

  QStringList mru;
  for (int i = 0; i < m_filter->count (); i++)
    mru.append (m_filter->itemText (i));
  settings->setValue (ws_mru_list.key, mru);

  settings->sync ();

  octave_dock_widget::save_settings ();
}

void workspace_view::set_filter_focus (bool focus)
{
  if (focus)
    {
      m_filter->setFocus ();
      setFocusProxy (m_filter);
    }
  else
    {
      m_view->setFocus ();
      setFocusProxy (m_view);
    }
}

void
workspace_view::filter_update (const QString& expression)
{
  m_filter_model.setFilterWildcard (expression);
  handle_model_changed ();
}

void
workspace_view::filter_activate (bool state)
{
  m_filter->setEnabled (state);
  m_filter_model.setDynamicSortFilter (state);

  if (state)
    filter_update (m_filter->currentText ());
  else
    filter_update (QString ());

  set_filter_focus (state);
}

void
workspace_view::update_filter_history (void)
{
  QString text = m_filter->currentText ();   // get current text
  int index = m_filter->findText (text);     // and its actual index

  if (index > -1)
    m_filter->removeItem (index);    // remove if already existing

  m_filter->insertItem (0, text);    // (re)insert at beginning
  m_filter->setCurrentIndex (0);
}

void
workspace_view::header_contextmenu_requested (const QPoint& mpos)
{
  QMenu menu (this);
  QSignalMapper sig_mapper (this);

  resource_manager& rmgr = m_octave_qobj.get_resource_manager ();
  gui_settings *settings = rmgr.get_settings ();

  for (int i = 0; i < ws_columns_shown.length (); i++)
    {
      QAction *action
        = menu.addAction (tr (ws_columns_shown.at (i).toStdString ().data ()),
                          &sig_mapper, SLOT (map ()));
      sig_mapper.setMapping (action, i);
      action->setCheckable (true);
      action->setChecked (settings->value (ws_columns_shown_keys.at (i), true).toBool ());
    }

  // FIXME: We could use
  //
  //   connect (&m_sig_mapper, QOverload<int>::of (&QSignalMapper::mapped),
  //            this, &workspace_view::toggle_header);
  //
  // but referring to QSignalMapper::mapped will generate deprecated
  // function warnings from GCC.  We could also use
  //
  //   connect (&m_sig_mapper, &QSignalMapper::mappedInt,
  //            this, &workspace_view::toggle_header);
  //
  // but the function mappedInt was not introduced until Qt 5.15 so
  // we'll need a feature test.

  connect (&sig_mapper, SIGNAL (mapped (int)),
           this, SLOT (toggle_header (int)));

  menu.exec (m_view->mapToGlobal (mpos));
}

void
workspace_view::toggle_header (int col)
{
  resource_manager& rmgr = m_octave_qobj.get_resource_manager ();
  gui_settings *settings = rmgr.get_settings ();

  QString key = ws_columns_shown_keys.at (col);
  bool shown = settings->value (key, true).toBool ();

  m_view->setColumnHidden (col + 1, shown);

  settings->setValue (key, ! shown);
  settings->sync ();

  octave_dock_widget::save_settings ();
}

void
workspace_view::contextmenu_requested (const QPoint& qpos)
{
  QMenu menu (this);

  QModelIndex index = m_view->indexAt (qpos);

  // if it isn't Local, Global etc, allow the ctx menu
  if (index.isValid () && index.column () == 0)
    {
      QString var_name = get_var_name (index);

      menu.addAction (tr ("Open in Variable Editor"), this,
                      &workspace_view::handle_contextmenu_edit);

      menu.addAction (tr ("Copy name"), this,
                      &workspace_view::handle_contextmenu_copy);

      menu.addAction (tr ("Copy value"), this,
                      &workspace_view::handle_contextmenu_copy_value);

      QAction *rename
        = menu.addAction (tr ("Rename"), this,
                          &workspace_view::handle_contextmenu_rename);

      // Use m_model here instead of using "m_view->model ()" because
      // that points to the proxy model.
      if (! m_model->is_top_level ())
        {
          rename->setDisabled (true);
          rename->setToolTip (tr ("Only top-level symbols may be renamed"));
        }

      menu.addAction ("Clear " + var_name, this,
                      &workspace_view::handle_contextmenu_clear);

      menu.addSeparator ();

      menu.addAction ("disp (" + var_name + ')', this,
                      &workspace_view::handle_contextmenu_disp);

      menu.addAction ("plot (" + var_name + ')', this,
                      &workspace_view::handle_contextmenu_plot);

      menu.addAction ("stem (" + var_name + ')', this,
                      &workspace_view::handle_contextmenu_stem);

      menu.addSeparator ();

    }

  if (m_filter_shown)
    menu.addAction (tr ("Hide filter"), this,
                    &workspace_view::handle_contextmenu_filter);
  else
    menu.addAction (tr ("Show filter"), this,
                    &workspace_view::handle_contextmenu_filter);

  menu.exec (m_view->mapToGlobal (qpos));
}

void
workspace_view::handle_contextmenu_copy (void)
{
  QModelIndex index = m_view->currentIndex ();

  if (index.isValid ())
    {
      QString var_name = get_var_name (index);

      QClipboard *clipboard = QApplication::clipboard ();

      clipboard->setText (var_name);
    }
}

void
workspace_view::handle_contextmenu_copy_value (void)
{
  QModelIndex index = m_view->currentIndex ();

  if (index.isValid ())
    emit copy_variable_value_to_clipboard (get_var_name (index));
}

void
workspace_view::handle_contextmenu_rename (void)
{
  QModelIndex index = m_view->currentIndex ();

  if (index.isValid ())
    {
      QString var_name = get_var_name (index);

      QInputDialog *inputDialog = new QInputDialog ();

      inputDialog->setOptions (QInputDialog::NoButtons);

      bool ok = false;

      QString new_name
        = inputDialog->getText (nullptr, "Rename Variable", "New name:",
                                QLineEdit::Normal, var_name, &ok);

      if (ok && ! new_name.isEmpty ())
        emit rename_variable_signal (var_name, new_name);
    }
}

void
workspace_view::handle_contextmenu_edit (void)
{
  QModelIndex index = m_view->currentIndex ();

  if (index.isValid ())
    emit edit_variable_signal (get_var_name (index));
}

void
workspace_view::handle_contextmenu_clear (void)
{
  relay_contextmenu_command ("clear", true);
}

void
workspace_view::handle_contextmenu_disp (void)
{
  relay_contextmenu_command ("disp");
}

void
workspace_view::handle_contextmenu_plot (void)
{
  relay_contextmenu_command ("figure (); plot");
}

void
workspace_view::handle_contextmenu_stem (void)
{
  relay_contextmenu_command ("figure (); stem");
}

void
workspace_view::handle_contextmenu_filter (void)
{
  m_filter_shown = ! m_filter_shown;
  m_filter_widget->setVisible (m_filter_shown);

  set_filter_focus (m_filter_shown && m_filter_checkbox->isChecked ());
}

void
workspace_view::handle_model_changed (void)
{
  // m_view->resizeRowsToContents ();
  // Just modify those rows that have been added rather than go through
  // the whole list.  For-loop test will handle when number of rows reduced.
  QFontMetrics fm = m_view->fontMetrics ();
  int row_height = fm.height ();
  int new_row_count = m_filter_model.rowCount ();
  for (int i = m_view_previous_row_count; i < new_row_count; i++)
    m_view->setRowHeight (i, row_height);
  m_view_previous_row_count = new_row_count;
}

void
workspace_view::copyClipboard ()
{
  if (m_view->hasFocus ())
    handle_contextmenu_copy ();
}

void
workspace_view::selectAll ()
{
  if (m_view->hasFocus ())
    m_view->selectAll ();
}

void
workspace_view::relay_contextmenu_command (const QString& cmdname, bool str)
{
  QModelIndex index = m_view->currentIndex ();

  if (index.isValid ())
    {
      QString var_name;

      if (str)
        var_name = "\'" + get_var_name (index) + "\'";
      else
        var_name = get_var_name (index);

      emit command_requested (cmdname + " (" + var_name + ");");
    }
}

QString
workspace_view::get_var_name (const QModelIndex& index)
{
  // We are using a sort model proxy so m_model won't provide the
  // correct ordering.

  QAbstractItemModel *m = m_view->model ();

  QMap<int, QVariant> item_data
    = m->itemData (index.sibling (index.row (), 0));

  return item_data[0].toString ();
}

OCTAVE_END_NAMESPACE(octave)
