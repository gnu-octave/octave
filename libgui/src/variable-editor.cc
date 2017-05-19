/*

Copyright (C) 2015 Michael Barnes
Copyright (C) 2013 Rüdiger Sonderfeld
Copyright (C) 2013 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <limits>

#include <QSignalMapper>
#include <QApplication>
#include <QFileDialog>
#include <QMainWindow>
#include <QVBoxLayout>
#include <QToolButton>
#include <QClipboard>
#include <QTableView>
#include <QTabWidget>
#include <QToolBar>
#include <QLabel>
#include <QDebug> //DBG
#include <QMenu>
#include <QHeaderView>
#include <QPalette>

#include "resource-manager.h"
#include "variable-editor.h"
#include "variable-editor-model.h"
#include "octave-qt-link.h"

#include "ov.h"
#include "operators/ops.h"

variable_editor::variable_editor (QWidget *p)
  : octave_dock_widget (p),
    main (new QMainWindow ()),
    tool_bar (new QToolBar (main)),
    tab_widget (new QTabWidget (main)),
    default_width (20), default_height (100),
    add_font_height (0),
    autofit (false), autofit_max (false),
    use_terminal_font (true), alternate_rows (true),
    stylesheet (""), font (), sel_font (),
    table_colors ()
{

  /*
    We use a MainWindow
   */

  setObjectName ("variable_editor");
  setWindowTitle (tr ("Variable Editor"));
  set_title (tr ("Variable Editor"));
  setStatusTip (tr ("Edit variables."));
  setWindowIcon (QIcon (":/actions/icons/logo.png"));

  // Tool Bar
  construct_tool_bar ();
  main->addToolBar (tool_bar);

  for (int i = 0; i<resource_manager::varedit_color_chars().length(); i++)
    table_colors.append(QColor(Qt::white));

  // Tab Widget
  tab_widget->setTabsClosable (true);
  tab_widget->setMovable (true);
  connect (tab_widget, SIGNAL (tabCloseRequested (int)),
           this, SLOT (closeTab (int)));
  main->setCentralWidget (tab_widget);

  // Main
  main->setParent (this);
  setWidget (main);

  connect (this, SIGNAL (command_requested (const QString&)),
           p, SLOT (execute_command_in_terminal (const QString&)));
}

void
variable_editor::construct_tool_bar ()
{
  tool_bar->setObjectName ("VariableEditorToolBar");
  tool_bar->setWindowTitle (tr ("Variable Editor Toolbar"));

  tool_bar->addAction
    (resource_manager::icon("document-save"),
     tr ("Save"),
     this, SLOT (save ()));
  tool_bar->addSeparator ();

  tool_bar->addAction
    (resource_manager::icon("edit-cut"),
     tr ("Cut"),
     this, SLOT (cutClipboard ()));
  tool_bar->addAction
    (resource_manager::icon("edit-copy"),
     tr ("Copy"),
     this, SLOT (copyClipboard ()));
  tool_bar->addAction
    (resource_manager::icon("edit-paste"),
     tr ("Paste"),
     this, SLOT (pasteClipboard ()));
  tool_bar->addAction
    (resource_manager::icon("edit-paste"),// TODO - different icon?
     tr ("Paste Table"),
     this, SLOT (pasteTableClipboard ()));
  tool_bar->addSeparator ();

  //QAction *print_action; /icons/fileprint.png
  //tool_bar->addSeparator ();

  QToolButton *plot_tool_button = new QToolButton (tool_bar);
  plot_tool_button->setText (tr ("Plot"));
  plot_tool_button->setIcon (resource_manager::icon("logo"));//QIcon (":/actions/icons/gear.png")); // TODO

  plot_tool_button->setPopupMode (QToolButton::InstantPopup);

  QMenu *plot_menu = new QMenu (tr ("Plot"), plot_tool_button);
  plot_menu->setSeparatorsCollapsible (false);
  QSignalMapper *plot_mapper = new QSignalMapper (plot_menu);
  plot_mapper->setMapping(plot_menu->addAction ("plot",
                                                plot_mapper,
                                                SLOT (map ())),
                          "figure (); plot (%1);");
  plot_mapper->setMapping(plot_menu->addAction ("bar",
                                                plot_mapper,
                                                SLOT (map ())),
                          "figure (); bar (%1);");
  plot_mapper->setMapping(plot_menu->addAction ("stem",
                                                plot_mapper,
                                                SLOT (map ())),
                          "figure (); stem (%1);");
  plot_mapper->setMapping(plot_menu->addAction ("stairs",
                                                plot_mapper,
                                                SLOT (map ())),
                          "figure (); stairs (%1);");
  plot_mapper->setMapping(plot_menu->addAction ("area",
                                                plot_mapper,
                                                SLOT (map ())),
                          "figure (); area (%1);");
  plot_mapper->setMapping(plot_menu->addAction ("pie",
                                                plot_mapper,
                                                SLOT (map ())),
                          "figure (); pie (%1);");
  plot_mapper->setMapping(plot_menu->addAction ("hist",
                                                plot_mapper,
                                                SLOT (map ())),
                          "figure (); hist (%1);");
  connect (plot_mapper, SIGNAL (mapped (const QString &)),
           this, SLOT (relay_command (const QString &)));

  plot_tool_button->setMenu (plot_menu);
  tool_bar->addWidget (plot_tool_button);



  tool_bar->addSeparator ();
  tool_bar->addAction
    (QIcon (":/actions/icons/arrow_up.png"),
     tr ("Up"),
     this, SLOT (up ()));
}

/*variable_editor::~variable_editor ()
{
}*/

namespace
{
  /// Helper struct to store widget pointers in "data" Tab property.
  struct table_data
  {
    table_data (QTableView *t = nullptr)
      : table (t)
    { }
    QTableView *table;
  };

  table_data get_table_data (QTabWidget *w, int tidx)
  {
    return w->widget (tidx)->property("data").value<table_data> ();
  }

  table_data get_table_data (QTabWidget *w)
  {
    return get_table_data (w, w->currentIndex ());
  }
}
Q_DECLARE_METATYPE (table_data)

void
variable_editor::edit_variable (const QString &name)
{

  if (stylesheet == "")
  {
    QSettings *settings = resource_manager::get_settings ();
    notice_settings(settings);
  }
  const int tab_count = tab_widget->count ();
  for (int i = 0; i < tab_count; ++i)
    if (tab_widget->tabText (i) == name)
      {
        tab_widget->setCurrentIndex (i);
        return; // already open
      }

  QWidget *page = new QWidget; // Do not set parent.

  QVBoxLayout *vbox = new QVBoxLayout (page);
  page->setLayout (vbox);

  QLabel *label = new QLabel (page);
  label->setTextFormat (Qt::PlainText);
  label->setText (name);
  vbox->addWidget (label);

  QTableView *table = new QTableView (page);
  variable_editor_model *model =
    new variable_editor_model (name, label, table);

  table->setModel (model);
  table->setWordWrap (false);
  table->setContextMenuPolicy (Qt::CustomContextMenu);
  table->setSelectionMode (QAbstractItemView::ContiguousSelection);


  table->horizontalHeader()->setContextMenuPolicy (Qt::CustomContextMenu);
  table->verticalHeader()->setContextMenuPolicy (Qt::CustomContextMenu);

  connect (table->horizontalHeader(), SIGNAL(customContextMenuRequested(const QPoint&)),
             this, SLOT (columnmenu_requested (const QPoint &)));
  connect (table->verticalHeader(), SIGNAL(customContextMenuRequested(const QPoint&)),
             this, SLOT (rowmenu_requested (const QPoint &)));
  connect (table, SIGNAL (customContextMenuRequested (const QPoint&)),
           this, SLOT (contextmenu_requested (const QPoint&)));
  connect (table, SIGNAL (doubleClicked (const QModelIndex&)),
           this, SLOT (double_click (const QModelIndex&)));
  connect (model, SIGNAL (dataChanged(const QModelIndex&,const QModelIndex&)),
           this, SLOT (callUpdate(const QModelIndex&,const QModelIndex&)));

  vbox->addWidget (table);

  page->setProperty ("data", QVariant::fromValue (table_data (table)));
  int tab_idx = tab_widget->addTab (page, name);
  tab_widget->setCurrentIndex (tab_idx);
  if (autofit)
    {
      table->resizeColumnsToContents();
      if (autofit_max)
        {
          int mx = 0;
          for (int i = 0;i<table->model()->columnCount();i++)
            {
              if (table->columnWidth(i) > mx)
                mx = table->columnWidth(i);
            }
          table->horizontalHeader()->setDefaultSectionSize(mx);
        }
    }
  else
    {
      table->horizontalHeader()->setDefaultSectionSize(default_width);
    }
  table->setFont(font);
  table->setStyleSheet(stylesheet);
  table->setAlternatingRowColors(alternate_rows);
#if defined (HAVE_QT4)
  table->verticalHeader()->setResizeMode(QHeaderView::Interactive);
#else
  table->verticalHeader()->setSectionResizeMode(QHeaderView::Interactive);
#endif
  table->verticalHeader()->setDefaultSectionSize(default_height+add_font_height);
}

void
variable_editor::callUpdate(const QModelIndex&, const QModelIndex&)
{
  if (autofit)
    {
      QTableView *view = get_table_data(tab_widget).table;
      view->resizeColumnsToContents();
      if (autofit_max)
        {
          int mx = 0;
          for (int i = 0;i<view->model()->columnCount();i++)
            {
              if (view->columnWidth(i) > mx)
                mx = view->columnWidth(i);
            }
          view->horizontalHeader()->setDefaultSectionSize(mx);
        }

    }

  emit updated();
}

void
variable_editor::closeTab (int idx)
{
  if (idx < 0 || idx > tab_widget->count ())
    return;

  QWidget *const wdgt = tab_widget->widget (idx);
  tab_widget->removeTab (idx);
  delete wdgt;
}

void
variable_editor::closeEvent (QCloseEvent *e)
{
  emit finished ();
  octave_dock_widget::closeEvent (e);
}

void
variable_editor::contextmenu_requested (const QPoint &qpos)
{
  QTableView *view = get_table_data (tab_widget).table;
  QModelIndex index = view->indexAt (qpos);

  if (index.isValid ())
    {
      QMenu *menu = new QMenu (this);
      menu->addAction
        (resource_manager::icon("edit-cut"),
         tr ("Cut"),
         this, SLOT (cutClipboard ()));
      menu->addAction
        (resource_manager::icon("edit-copy"),
         tr ("Copy"),
         this, SLOT (copyClipboard ()));
      menu->addAction
        (resource_manager::icon("edit-paste"),
         tr ("Paste"),
         this, SLOT (pasteClipboard ()));
      menu->addAction
        (resource_manager::icon("edit-paste"), // TODO
         tr ("Paste Table"),
         this, SLOT (pasteTableClipboard ()));
      menu->addSeparator ();

      menu->addAction
        (resource_manager::icon("edit-delete"),
         tr ("Clear"),
         this, SLOT (clearContent ()));

      menu->addAction
        (resource_manager::icon("document-new"),
         tr ("Variable from Selection"),
         this, SLOT (createVariable ()));

      // TODO sort

      menu->addAction
        ( //QIcon (), TODO
         tr ("Transpose"),
         this, SLOT (transposeContent ()));

      QItemSelectionModel *sel = view->selectionModel ();
      QList<QModelIndex> indices = sel->selectedIndexes ();
      if (! indices.isEmpty ())
        {
          menu->addSeparator ();
          QSignalMapper *plot_mapper = new QSignalMapper (menu);
          plot_mapper->setMapping(menu->addAction ("plot",
                                                   plot_mapper,
                                                   SLOT (map ())),
                                  "figure (); plot (%1);");
          plot_mapper->setMapping(menu->addAction ("bar",
                                                   plot_mapper,
                                                   SLOT (map ())),
                                  "figure (); bar (%1);");
          plot_mapper->setMapping(menu->addAction ("stem",
                                                   plot_mapper,
                                                   SLOT (map ())),
                                  "figure (); stem (%1);");
          plot_mapper->setMapping(menu->addAction ("stairs",
                                                   plot_mapper,
                                                   SLOT (map ())),
                                  "figure (); stairs (%1);");
          plot_mapper->setMapping(menu->addAction ("area",
                                                   plot_mapper,
                                                   SLOT (map ())),
                                  "figure (); area (%1);");
          plot_mapper->setMapping(menu->addAction ("pie",
                                                   plot_mapper,
                                                   SLOT (map ())),
                                  "figure (); pie (%1);");
          plot_mapper->setMapping(menu->addAction ("hist",
                                                   plot_mapper,
                                                   SLOT (map ())),
                                  "figure (); hist (%1);");
          connect (plot_mapper, SIGNAL (mapped (const QString &)),
                   this, SLOT (relay_command (const QString &)));
        }

      menu->exec (view->mapToGlobal (qpos));
    }
}

QList<int>
variable_editor::octave_to_coords(QString& selection)
{
  //TODO: Is this necessary or would it be quicker to clone the function that gives us the QString?
  // sanity check
  if (selection.count(",") != 1)
  {
    return QList<int>();
  }

  QList<int> output;
  output.clear();
  // remove braces
  int firstbracket = std::max(selection.indexOf("("), selection.indexOf("{"));
  selection = selection.mid(firstbracket+1,selection.length()-(firstbracket+2));

  QString rows = selection.left(selection.indexOf(","));
  if (!(rows.contains(":")))
  {
    // Only one row
    output.push_back(rows.toInt());
    output.push_back(output.last());
  }
  else
  {
    output.push_back(rows.left(rows.indexOf(":")).toInt());
    output.push_back(rows.right(rows.length() - (rows.indexOf(":") +1 )).toInt());
  }

  QString cols = selection.right(selection.length() - (selection.indexOf(",") +1 ));
  if (cols.left(1) == " ")
    cols = cols.right(cols.length()-1);

  if (!(cols.contains(":")))
    {
      // Only one row
      output.push_back(cols.toInt());
      output.push_back(output.last());
    }
    else
    {
      output.push_back(cols.left(cols.indexOf(":")).toInt());
      output.push_back(cols.right(cols.length() - (cols.indexOf(":")+1)).toInt());
    }
  return output;
}

void
variable_editor::delete_selected()
{
  QTableView *view = get_table_data (tab_widget).table;
  QString selection = selected_to_octave();
  QList<int> coords = octave_to_coords(selection);

  if (coords.isEmpty())
      return;

  bool whole_columns_selected = (coords[0] == 1 ) && (coords[1] == view->model()->rowCount());
  bool whole_rows_selected = (coords[2] == 1 ) && (coords[3] == view->model()->columnCount());

  emit command_requested(QString("disp('") + QString::number(coords[0]) + ","+ QString::number(coords[1]) + ","+ QString::number(coords[2]) + ","+ QString::number(coords[3]) + "');");


  // Must be deleting whole columns or whole rows, and not the whole thing.
  if (whole_columns_selected == whole_rows_selected) // all or nothing
    return;


  if (whole_rows_selected)
  {
    view->model()->removeRows(coords[0],coords[1] - coords[0]);
  }

  if (whole_columns_selected)
  {
    view->model()->removeColumns(coords[2], coords[3] - coords[2]);
  }

  emit updated();
}

void
variable_editor::columnmenu_requested(const QPoint &pt)
{
  QTableView *view = get_table_data (tab_widget).table;


  int index = view->horizontalHeader()->logicalIndexAt(pt);

  //emit command_requested(QString("disp('") + QString::number(index) + "');");

  if (index < 0 || index > view->model()->columnCount())
    return;

  QString selection = selected_to_octave();
  QList<int> coords = octave_to_coords(selection);

  bool nothingSelected = false;
  if (coords.isEmpty())
    nothingSelected = true;

  bool whole_columns_selected = nothingSelected ? false : (coords[0] == 1 ) && (coords[1] == view->model()->rowCount());

  bool current_column_selected = nothingSelected ? false : (coords[2] <= index+1) && (coords[3] > index);

  int column_selection_count = nothingSelected ? 0 : (coords[3] - coords[2]) +1;

  if (!whole_columns_selected || !current_column_selected)
  {
    view->selectColumn(index);
    column_selection_count = 1;
    current_column_selected = true;
    whole_columns_selected = true;
  }

  QString column_string = tr(column_selection_count > 1 ? " columns" : " column");

  QMenu *menu = new QMenu(this);
  menu->addAction
    (resource_manager::icon("edit-cut"),
     tr ("Cut") + column_string,
     this, SLOT (cutClipboard ()));
  menu->addAction
    (resource_manager::icon("edit-copy"),
     tr ("Copy") + column_string,
     this, SLOT (copyClipboard ()));
  menu->addAction
    (resource_manager::icon("edit-paste"),
     tr ("Paste"),
     this, SLOT (pasteClipboard ()));
  menu->addAction
    (resource_manager::icon("edit-paste"), // TODO
     tr ("Paste Table"),
     this, SLOT (pasteTableClipboard ()));
  menu->addSeparator ();

  menu->addAction
    (resource_manager::icon("edit-delete"),
     tr ("Clear") + column_string,
     this, SLOT (clearContent ()));

  menu->addAction
    (resource_manager::icon("edit-delete"),
     tr ("Delete") + column_string,
     this, SLOT (delete_selected ()));

  menu->addAction
    (resource_manager::icon("document-new"),
     tr ("Variable from Selection"),
     this, SLOT (createVariable ()));

  menu->addSeparator ();

  QSignalMapper *plot_mapper = new QSignalMapper (menu);
  plot_mapper->setMapping(menu->addAction ("plot",
                                           plot_mapper,
                                           SLOT (map ())),
                          "figure (); plot (%1);");
  plot_mapper->setMapping(menu->addAction ("bar",
                                           plot_mapper,
                                           SLOT (map ())),
                          "figure (); bar (%1);");
  plot_mapper->setMapping(menu->addAction ("stem",
                                           plot_mapper,
                                           SLOT (map ())),
                          "figure (); stem (%1);");
  plot_mapper->setMapping(menu->addAction ("stairs",
                                           plot_mapper,
                                           SLOT (map ())),
                          "figure (); stairs (%1);");
  plot_mapper->setMapping(menu->addAction ("area",
                                           plot_mapper,
                                           SLOT (map ())),
                          "figure (); area (%1);");
  plot_mapper->setMapping(menu->addAction ("pie",
                                           plot_mapper,
                                           SLOT (map ())),
                          "figure (); pie (%1);");
  plot_mapper->setMapping(menu->addAction ("hist",
                                           plot_mapper,
                                           SLOT (map ())),
                          "figure (); hist (%1);");
  connect (plot_mapper, SIGNAL (mapped (const QString &)),
           this, SLOT (relay_command (const QString &)));

  QPoint menupos = pt;
  menupos.setY(view->horizontalHeader()->height());

  menu->exec (view->mapToGlobal (menupos));
}

void
variable_editor::rowmenu_requested(const QPoint &pt)
{
  QTableView *view = get_table_data (tab_widget).table;

  int index = view->verticalHeader()->logicalIndexAt(pt);

  //emit command_requested(QString("disp('") + QString::number(index) + "');");

  if (index < 0 || index > view->model()->columnCount())
    return;

  QString selection = selected_to_octave();
  QList<int> coords = octave_to_coords(selection);

  bool nothingSelected = false;
  if (coords.isEmpty())
    nothingSelected = true;

  bool whole_rows_selected = nothingSelected ? false : (coords[2] == 1 ) && (coords[3] == view->model()->columnCount());

  bool current_row_selected = nothingSelected ? false : (coords[0] <= index+1) && (coords[1] > index);

  int rowselection_count = nothingSelected ? 0 : (coords[3] - coords[2]) +1;

  if (!whole_rows_selected || !current_row_selected)
  {
    view->selectRow(index);
    rowselection_count = 1;
    current_row_selected = true;
    whole_rows_selected = true;
  }

  QString row_string = tr(rowselection_count > 1 ? " rows" : " row");

  QMenu *menu = new QMenu(this);
  menu->addAction
    (resource_manager::icon("edit-cut"),
     tr ("Cut") + row_string,
     this, SLOT (cutClipboard ()));
  menu->addAction
    (resource_manager::icon("edit-copy"),
     tr ("Copy") + row_string,
     this, SLOT (copyClipboard ()));
  menu->addAction
    (resource_manager::icon("edit-paste"),
     tr ("Paste"),
     this, SLOT (pasteClipboard ()));
  menu->addAction
    (resource_manager::icon("edit-paste"), // TODO
     tr ("Paste Table"),
     this, SLOT (pasteTableClipboard ()));
  menu->addSeparator ();

  menu->addAction
    (resource_manager::icon("edit-delete"),
     tr ("Clear") + row_string,
     this, SLOT (clearContent ()));

  menu->addAction
    (resource_manager::icon("edit-delete"),
     tr ("Delete") + row_string,
     this, SLOT (delete_selected ()));

  menu->addAction
    (resource_manager::icon("document-new"),
     tr ("Variable from Selection"),
     this, SLOT (createVariable ()));

  menu->addSeparator ();

  QSignalMapper *plot_mapper = new QSignalMapper (menu);
  plot_mapper->setMapping(menu->addAction ("plot",
                                           plot_mapper,
                                           SLOT (map ())),
                          "figure (); plot (%1);");
  plot_mapper->setMapping(menu->addAction ("bar",
                                           plot_mapper,
                                           SLOT (map ())),
                          "figure (); bar (%1);");
  plot_mapper->setMapping(menu->addAction ("stem",
                                           plot_mapper,
                                           SLOT (map ())),
                          "figure (); stem (%1);");
  plot_mapper->setMapping(menu->addAction ("stairs",
                                           plot_mapper,
                                           SLOT (map ())),
                          "figure (); stairs (%1);");
  plot_mapper->setMapping(menu->addAction ("area",
                                           plot_mapper,
                                           SLOT (map ())),
                          "figure (); area (%1);");
  plot_mapper->setMapping(menu->addAction ("pie",
                                           plot_mapper,
                                           SLOT (map ())),
                          "figure (); pie (%1);");
  plot_mapper->setMapping(menu->addAction ("hist",
                                           plot_mapper,
                                           SLOT (map ())),
                          "figure (); hist (%1);");
  connect (plot_mapper, SIGNAL (mapped (const QString &)),
           this, SLOT (relay_command (const QString &)));

  QPoint menupos = pt;
  menupos.setX(view->verticalHeader()->width());
  //setY(view->verticalHeader()->sectionPosition(index+1) +
  //             view->verticalHeader()->sectionSize(index));

  menu->exec (view->mapToGlobal (menupos));
}

static
QString idx_to_expr (int32_t from, int32_t to)
{
  if (from == to)
    return QString ("%1").arg (from + 1);
  else
    return QString ("%1:%2").arg (from + 1).arg (to + 1);
}

QString
variable_editor::selected_to_octave ()
{
  QString name = tab_widget->tabText (tab_widget->currentIndex ());
  QTableView *view = get_table_data (tab_widget).table;
  QItemSelectionModel *sel = view->selectionModel ();

  if (!(sel->hasSelection()))
      return name; // Nothing selected

  QList<QModelIndex> indices = sel->selectedIndexes (); // it's indices!

  int32_t from_row = std::numeric_limits<int32_t>::max ();
  int32_t to_row = 0;
  int32_t from_col = std::numeric_limits<int32_t>::max ();
  int32_t to_col = 0;

  for (const auto& idx : indices)
    {
      from_row = std::min (from_row, idx.row ());
      to_row = std::max (to_row, idx.row ());
      from_col = std::min (from_col, idx.column ());
      to_col = std::max (to_col, idx.column ());
    }

  QString rows = idx_to_expr (from_row, to_row);
  QString cols = idx_to_expr (from_col, to_col);

  // TODO Cells
  return QString ("%1 (%2, %3)").arg (name).arg (rows).arg (cols);
}

void
variable_editor::double_click (const QModelIndex &idx)
{
  QString name = tab_widget->tabText (tab_widget->currentIndex ());
  QTableView *const table = get_table_data (tab_widget).table;
  variable_editor_model *const model =
    qobject_cast<variable_editor_model*>(table->model ());
  if (model->requires_sub_editor (idx))
    {
      if (model ->editor_type_matrix(idx))
          edit_variable(name +
                        model->parens ()
                        .arg (idx.row () + 1)
                        .arg (idx.column () + 1));
/*        emit command_requested ("openvar ('" + name +
                                model->parens ()
                                .arg (idx.row () + 1)
                                .arg (idx.column () + 1)
                                + "');");
*/

    }
}

void
variable_editor::relay_command (const QString &cmd)
{
  emit command_requested (cmd.arg (selected_to_octave ()));
}

void
variable_editor::save ()
{
  QString name = tab_widget->tabText (tab_widget->currentIndex ());
  QString file =
    QFileDialog::getSaveFileName (this,
                                  tr ("Save Variable %1 As").arg (name),
                                  ".", 0, 0,
                                  QFileDialog::DontUseNativeDialog);
  // TODO type? binary, float-binary, ascii, text, hdf5 matlab format?
  if (! file.isEmpty ())
    // TODO use octave_value::save_*?
    emit command_requested (QString ("save ('%1', '%2');")
                            .arg (file)
                            .arg (name));
}

void
variable_editor::clearContent ()
{
  // TODO shift?
  QTableView *view = get_table_data (tab_widget).table;
  QAbstractItemModel *model = view->model ();
  QItemSelectionModel *sel = view->selectionModel ();
  QList<QModelIndex> indices = sel->selectedIndexes ();
  for (const auto& idx : indices)
    model->setData(idx, QVariant ("0")); // TODO [] for cell
}

void
variable_editor::cutClipboard ()
{
  if (!has_focus())
    return;

  copyClipboard ();
  clearContent ();
}

void
variable_editor::copyClipboard ()
{
  if (!has_focus())
    return;

  QTableView *view = get_table_data (tab_widget).table;
  QAbstractItemModel *model = view->model ();
  QItemSelectionModel *sel = view->selectionModel ();
  QList<QModelIndex> indices = sel->selectedIndexes ();
  qSort(indices);
  if (indices.size () <= 0)
    return;

  // Convert selected items into TSV format and copy that.
  // Spreadsheet tools should understand that.
  QModelIndex previous = indices.first ();
  QString copy = model->data (previous).toString ();
  indices.removeFirst ();
  foreach (QModelIndex idx, indices)
    {
      copy.append (previous.row () != idx.row () ? '\n' : '\t');
      copy.append (model->data (idx).toString ());
      previous = idx;
    }
  copy.append ('\n');

  QClipboard *clipboard = QApplication::clipboard ();
  clipboard->setText (copy);
}

bool
variable_editor::has_focus()
{
    // ToDo: This only generates exceptions in certain circumstances. Get
    //       a definitive list and eliminate the need to handle exceptions

    if (tab_widget->currentIndex() == -1)
        return false; // No tabs

    try
      {
        QTableView *view = get_table_data(tab_widget).table;
        if (view)
            return view->hasFocus();

        return false;
      }
    catch (...)
      {
        return false;
      }
    return false;
}

void
variable_editor::pasteClipboard ()
{
  // TODO

  if (!has_focus())
    return;

  QClipboard *clipboard = QApplication::clipboard ();
  QString text = clipboard->text ();

  QTableView *view = get_table_data (tab_widget).table;
  QItemSelectionModel *sel = view->selectionModel ();
  QList<QModelIndex> indices = sel->selectedIndexes ();

  variable_editor_model *model = static_cast<variable_editor_model *>(view->model());

  if (indices.isEmpty())
    {
      if (view->size() == QSize(1,1))
        {
          model->setData(view->model()->index(0,0),text.toDouble());
        }
      else if (view->size() == QSize(0,0))
        {
          model->insertColumn(0);
          model->insertRow(0);
          model->setData(view->model()->index(0,0),text.toDouble());
        }
    }
  else
    {
      for (int i = 0;i<indices.size();i++)
        {
          view->model()->setData(indices[i],text.toDouble());
        }
    }

  emit updated();
}

void variable_editor::pasteTableClipboard ()
{
  if (!has_focus())
    return;

  QClipboard *clipboard = QApplication::clipboard ();
  QString text = clipboard->text ();


  QTableView *view = get_table_data (tab_widget).table;
  QItemSelectionModel *sel = view->selectionModel ();
  QList<QModelIndex> indices = sel->selectedIndexes ();

  variable_editor_model *model = static_cast<variable_editor_model *>(view->model());

  QPoint start;
  QPoint end;

  QPoint tabsize = QPoint(model->rowCount(), model->columnCount());

  if (indices.isEmpty())
    {
      start = QPoint(0,0);
      end = tabsize;
    }
  else if (indices.size() == 1)
    {
      start = QPoint(indices[0].row(),indices[0].column());
      end = tabsize;
    }
  else
    {
      end = QPoint(0,0);
      start = tabsize;

      for (int i = 0;i<indices.size();i++)
        {
          if (indices[i].column() < start.y())
              start.setY(indices[i].column());

          if (indices[i].column() > end.y())
              end.setY(indices[i].column());

          if (indices[i].row() < start.x())
              start.setX(indices[i].column());

          if (indices[i].row() > end.x())
              end.setX(indices[i].column());

        }
    }

  int rownum = 0,colnum = 0;

  QStringList rows = text.split ('\n');
  for (const auto& row : rows)
    {
      if (rownum > end.x() - start.x())
          continue;

      QStringList cols = row.split ('\t');
      if (cols.isEmpty())
          continue;

      for (const auto& col : cols)
        {
          if (col.isEmpty())
              continue;

          if (colnum > end.y() - start.y() )
            {
              continue;
            }
          model->setData(model->index(rownum + start.x(),colnum + start.y()),QVariant(col));

//          relay_command("disp('" + QString::number(colnum+start.y()) + "," + QString::number(rownum+start.x()) +"');");
          colnum++;
        }
      colnum = 0;
      rownum++;
    }

  emit updated();
}

void
variable_editor::createVariable ()
{
  // TODO unnamed1..n if exist ('unnamed', 'var')
  relay_command ("unnamed = %1");
}

void
variable_editor::transposeContent ()
{
  QString name = tab_widget->tabText (tab_widget->currentIndex ());
  emit command_requested (QString ("%1 = %1';").arg (name));
  emit updated();
}

void
variable_editor::up ()
{
  QString name = tab_widget->tabText (tab_widget->currentIndex ());
  // TODO is there a better way?
  if (name.endsWith (')') || name.endsWith ('}'))
    {
      qDebug () << "up";
      name.remove (QRegExp ("(\\(|\\{)[^({]*(\\)|\\})$"));
      edit_variable(name);
      //emit command_requested (QString ("openvar ('%1');").arg (name));
    }
}

void variable_editor::clear_data_cache ()
{
  for (int i = 0; i < tab_widget->count (); ++i)
    {
      QTableView *const table = get_table_data (tab_widget, i).table;
      QAbstractItemModel *const model = table->model ();
      qobject_cast<variable_editor_model*>(model)->clear_data_cache ();
    }
}

void variable_editor::notice_settings(const QSettings *settings)
{
  default_width = settings->value("variable_editor/column_width", QVariant("100")).toString().toInt();
  autofit = settings->value("variable_editor/autofit_column_width", QVariant(false)).toBool();
  if (autofit)
    {
      if (settings->value("variable_editor/autofit_type",0).toInt() == 1)
        {
          autofit_max = true;
        }
    }

  default_height = settings->value("variable_editor/row_height",QVariant("10")).toString().toInt();


  alternate_rows = settings->value("variable_editor/alternate_rows", QVariant(false)).toBool();
  QList<QColor> _default_colors =
    resource_manager::varedit_default_colors ();
  QString class_chars = resource_manager::varedit_color_chars ();


  use_terminal_font = settings->value("variable_editor/use_terminal_font",true).toBool();

  QString font_name;
  int font_size;

  if (use_terminal_font)
  {
    font_name = settings->value("terminal/fontName","").toString();
    font_size = settings->value("terminal/fontSize",10).toInt();
  }
  else
  {
    font_name = settings->value("variable_editor/font_name","").toString();
    font_size = settings->value("variable_editor/font_size",10).toInt();
  }
  font = QFont(font_name,font_size);

  if (settings->value("variable_editor/autofit_row_height",false).toBool())
    {
      QFontMetrics fm(font);
      add_font_height = fm.height();
    }
  else
    add_font_height = 0;

  for (int i = 0; i < class_chars.length (); i++)
    {
      QVariant default_var = _default_colors.at (i);
      QColor setting_color = settings->value ("variable_editor/color_"
                                              + class_chars.mid (i,1),
                                              default_var).value<QColor> ();
      table_colors.replace (i,setting_color);
    }
  update_colors();
  int toolsize = settings->value("variable_editor/toolbar_size",QVariant(0)).toInt();
  if (toolsize > 0)
      tool_bar->setIconSize(QSize(toolsize,toolsize));
}

/// Also updates the font
void variable_editor::update_colors()
{
  stylesheet="";
  stylesheet += "QTableView::item{ foreground-color: " + table_colors[0].name() +" }";
  stylesheet += "QTableView::item{ background-color: " + table_colors[1].name() +" }";
  stylesheet += "QTableView::item{ selection-color: " + table_colors[2].name() +" }";
  stylesheet += "QTableView::item:selected{ background-color: " + table_colors[3].name() +" }";
  if ((table_colors.length() > 4) && alternate_rows)
    {
      stylesheet += "QTableView::item:alternate{ background-color: " + table_colors[4].name() +" }";
      stylesheet += "QTableView::item:alternate:selected{ background-color: " + table_colors[3].name() +" }";
    }

  if (tab_widget->count() < 1)
    return;

  for (int i=0;i<tab_widget->count();i++)
    {
      QTableView *view = get_table_data(tab_widget).table;
      view->setAlternatingRowColors(alternate_rows);
      view->setStyleSheet(stylesheet);
      view->setFont(font);
    }

}

QStringList variable_editor::color_names()
{
    QStringList output;

    output << "Foreground";
    output << "Background";
    output << "Selected Foreground";
    output << "Selected Background";
    output << "Alternate Background";

    return output;

}

QList<QColor> variable_editor::default_colors()
{
    // fbsa
    QList<QColor> colorlist;

    colorlist << qApp->palette().color(QPalette::WindowText);
    colorlist << qApp->palette().color(QPalette::Base);
    colorlist << qApp->palette().color(QPalette::HighlightedText);
    colorlist << qApp->palette().color(QPalette::Highlight);
    colorlist << qApp->palette().color(QPalette::AlternateBase);

    return colorlist;
}

