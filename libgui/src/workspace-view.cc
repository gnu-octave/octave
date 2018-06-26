/*

Copyright (C) 2013-2018 John W. Eaton
Copyright (C) 2011-2018 Jacob Dawid

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

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <QInputDialog>
#include <QApplication>
#include <QClipboard>
#include <QMessageBox>
#include <QLineEdit>
#include <QHeaderView>
#include <QHBoxLayout>
#include <QVBoxLayout>
#include <QPushButton>
#include <QMenu>
#include <QLabel>
#include <QCompleter>
#include <QSignalMapper>

#include "workspace-view.h"
#include "resource-manager.h"

#include "interpreter-private.h"
#include "symscope.h"

namespace octave
{
  workspace_view::workspace_view (QWidget *p)
    : octave_dock_widget ("WorkspaceView", p), m_view (new QTableView (this))
  {
    setWindowIcon (QIcon (":/actions/icons/logo.png"));
    set_title (tr ("Workspace"));
    setStatusTip (tr ("View the variables in the active workspace."));

    m_filter = new QComboBox (this);
    m_filter->setToolTip (tr ("Enter text to filter the workspace"));
    m_filter->setEditable (true);
    m_filter->setMaxCount (MaxFilterHistory);
    m_filter->setInsertPolicy (QComboBox::NoInsert);
    m_filter->setSizeAdjustPolicy (QComboBox::AdjustToMinimumContentsLengthWithIcon);
    QSizePolicy sizePol (QSizePolicy::Expanding, QSizePolicy::Preferred);
    m_filter->setSizePolicy (sizePol);
    m_filter->completer ()->setCaseSensitivity (Qt::CaseSensitive);

    QLabel *filter_label = new QLabel (tr ("Filter"));

    m_filter_checkbox = new QCheckBox ();

    m_view->setWordWrap (false);
    m_view->setContextMenuPolicy (Qt::CustomContextMenu);
    m_view->setShowGrid (false);
    (m_view->verticalHeader) ()->hide ();
    m_view->setAlternatingRowColors (true);
    m_view_previous_row_count = 0;

    // Set an empty widget, so we can assign a layout to it.
    setWidget (new QWidget (this));

    // Create the layouts
    m_filter_widget = new QWidget (this);
    QHBoxLayout *filter_layout = new QHBoxLayout ();

    filter_layout->addWidget (filter_label);
    filter_layout->addWidget (m_filter_checkbox);
    filter_layout->addWidget (m_filter);
    filter_layout->setMargin(0);
    m_filter_widget->setLayout (filter_layout);

    QVBoxLayout *ws_layout = new QVBoxLayout ();
    ws_layout->addWidget (m_filter_widget);
    ws_layout->addWidget (m_view);

    QSettings *settings = resource_manager::get_settings ();

    if (settings)
      {
        m_filter_shown = settings->value ("workspaceview/filter_shown",true).toBool ();
        m_filter_widget->setVisible (m_filter_shown);

        ws_layout->setMargin (2);

        // Set the empty widget to have our layout.
        widget ()->setLayout (ws_layout);

        // Initialize collapse/expand state of the workspace subcategories.

        //enable sorting (setting column and order after model was set)
        m_view->setSortingEnabled (true);
        // Initialize column order and width of the workspace
        m_view->horizontalHeader ()->restoreState (
          settings->value ("workspaceview/column_state").toByteArray ());

        // Set header properties for sorting
#if defined (HAVE_QHEADERVIEW_SETSECTIONSCLICKABLE)
        m_view->horizontalHeader ()->setSectionsClickable (true);
#else
        m_view->horizontalHeader ()->setClickable (true);
#endif
#if defined (HAVE_QHEADERVIEW_SETSECTIONSMOVABLE)
        m_view->horizontalHeader ()->setSectionsMovable (true);
#else
        m_view->horizontalHeader ()->setMovable (true);
#endif
        m_view->horizontalHeader ()->setSortIndicator (settings->value ("workspaceview/sort_by_column",0).toInt (),
                                                       static_cast<Qt::SortOrder>
                                                       (settings->value ("workspaceview/sort_order", Qt::AscendingOrder).toUInt ()));
        m_view->horizontalHeader ()->setSortIndicatorShown (true);

        m_view->horizontalHeader ()->setContextMenuPolicy (Qt::CustomContextMenu);
        connect (m_view->horizontalHeader (),
                 SIGNAL (customContextMenuRequested (const QPoint &)),
                 this, SLOT (header_contextmenu_requested (const QPoint &)));

        // Init state of the filter
        m_filter->addItems (settings->value ("workspaceview/mru_list").toStringList ());

        bool filter_state =
          settings->value ("workspaceview/filter_active", false).toBool ();
        m_filter_checkbox->setChecked (filter_state);
        filter_activate (filter_state);
      }

    // Connect signals and slots.

    connect (m_filter, SIGNAL (editTextChanged (const QString&)),
             this, SLOT (filter_update (const QString&)));
    connect (m_filter_checkbox, SIGNAL (toggled (bool)),
             this, SLOT (filter_activate (bool)));
    connect (m_filter->lineEdit (), SIGNAL (editingFinished ()),
             this, SLOT (update_filter_history ()));

    connect (m_view, SIGNAL (customContextMenuRequested (const QPoint&)),
             this, SLOT (contextmenu_requested (const QPoint&)));

    connect (m_view, SIGNAL (activated (QModelIndex)),
             this, SLOT (handle_contextmenu_edit (void)));

    connect (this, SIGNAL (command_requested (const QString&)),
             p, SLOT (execute_command_in_terminal (const QString&)));

    connect (this,
             SIGNAL (edit_variable_signal (const QString&, const octave_value&)),
             p, SLOT (edit_variable (const QString&, const octave_value&)));
  }

  void workspace_view::setModel (workspace_model *model)
  {
    m_filter_model.setSourceModel (model);
    m_filter_model.setFilterKeyColumn(0);

    m_view->setModel (&m_filter_model);

    // set the sorting after a model was set, it would be ignored otherwise
    QSettings *settings = resource_manager::get_settings ();
    m_view->sortByColumn (settings->value ("workspaceview/sort_by_column",0).toInt (),
                          static_cast<Qt::SortOrder> (settings->value ("workspaceview/sort_order", Qt::AscendingOrder).toUInt ()));

    m_model = model;
  }

  void
  workspace_view::notice_settings (const QSettings *settings)
  {
    int i;

    m_model->notice_settings (settings); // update colors of model first

    for (i = 0; i < m_columns_shown_keys.size (); i++)
      m_view->setColumnHidden (i + 1, ! settings->value (m_columns_shown_keys.at (i),true).toBool ());

    QString tool_tip;

    if (! settings->value ("workspaceview/hide_tool_tips",false).toBool ())
      {
        tool_tip  = QString (tr ("View the variables in the active workspace.<br>"));
        tool_tip += QString (tr ("Colors for variable attributes:"));
        for (i = 0; i < resource_manager::storage_class_chars ().length (); i++)
          {
            tool_tip +=
              QString (R"(<div style="background-color:%1;color:#000000">%2</div>)")
              .arg (m_model->storage_class_color (i).name ())
              .arg (resource_manager::storage_class_names ().at (i));
          }
      }

    setToolTip (tool_tip);

    m_columns_shown = QStringList ();
    m_columns_shown.append (tr ("Class"));
    m_columns_shown.append (tr ("Dimension"));
    m_columns_shown.append (tr ("Value"));
    m_columns_shown.append (tr ("Attribute"));

    m_columns_shown_keys = QStringList ();
    m_columns_shown_keys.append ("workspaceview/show_class");
    m_columns_shown_keys.append ("workspaceview/show_dimension");
    m_columns_shown_keys.append ("workspaceview/show_value");
    m_columns_shown_keys.append ("workspaceview/show_attribute");

    m_sig_mapper = nullptr;
  }

  void
  workspace_view::save_settings (void)
  {
    QSettings *settings = resource_manager::get_settings ();

    if (! settings)
      return;

    settings->setValue ("workspaceview/column_state",
                        m_view->horizontalHeader ()->saveState ());

    int sort_column = m_view->horizontalHeader ()->sortIndicatorSection ();
    Qt::SortOrder sort_order = m_view->horizontalHeader ()->sortIndicatorOrder ();
    settings->setValue ("workspaceview/sort_by_column", sort_column);
    settings->setValue ("workspaceview/sort_order", sort_order);

    settings->setValue ("workspaceview/filter_active",
                        m_filter_checkbox->isChecked ());
    settings->setValue ("workspaceview/filter_shown", m_filter_shown);

    QStringList mru;
    for (int i = 0; i < m_filter->count (); i++)
      mru.append (m_filter->itemText (i));
    settings->setValue ("workspaceview/mru_list", mru);

    settings->sync ();

    octave_dock_widget::save_settings ();

    if (m_sig_mapper)
      delete m_sig_mapper;
  }

  void
  workspace_view::closeEvent (QCloseEvent *e)
  {
    emit active_changed (false);
    QDockWidget::closeEvent (e);
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

    if (m_sig_mapper)
      delete m_sig_mapper;
    m_sig_mapper = new QSignalMapper (this);

    QSettings *settings = resource_manager::get_settings ();

    for (int i = 0; i < m_columns_shown.size (); i++)
      {
        QAction *action = menu.addAction (m_columns_shown.at (i),
                                          m_sig_mapper, SLOT (map ()));
        m_sig_mapper->setMapping (action, i);
        action->setCheckable (true);
        action->setChecked (settings->value (m_columns_shown_keys.at (i),true).toBool ());
      }

    connect (m_sig_mapper, SIGNAL (mapped (int)), this, SLOT (toggle_header (int)));

    menu.exec (m_view->mapToGlobal (mpos));
  }

  void
  workspace_view::toggle_header (int col)
  {
    QSettings *settings = resource_manager::get_settings ();

    QString key = m_columns_shown_keys.at (col);
    bool shown = settings->value (key,true).toBool ();

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

    // if it isnt Local, Glocal etc, allow the ctx menu
    if (index.isValid () && index.column () == 0)
      {
        QString var_name = get_var_name (index);

        menu.addAction (tr ("Open in Variable Editor"), this,
                        SLOT (handle_contextmenu_edit ()));

        menu.addAction (tr ("Copy name"), this,
                        SLOT (handle_contextmenu_copy ()));

        menu.addAction (tr ("Copy value"), this,
                        SLOT (handle_contextmenu_copy_value ()));

        QAction *rename = menu.addAction (tr ("Rename"), this,
                                          SLOT (handle_contextmenu_rename ()));

        QAbstractItemModel *m = m_view->model ();
        const workspace_model *wm = static_cast<const workspace_model *> (m);

        if (! wm->is_top_level ())
          {
            rename->setDisabled (true);
            rename->setToolTip (tr ("Only top-level symbols may be renamed"));
          }

        menu.addAction ("Clear " + var_name, this,
                        SLOT (handle_contextmenu_clear ()));

        menu.addSeparator ();

        menu.addAction ("disp (" + var_name + ')', this,
                        SLOT (handle_contextmenu_disp ()));

        menu.addAction ("plot (" + var_name + ')', this,
                        SLOT (handle_contextmenu_plot ()));

        menu.addAction ("stem (" + var_name + ')', this,
                        SLOT (handle_contextmenu_stem ()));

        menu.addSeparator ();

      }

    if (m_filter_shown)
      menu.addAction (tr ("Hide filter"), this,
                      SLOT (handle_contextmenu_filter ()));
    else
      menu.addAction (tr ("Show filter"), this,
                      SLOT (handle_contextmenu_filter ()));

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
      {
        QString var_name = get_var_name (index);

        symbol_scope scope
          = __get_current_scope__ ("workspace_view::handle_contextmenu_copy_value");

        octave_value val = scope ? scope.varval (var_name.toStdString ()) : 0;
        std::ostringstream buf;
        val.print_raw (buf, true);

        QClipboard *clipboard = QApplication::clipboard ();
        clipboard->setText (QString::fromStdString (buf.str ()));
      }
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
          {
            QAbstractItemModel *m = m_view->model ();
            m->setData (index, new_name, Qt::EditRole);
          }
      }
  }

  void
  workspace_view::handle_contextmenu_edit (void)
  {
    QModelIndex index = m_view->currentIndex ();

    if (index.isValid ())
      {
        QString var_name = get_var_name (index);

        symbol_scope scope = m_model->scope ();

        octave_value val;
        if (scope)
          val = scope.varval (var_name.toStdString ());

        emit edit_variable_signal (var_name, val);
      }
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
}
