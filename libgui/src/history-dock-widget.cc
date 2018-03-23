/*

Copyright (C) 2011-2017 Jacob Dawid

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

#include <QApplication>
#include <QClipboard>
#include <QVBoxLayout>
#include <QMenu>
#include <QScrollBar>
#include <QCompleter>
#include <QLabel>

#include "error.h"
#include "resource-manager.h"

#include "cmd-hist.h"

#include "history-dock-widget.h"

namespace octave
{
  history_dock_widget::history_dock_widget (QWidget *p)
    : octave_dock_widget (p)
  {
    setObjectName ("HistoryDockWidget");
    setStatusTip (tr ("Browse and search the command history."));

    connect (this, SIGNAL (command_create_script (const QString&)),
             p, SLOT (new_file (const QString&)));

    connect (this, SIGNAL (information (const QString&)),
             p, SLOT (report_status_message (const QString&)));

    connect (this, SIGNAL (command_double_clicked (const QString&)),
             p, SLOT (execute_command_in_terminal (const QString&)));

    construct ();
  }

  void history_dock_widget::set_history (const QStringList& hist)
  {
    m_history_model->setStringList (hist);
    m_history_list_view->scrollToBottom ();
  }

  void history_dock_widget::append_history (const QString& hist_entry)
  {
    QStringList lst = m_history_model->stringList ();
    lst.append (hist_entry);

    QScrollBar *scroll_bar = m_history_list_view->verticalScrollBar ();

    bool at_bottom = scroll_bar->maximum () - scroll_bar->value () < 1;

    m_history_model->setStringList (lst);

    // Scroll if slider position at bottom.
    if (at_bottom)
      m_history_list_view->scrollToBottom ();
  }

  void history_dock_widget::clear_history (void)
  {
    m_history_model->setStringList (QStringList ());
  }

  void history_dock_widget::save_settings (void)
  {
    QSettings *settings = resource_manager::get_settings ();

    if (! settings)
      return;

    settings->setValue ("history_dock_widget/filter_active",
                        m_filter_checkbox->isChecked ());
    settings->setValue ("history_dock_widget/filter_shown", m_filter_shown);

    QStringList mru;
    for (int i = 0; i < m_filter->count (); i++)
      mru.append (m_filter->itemText (i));
    settings->setValue ("history_dock_widget/mru_list", mru);

    settings->sync ();

    octave_dock_widget::save_settings ();
  }

  void history_dock_widget::update_filter_history (void)
  {
    QString text = m_filter->currentText ();   // get current text
    int index = m_filter->findText (text);     // and its actual index

    if (index > -1)
      m_filter->removeItem (index);    // remove if already existing

    m_filter->insertItem (0, text);    // (re)insert at beginning
    m_filter->setCurrentIndex (0);
  }

  void history_dock_widget::filter_activate (bool state)
  {
    m_filter->setEnabled (state);
    m_sort_filter_proxy_model.setDynamicSortFilter (state);

    if (state)
      m_sort_filter_proxy_model.setFilterWildcard (m_filter->currentText ());
    else
      m_sort_filter_proxy_model.setFilterWildcard (QString ());
  }

  void history_dock_widget::ctxMenu (const QPoint& xpos)
  {
    QMenu menu (this);

    QModelIndex index = m_history_list_view->indexAt (xpos);

    if (index.isValid () && index.column () == 0)
      {
        menu.addAction (resource_manager::icon ("edit-copy"),
                        tr ("Copy"), this, SLOT (handle_contextmenu_copy (bool)));
        menu.addAction (tr ("Evaluate"), this,
                        SLOT (handle_contextmenu_evaluate (bool)));
        menu.addAction (resource_manager::icon ("document-new"),
                        tr ("Create script"), this,
                        SLOT (handle_contextmenu_create_script (bool)));
      }
    if (m_filter_shown)
      menu.addAction (tr ("Hide filter"), this,
                      SLOT (handle_contextmenu_filter ()));
    else
      menu.addAction (tr ("Show filter"), this,
                      SLOT (handle_contextmenu_filter ()));

    menu.exec (m_history_list_view->mapToGlobal (xpos));
  }

  void history_dock_widget::handle_double_click (QModelIndex modelIndex)
  {
    emit command_double_clicked (modelIndex.data ().toString ());
  }

  void history_dock_widget::handle_contextmenu_copy (bool)
  {
    QString text;
    QItemSelectionModel *selectionModel = m_history_list_view->selectionModel ();
    QModelIndexList rows = selectionModel->selectedRows ();
    QModelIndexList::iterator it;
    bool prev_valid_row = false;
    for (it = rows.begin (); it != rows.end (); it++)
      {
        if ((*it).isValid ())
          {
            if (prev_valid_row)
              text += '\n';
            text += (*it).data ().toString ();
            prev_valid_row = true;
          }
      }
    QApplication::clipboard ()->setText (text);
  }

  void history_dock_widget::handle_contextmenu_evaluate (bool)
  {
    QItemSelectionModel *selectionModel = m_history_list_view->selectionModel ();
    QModelIndexList rows = selectionModel->selectedRows ();
    QModelIndexList::iterator it;
    for (it = rows.begin () ; it != rows.end (); it++)
      {
        if ((*it).isValid ())
          emit command_double_clicked ((*it).data ().toString ());
      }
  }

  void history_dock_widget::handle_contextmenu_create_script (bool)
  {
    QString text;
    QItemSelectionModel *selectionModel = m_history_list_view->selectionModel ();
    QModelIndexList rows = selectionModel->selectedRows ();

    bool prev_valid_row = false;
    for (QModelIndexList::iterator it = rows.begin (); it != rows.end (); it++)
      {
        if ((*it).isValid ())
          {
            if (prev_valid_row)
              text += '\n';
            text += (*it).data ().toString ();
            prev_valid_row = true;
          }
      }

    if (text.length () > 0)
      emit command_create_script (text);
  }

  void history_dock_widget::handle_contextmenu_filter (void)
  {
    m_filter_shown = ! m_filter_shown;
    m_filter_widget->setVisible (m_filter_shown);
  }

  void history_dock_widget::copyClipboard (void)
  {
    if (m_history_list_view->hasFocus ())
      handle_contextmenu_copy (true);
    if (m_filter->lineEdit ()->hasFocus ()
        && m_filter->lineEdit ()->hasSelectedText ())
      {
        QClipboard *clipboard = QApplication::clipboard ();
        clipboard->setText (m_filter->lineEdit ()->selectedText ());
      }
  }

  void history_dock_widget::pasteClipboard (void)
  {
    if (m_filter->lineEdit ()->hasFocus ())
      {
        QClipboard *clipboard = QApplication::clipboard ();
        QString str = clipboard->text ();
        if (str.length () > 0)
          m_filter->lineEdit ()->insert (str);
      }
  }

  void history_dock_widget::selectAll (void)
  {
    if (m_filter->lineEdit ()->hasFocus ())
      m_filter->lineEdit ()->selectAll ();

    if (m_history_list_view->hasFocus ())
      m_history_list_view->selectAll ();
  }

  void history_dock_widget::handle_visibility (bool visible)
  {
    octave_dock_widget::handle_visibility (visible);

    if (visible)
      {
        int filter_state = m_filter_checkbox->isChecked ();
        filter_activate (filter_state);
      }
  }

  void history_dock_widget::construct (void)
  {
    m_history_model = new QStringListModel ();
    m_sort_filter_proxy_model.setSourceModel (m_history_model);
    m_history_list_view = new QListView (this);
    m_history_list_view->setModel (&m_sort_filter_proxy_model);
    m_history_list_view->setAlternatingRowColors (true);
    m_history_list_view->setEditTriggers (QAbstractItemView::NoEditTriggers);
    m_history_list_view->setStatusTip (
                                       tr ("Double-click a command to transfer it to the terminal."));
    m_history_list_view->setSelectionMode (QAbstractItemView::ExtendedSelection);
    m_history_list_view->setContextMenuPolicy (Qt::CustomContextMenu);
    connect (m_history_list_view,
             SIGNAL (customContextMenuRequested (const QPoint &)), this,
             SLOT (ctxMenu (const QPoint &)));

    m_filter = new QComboBox (this);
    m_filter->setToolTip (tr ("Enter text to filter the command history"));
    m_filter->setEditable (true);
    m_filter->setMaxCount (MaxFilterHistory);
    m_filter->setInsertPolicy (QComboBox::NoInsert);
    m_filter->setSizeAdjustPolicy (
                                   QComboBox::AdjustToMinimumContentsLengthWithIcon);
    QSizePolicy sizePol (QSizePolicy::Expanding, QSizePolicy::Preferred);
    m_filter->setSizePolicy (sizePol);
    m_filter->completer ()->setCaseSensitivity (Qt::CaseSensitive);

    QLabel *filter_label = new QLabel (tr ("Filter"));

    m_filter_checkbox = new QCheckBox ();

    setWindowIcon (QIcon (":/actions/icons/logo.png"));
    set_title (tr ("Command History"));
    setWidget (new QWidget ());

    m_filter_widget = new QWidget (this);
    QHBoxLayout *filter_layout = new QHBoxLayout ();
    filter_layout->addWidget (filter_label);
    filter_layout->addWidget (m_filter_checkbox);
    filter_layout->addWidget (m_filter);
    filter_layout->setMargin(0);
    m_filter_widget->setLayout (filter_layout);

    QVBoxLayout *hist_layout = new QVBoxLayout ();
    hist_layout->addWidget (m_filter_widget);
    hist_layout->addWidget (m_history_list_view);

    hist_layout->setMargin (2);
    widget ()->setLayout (hist_layout);

    // Init state of the filter
    QSettings *settings = resource_manager::get_settings ();

    m_filter_shown
      = settings->value ("history_dock_widget/filter_shown",true).toBool ();
    m_filter_widget->setVisible (m_filter_shown);

    m_filter->addItems (settings->value ("history_dock_widget/mru_list").toStringList ());

    bool filter_state
      = settings->value ("history_dock_widget/filter_active", false).toBool ();
    m_filter_checkbox->setChecked (filter_state);
    filter_activate (filter_state);

    // Connect signals and slots
    connect (m_filter, SIGNAL (editTextChanged (const QString&)),
             &m_sort_filter_proxy_model,
             SLOT (setFilterWildcard (const QString&)));
    connect (m_filter_checkbox, SIGNAL (toggled (bool)),
             this, SLOT (filter_activate (bool)));
    connect (m_filter->lineEdit (), SIGNAL (editingFinished (void)),
             this, SLOT (updatem_filter_history (void)));

    connect (m_history_list_view, SIGNAL (doubleClicked (QModelIndex)),
             this, SLOT (handle_double_click (QModelIndex)));

    m_history_list_view->setTextElideMode (Qt::ElideRight);
  }
}
