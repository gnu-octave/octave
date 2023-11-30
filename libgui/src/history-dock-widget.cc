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
#include <QLabel>
#include <QMenu>
#include <QScrollBar>
#include <QVBoxLayout>

#include "gui-preferences-cs.h"
#include "gui-preferences-global.h"
#include "gui-preferences-hw.h"
#include "gui-settings.h"
#include "history-dock-widget.h"

#include "cmd-hist.h"

#include "error.h"

OCTAVE_BEGIN_NAMESPACE(octave)

history_dock_widget::history_dock_widget (QWidget *p)
  : octave_dock_widget ("HistoryDockWidget", p)
{
  setStatusTip (tr ("Browse and search the command history."));

  construct ();

  if (! p)
    make_window ();
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

void history_dock_widget::clear_history ()
{
  m_history_model->setStringList (QStringList ());
}

void history_dock_widget::save_settings ()
{
  gui_settings settings;

  settings.setValue (hw_filter_active.settings_key (), m_filter_checkbox->isChecked ());
  settings.setValue (hw_filter_shown.settings_key (), m_filter_shown);

  QStringList mru;
  for (int i = 0; i < m_filter->count (); i++)
    mru.append (m_filter->itemText (i));
  settings.setValue (hw_mru_list.settings_key (), mru);

  settings.sync ();

  octave_dock_widget::save_settings ();
}

void history_dock_widget::update_filter_history ()
{
  QString text = m_filter->currentText ();   // get current text
  int index = m_filter->findText (text);     // and its actual index

  if (index > -1)
    m_filter->removeItem (index);    // remove if already existing

  m_filter->insertItem (0, text);    // (re)insert at beginning
  m_filter->setCurrentIndex (0);
}

void history_dock_widget::set_filter_focus (bool focus)
{
  if (focus)
    {
      m_filter->setFocus ();
      setFocusProxy (m_filter);
    }
  else
    {
      m_history_list_view->setFocus ();
      setFocusProxy (m_history_list_view);
    }
}

void history_dock_widget::filter_activate (bool state)
{
  m_filter->setEnabled (state);
  m_sort_filter_proxy_model.setDynamicSortFilter (state);

  if (state)
    m_sort_filter_proxy_model.setFilterWildcard (m_filter->currentText ());
  else
    m_sort_filter_proxy_model.setFilterWildcard (QString ());

  set_filter_focus (state);
}

void history_dock_widget::ctxMenu (const QPoint& xpos)
{
  QMenu menu (this);

  QModelIndex index = m_history_list_view->indexAt (xpos);

  if (index.isValid () && index.column () == 0)
    {
      gui_settings settings;

      menu.addAction (settings.icon ("edit-copy"), tr ("Copy"), this,
                      &history_dock_widget::handle_contextmenu_copy);
      menu.addAction (tr ("Evaluate"), this,
                      &history_dock_widget::handle_contextmenu_evaluate);
      menu.addAction (settings.icon ("document-new"), tr ("Create script"), this,
                      &history_dock_widget::handle_contextmenu_create_script);
      menu.addSeparator ();
    }
  if (m_filter_shown)
    menu.addAction (tr ("Hide filter"), this,
                    &history_dock_widget::handle_contextmenu_filter);
  else
    menu.addAction (tr ("Show filter"), this,
                    &history_dock_widget::handle_contextmenu_filter);

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
  bool prev_valid_row = false;
  for (const auto& it : rows)
    {
      if (it.isValid ())
        {
          if (prev_valid_row)
            text += '\n';
          text += it.data ().toString ();
          prev_valid_row = true;
        }
    }
  QApplication::clipboard ()->setText (text);
}

void history_dock_widget::handle_contextmenu_evaluate (bool)
{
  QItemSelectionModel *selectionModel = m_history_list_view->selectionModel ();
  QModelIndexList rows = selectionModel->selectedRows ();
  for (const auto& it : rows)
    if (it.isValid ())
      emit command_double_clicked (it.data ().toString ());
}

void history_dock_widget::handle_contextmenu_create_script (bool)
{
  QString text;
  QItemSelectionModel *selectionModel = m_history_list_view->selectionModel ();
  QModelIndexList rows = selectionModel->selectedRows ();

  bool prev_valid_row = false;
  for (const auto& it : rows)
    {
      if (it.isValid ())
        {
          if (prev_valid_row)
            text += '\n';
          text += it.data ().toString ();
          prev_valid_row = true;
        }
    }

  if (text.length () > 0)
    emit command_create_script (text);
}

void history_dock_widget::handle_contextmenu_filter ()
{
  m_filter_shown = ! m_filter_shown;
  m_filter_widget->setVisible (m_filter_shown);

  set_filter_focus (m_filter_shown && m_filter_checkbox->isChecked ()) ;
}

void history_dock_widget::copyClipboard ()
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

void history_dock_widget::pasteClipboard ()
{
  if (m_filter->lineEdit ()->hasFocus ())
    {
      QClipboard *clipboard = QApplication::clipboard ();
      QString str = clipboard->text ();
      if (str.length () > 0)
        m_filter->lineEdit ()->insert (str);
    }
}

void history_dock_widget::selectAll ()
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

void history_dock_widget::construct ()
{
  m_history_model = new QStringListModel ();
  m_sort_filter_proxy_model.setSourceModel (m_history_model);
  m_history_list_view = new QListView (this);
  m_history_list_view->setModel (&m_sort_filter_proxy_model);
  m_history_list_view->setAlternatingRowColors (true);
  m_history_list_view->setEditTriggers (QAbstractItemView::NoEditTriggers);
  m_history_list_view->setStatusTip
    (tr ("Double-click a command to transfer it to the Command Window."));
  m_history_list_view->setSelectionMode (QAbstractItemView::ExtendedSelection);
  m_history_list_view->setContextMenuPolicy (Qt::CustomContextMenu);
  connect (m_history_list_view, &QListView::customContextMenuRequested,
           this, &history_dock_widget::ctxMenu);

  m_filter = new QComboBox (this);
  m_filter->setToolTip (tr ("Enter text to filter the command history"));
  m_filter->setEditable (true);
  m_filter->setMaxCount (MaxFilterHistory);
  m_filter->setInsertPolicy (QComboBox::NoInsert);
  m_filter->setSizeAdjustPolicy
    (QComboBox::AdjustToMinimumContentsLengthWithIcon);
  QSizePolicy sizePol (QSizePolicy::Expanding, QSizePolicy::Preferred);
  m_filter->setSizePolicy (sizePol);
  m_filter->completer ()->setCaseSensitivity (Qt::CaseSensitive);

  QLabel *filter_label = new QLabel (tr ("Filter"));

  m_filter_checkbox = new QCheckBox ();

  set_title (tr ("Command History"));
  setWidget (new QWidget ());

  m_filter_widget = new QWidget (this);
  QHBoxLayout *filter_layout = new QHBoxLayout ();
  filter_layout->addWidget (filter_label);
  filter_layout->addWidget (m_filter_checkbox);
  filter_layout->addWidget (m_filter);
  filter_layout->setContentsMargins (0, 0, 0, 0);
  m_filter_widget->setLayout (filter_layout);

  QVBoxLayout *hist_layout = new QVBoxLayout ();
  hist_layout->addWidget (m_filter_widget);
  hist_layout->addWidget (m_history_list_view);

  hist_layout->setContentsMargins (2, 2, 2, 2);
  hist_layout->setSpacing (0);
  widget ()->setLayout (hist_layout);

  // Init state of the filter

  gui_settings settings;

  m_filter_shown = settings.bool_value (hw_filter_shown);
  m_filter_widget->setVisible (m_filter_shown);

  m_filter->addItems (settings.string_list_value (hw_mru_list));

  bool filter_state = settings.bool_value (hw_filter_active);
  m_filter_checkbox->setChecked (filter_state);
  filter_activate (filter_state);

  // Connect signals and slots
  connect (m_filter, &QComboBox::editTextChanged,
           &m_sort_filter_proxy_model,
           &QSortFilterProxyModel::setFilterWildcard);
  connect (m_filter_checkbox, &QCheckBox::toggled,
           this, &history_dock_widget::filter_activate);
  connect (m_filter->lineEdit (), &QLineEdit::editingFinished,
           this, &history_dock_widget::update_filter_history);

  connect (m_history_list_view, &QListView::doubleClicked,
           this, &history_dock_widget::handle_double_click);

  m_history_list_view->setTextElideMode (Qt::ElideRight);
}

void history_dock_widget::notice_settings ()
{
  gui_settings settings;

  QFont font = QFont ();

  font.setStyleHint (QFont::TypeWriter);
  QString default_font = settings.string_value (global_mono_font);

  font.setFamily (settings.value (cs_font.settings_key (), default_font).toString ());
  font.setPointSize (settings.int_value (cs_font_size));

  m_history_list_view->setFont (font);
}

OCTAVE_END_NAMESPACE(octave)
