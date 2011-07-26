/* OctaveGUI - A graphical user interface for Octave
 * Copyright (C) 2011 Jacob Dawid
 * jacob.dawid@googlemail.com
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#include "HistoryDockWidget.h"
#include <QVBoxLayout>

HistoryDockWidget::HistoryDockWidget (QWidget * parent):QDockWidget (parent)
{
  setObjectName ("HistoryDockWidget");
  construct ();
}

void
HistoryDockWidget::construct ()
{
  m_sortFilterProxyModel.setSourceModel(OctaveLink::instance ()->historyModel());
  m_historyListView = new QListView (this);
  m_historyListView->setModel (&m_sortFilterProxyModel);
  m_historyListView->setAlternatingRowColors (true);
  m_historyListView->setEditTriggers (QAbstractItemView::NoEditTriggers);
  m_filterLineEdit = new QLineEdit (this);
  QVBoxLayout *layout = new QVBoxLayout ();

  setWindowTitle (tr ("Command History"));
  setWidget (new QWidget ());

  layout->addWidget (m_historyListView);
  layout->addWidget (m_filterLineEdit);
  layout->setMargin (2);

  widget ()->setLayout (layout);

  connect (m_filterLineEdit, SIGNAL (textEdited (QString)), this, SLOT (setFilter (QString)));
}

void
HistoryDockWidget::noticeSettings ()
{

}

void
HistoryDockWidget::setFilter(QString filter)
{
  m_historyListView->setEnabled (false);
  m_sortFilterProxyModel.setFilterWildcard ( QString ("*%1*").arg (filter));
  m_historyListView->setEnabled (true);
}
