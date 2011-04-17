/* Quint - A graphical user interface for Octave
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
#include <QHBoxLayout>

HistoryDockWidget::HistoryDockWidget(QWidget *parent)
    : QDockWidget(parent) {
    setObjectName("HistoryDockWidget");
    construct();
}

void HistoryDockWidget::handleListViewItemDoubleClicked(QModelIndex modelIndex) {
    QString command = m_historyListModel->data(modelIndex, 0).toString();
    emit commandDoubleClicked(command);
}

void HistoryDockWidget::construct() {
    m_historyListModel = new QStringListModel();
    m_historyListView = new QListView(this);
    m_historyListView->setModel(m_historyListModel);
    m_historyListView->setAlternatingRowColors(true);
    m_historyListView->setEditTriggers(QAbstractItemView::NoEditTriggers);
    QHBoxLayout *layout = new QHBoxLayout();

    setWindowTitle(tr("Command History"));
    setWidget(new QWidget());

    layout->addWidget(m_historyListView);
    layout->setMargin(2);

    widget()->setLayout(layout);
    connect(m_historyListView, SIGNAL(doubleClicked(QModelIndex)), this, SLOT(handleListViewItemDoubleClicked(QModelIndex)));
}

void HistoryDockWidget::updateHistory(string_vector historyEntries) {
    QStringList stringList = m_historyListModel->stringList();
    for(int i = 0; i < historyEntries.length(); i++) {
        QString command(historyEntries[i].c_str());
        if(!command.startsWith("#")) {
            stringList.push_front(command);
        }
    }
    m_historyListModel->setStringList(stringList);
    emit information(tr("History updated."));
}
