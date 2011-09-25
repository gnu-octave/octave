/* OctaveGUI - A graphical user interface for Octave
 * Copyright (C) 2011 Jacob Dawid (jacob.dawid@googlemail.com)
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */

#ifndef HISTORYDOCKWIDGET_H
#define HISTORYDOCKWIDGET_H

#include <QDockWidget>
#include <QLineEdit>
#include <QListView>
#include <QSortFilterProxyModel>
#include "OctaveLink.h"

class HistoryDockWidget:public QDockWidget
{
Q_OBJECT
public:
  HistoryDockWidget (QWidget *parent = 0);
  void updateHistory (QStringList history);

public slots:
  /** Tells the widget to notice settings that are probably new. */
  void noticeSettings ();
  void handleVisibilityChanged (bool visible);

signals:
  void information (QString message);
  void commandDoubleClicked (QString command);
  /** Custom signal that tells if a user has clicke away that dock widget. */
  void activeChanged (bool active);
protected:
  void closeEvent (QCloseEvent *event);
private slots:
  void handleDoubleClick (QModelIndex modelIndex);

private:
  void construct ();
  QListView *m_historyListView;
  QLineEdit *m_filterLineEdit;
  QSortFilterProxyModel m_sortFilterProxyModel;
};

#endif // HISTORYDOCKWIDGET_H
