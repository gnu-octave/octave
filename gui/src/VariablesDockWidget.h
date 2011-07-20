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

#ifndef VARIABLESDOCKWIDGET_H
#define VARIABLESDOCKWIDGET_H

#include <QDockWidget>
#include <QTreeWidget>
#include <QSemaphore>
#include "OctaveLink.h"

class VariablesDockWidget:public QDockWidget
{
Q_OBJECT public:
  VariablesDockWidget (QWidget * parent = 0);
  void setVariablesList (QList < SymbolRecord > symbolTable);

private:
  void construct ();
  void updateTreeEntry (QTreeWidgetItem * treeItem,
			SymbolRecord symbolRecord);
  void updateScope (int topLevelItemIndex,
		    QList < SymbolRecord > symbolTable);
  QTreeWidget *m_variablesTreeWidget;
  QSemaphore *m_updateSemaphore;
};

#endif // VARIABLESDOCKWIDGET_H
