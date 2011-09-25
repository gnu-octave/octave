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

#include "VariablesDockWidget.h"
#include <QHBoxLayout>
#include <QVBoxLayout>
#include <QPushButton>

VariablesDockWidget::VariablesDockWidget (QWidget * parent):QDockWidget
  (parent)
{
  setObjectName ("VariablesDockWidget");
  construct ();

  connect (OctaveLink::instance(), SIGNAL (symbolTableChanged()), this, SLOT (fetchSymbolTable()));
}

void
VariablesDockWidget::construct ()
{
  setWindowTitle (tr ("Workspace"));

  m_updateSemaphore = new QSemaphore (1);
  QStringList headerLabels;
  headerLabels << tr ("Name") << tr ("Type") << tr ("Value");
  m_variablesTreeWidget = new QTreeWidget (this);
  m_variablesTreeWidget->setHeaderHidden (false);
  m_variablesTreeWidget->setHeaderLabels (headerLabels);

  setWidget (new QWidget (this));
  QVBoxLayout *layout = new QVBoxLayout ();
  layout->addWidget (m_variablesTreeWidget);
  layout->setMargin (2);
  widget ()->setLayout (layout);

  QTreeWidgetItem *treeWidgetItem = new QTreeWidgetItem ();
  treeWidgetItem->setData (0, 0, QString (tr ("Local")));
  m_variablesTreeWidget->insertTopLevelItem (0, treeWidgetItem);

  treeWidgetItem = new QTreeWidgetItem ();
  treeWidgetItem->setData (0, 0, QString (tr ("Global")));
  m_variablesTreeWidget->insertTopLevelItem (1, treeWidgetItem);

  treeWidgetItem = new QTreeWidgetItem ();
  treeWidgetItem->setData (0, 0, QString (tr ("Persistent")));
  m_variablesTreeWidget->insertTopLevelItem (2, treeWidgetItem);

  treeWidgetItem = new QTreeWidgetItem ();
  treeWidgetItem->setData (0, 0, QString (tr ("Hidden")));
  m_variablesTreeWidget->insertTopLevelItem (3, treeWidgetItem);

  m_variablesTreeWidget->expandAll ();
  m_variablesTreeWidget->setAlternatingRowColors (true);
  m_variablesTreeWidget->setAnimated (true);

  connect (this, SIGNAL (visibilityChanged(bool)), this, SLOT(handleVisibilityChanged(bool)));
}

void
VariablesDockWidget::updateTreeEntry (QTreeWidgetItem * treeItem,
				      SymbolRecord symbolRecord)
{
  treeItem->setData (0, 0, QString (symbolRecord.name ().c_str ()));
  treeItem->setData (1, 0,
		     QString (symbolRecord.varval ().type_name ().c_str ()));
  treeItem->setData (2, 0,
		     OctaveLink::octaveValueAsQString (symbolRecord.
						       varval ()));
}

void
VariablesDockWidget::setVariablesList (QList < SymbolRecord > symbolTable)
{
  m_updateSemaphore->acquire ();
  // Split the symbol table into its different scopes.
  QList < SymbolRecord > localSymbolTable;
  QList < SymbolRecord > globalSymbolTable;
  QList < SymbolRecord > persistentSymbolTable;
  QList < SymbolRecord > hiddenSymbolTable;

  foreach (SymbolRecord symbolRecord, symbolTable)
  {
    // It's true that being global or hidden includes it's can mean it's also locally visible,
    // but we want to distinguish that here.
    if (symbolRecord.is_local () && !symbolRecord.is_global ()
	&& !symbolRecord.is_hidden ())
      {
	localSymbolTable.append (symbolRecord);
      }

    if (symbolRecord.is_global ())
      {
	globalSymbolTable.append (symbolRecord);
      }

    if (symbolRecord.is_persistent ())
      {
	persistentSymbolTable.append (symbolRecord);
      }

    if (symbolRecord.is_hidden ())
      {
	hiddenSymbolTable.append (symbolRecord);
      }
  }

  updateScope (0, localSymbolTable);
  updateScope (1, globalSymbolTable);
  updateScope (2, persistentSymbolTable);
  updateScope (3, hiddenSymbolTable);
  m_updateSemaphore->release ();
}

void
VariablesDockWidget::updateScope (int topLevelItemIndex,
				  QList < SymbolRecord > symbolTable)
{
  // This method may be a little bit confusing; variablesList is a complete list of all
  // variables that are in the workspace currently.
  QTreeWidgetItem *topLevelItem =
    m_variablesTreeWidget->topLevelItem (topLevelItemIndex);

  // First we check, if any variables that exist in the model tree have to be updated
  // or created. So we walk the variablesList check against the tree.
  foreach (SymbolRecord symbolRecord, symbolTable)
  {
    int childCount = topLevelItem->childCount ();
    bool alreadyExists = false;
    QTreeWidgetItem *child;

    // Search for the corresponding item in the tree. If it has been found, child
    // will contain the appropriate QTreeWidgetItem* pointing at it.
    for (int i = 0; i < childCount; i++)
      {
	child = topLevelItem->child (i);
	if (child->data (0, 0).toString () ==
	    QString (symbolRecord.name ().c_str ()))
	  {
	    alreadyExists = true;
	    break;
	  }
      }

    // If it already exists, just update it.
    if (alreadyExists)
      {
	updateTreeEntry (child, symbolRecord);
      }
    else
      {
	// It does not exist, so create a new one and set the right values.
	child = new QTreeWidgetItem ();
	updateTreeEntry (child, symbolRecord);
	topLevelItem->addChild (child);
      }
  }

  // Check the tree against the list for deleted variables.
  for (int i = 0; i < topLevelItem->childCount (); i++)
    {
      bool existsInVariableList = false;
      QTreeWidgetItem *child = topLevelItem->child (i);
      foreach (SymbolRecord symbolRecord, symbolTable)
      {
	if (QString (symbolRecord.name ().c_str ()) ==
	    child->data (0, 0).toString ())
	  {
	    existsInVariableList = true;
	  }
      }

      if (!existsInVariableList)
	{
	  topLevelItem->removeChild (child);
	  delete child;
	  i--;
	}
    }
}

void
VariablesDockWidget::noticeSettings ()
{

}

void
VariablesDockWidget::fetchSymbolTable ()
{
  QList < SymbolRecord > symbolTable = OctaveLink::instance ()->copyCurrentSymbolTable ();
  setVariablesList (symbolTable);
}

void
VariablesDockWidget::handleVisibilityChanged (bool visible)
{
  if (visible)
    emit activeChanged (true);
}

void
VariablesDockWidget::closeEvent (QCloseEvent *event)
{
  emit activeChanged (false);
  QDockWidget::closeEvent (event);
}
