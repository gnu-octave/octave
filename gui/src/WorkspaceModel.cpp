/* OctaveGUI - A graphical user interface for Octave
 * Copyright (C) 2011 Jacob Dawid (jacob.dawid@googlemail.com)
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */

#include "WorkspaceModel.h"
#include <QTreeWidget>
#include <QTime>

WorkspaceModel::WorkspaceModel(QObject *parent)
  : QAbstractItemModel(parent)
{
  QList<QVariant> rootData;
  rootData << tr ("Name") << tr ("Type") << tr ("Value");
  _rootItem = new TreeItem(rootData);
  _cachedSymbolTableSemaphore = new QSemaphore (1);
}

WorkspaceModel::~WorkspaceModel()
{
  delete _rootItem;
}

QModelIndex
WorkspaceModel::index(int row, int column, const QModelIndex &parent) const
{
  if (!hasIndex(row, column, parent))
    return QModelIndex();

  TreeItem *parentItem;

  if (!parent.isValid())
    parentItem = _rootItem;
  else
    parentItem = static_cast<TreeItem*>(parent.internalPointer());

  TreeItem *childItem = parentItem->child(row);
  if (childItem)
    return createIndex(row, column, childItem);
  else
    return QModelIndex();
}

QModelIndex
WorkspaceModel::parent(const QModelIndex &index) const
{
  if (!index.isValid())
    return QModelIndex();

  TreeItem *childItem = static_cast<TreeItem*>(index.internalPointer());
  TreeItem *parentItem = childItem->parent();

  if (parentItem == _rootItem)
    return QModelIndex();

  return createIndex(parentItem->row(), 0, parentItem);
}

int
WorkspaceModel::rowCount(const QModelIndex &parent) const
{
  TreeItem *parentItem;
  if (parent.column() > 0)
    return 0;

  if (!parent.isValid())
    parentItem = _rootItem;
  else
    parentItem = static_cast<TreeItem*>(parent.internalPointer());

  return parentItem->childCount();
}

int
WorkspaceModel::columnCount(const QModelIndex &parent) const
{
  if (parent.isValid())
    return static_cast<TreeItem*>(parent.internalPointer())->columnCount();
  else
    return _rootItem->columnCount();
}

void
WorkspaceModel::insertTopLevelItem(int at, TreeItem *treeItem)
{
  _rootItem->insertChildItem(at, treeItem);
}

TreeItem *
WorkspaceModel::topLevelItem (int at)
{
  return _rootItem->child(at);
}

Qt::ItemFlags
WorkspaceModel::flags(const QModelIndex &index) const
{
  if (!index.isValid())
    return 0;

  return Qt::ItemIsEnabled | Qt::ItemIsSelectable;
}

QVariant
WorkspaceModel::headerData(int section, Qt::Orientation orientation, int role) const
{
  if (orientation == Qt::Horizontal && role == Qt::DisplayRole)
    return _rootItem->data(section);

  return QVariant();
}

QVariant
WorkspaceModel::data(const QModelIndex &index, int role) const
{
  if (!index.isValid())
    return QVariant();

  if (role != Qt::DisplayRole)
    return QVariant();

  TreeItem *item = static_cast<TreeItem*>(index.internalPointer());

  return item->data(index.column());
}


void
WorkspaceModel::cacheSymbolTable ()
{
  std::list < symbol_table::symbol_record > symbolTable = symbol_table::all_variables ();

  _cachedSymbolTableSemaphore->acquire (1);
  _cachedSymbolTable.clear();
  for (std::list < symbol_table::symbol_record > ::iterator iterator = symbolTable.begin ();
       iterator != symbolTable.end (); iterator++)
    {
      _cachedSymbolTable.push_back((*iterator).dup(symbol_table::global_scope()));
    }
  _cachedSymbolTableSemaphore->release (1);
}

void
WorkspaceModel::updateFromSymbolTable ()
{
  // Split the symbol table into its different categories.
  QList < symbol_table::symbol_record* > localSymbolTable;
  QList < symbol_table::symbol_record* > globalSymbolTable;
  QList < symbol_table::symbol_record* > persistentSymbolTable;
  QList < symbol_table::symbol_record* > hiddenSymbolTable;

  _cachedSymbolTableSemaphore->acquire (1);
  for (std::list < symbol_table::symbol_record > ::iterator iterator = _cachedSymbolTable.begin ();
       iterator != _cachedSymbolTable.end (); iterator++)
    {
      // It's true that being global or hidden includes it's can mean it's also locally visible,
      // but we want to distinguish that here.
      if (iterator->is_local () && !iterator->is_global () && !iterator->is_hidden ())
        {
          localSymbolTable.append (&(*iterator));
        }

      if (iterator->is_global ())
        {
          globalSymbolTable.append (&(*iterator));
        }

      if (iterator->is_persistent ())
        {
          persistentSymbolTable.append (&(*iterator));
        }

      if (iterator->is_hidden ())
        {
          hiddenSymbolTable.append (&(*iterator));
        }
    }

  updateCategory (0, localSymbolTable);
  updateCategory (1, globalSymbolTable);
  updateCategory (2, persistentSymbolTable);
  updateCategory (3, hiddenSymbolTable);
 _cachedSymbolTableSemaphore->release (1);
  reset();
  emit expandRequest();
}

void
WorkspaceModel::updateCategory (int topLevelItemIndex, const QList < symbol_table::symbol_record* > &symbolTable)
{
  TreeItem *treeItem = topLevelItem (topLevelItemIndex);

  QModelIndex mi = index(treeItem->row(), 0);
  treeItem->deleteChildItems();

  int symbolTableSize = symbolTable.size ();
  for(int j = 0; j < symbolTableSize; j++)
    {
      TreeItem *child = new TreeItem ();
      updateTreeEntry (child, symbolTable[j]);
      treeItem->addChild (child);
    }
}

void
WorkspaceModel::updateTreeEntry (TreeItem * treeItem, symbol_table::symbol_record *symbolRecord)
{
  treeItem->setData (0, QString (symbolRecord->name ().c_str ()));
  treeItem->setData (1, QString (symbolRecord->varval ().type_name ().c_str ()));
  treeItem->setData (2, octaveValueAsQString (symbolRecord->varval ()));
}

QString
WorkspaceModel::octaveValueAsQString (const octave_value& octaveValue)
{
  // Convert single qouted string.
  if (octaveValue.is_sq_string ())
    {
      return QString ("\'%1\'").arg (octaveValue.string_value ().c_str ());

      // Convert double qouted string.
    }
  else if (octaveValue.is_dq_string ())
    {
      return QString ("\"%1\"").arg (octaveValue.string_value ().c_str ());

      // Convert real scalar.
    }
  else if (octaveValue.is_real_scalar ())
    {
      return QString ("%1").arg (octaveValue.scalar_value ());

      // Convert complex scalar.
    }
  else if (octaveValue.is_complex_scalar ())
    {
      return QString ("%1 + %2i").arg (octaveValue.scalar_value ()).
          arg (octaveValue.complex_value ().imag ());

      // Convert range.
    }
  else if (octaveValue.is_range ())
    {
      return QString ("%1 : %2 : %3").arg (octaveValue.range_value ().
                                           base ()).arg (octaveValue.
                                                         range_value ().
                                                         inc ()).
          arg (octaveValue.range_value ().limit ());

      // Convert real matrix.
    }
  else if (octaveValue.is_real_matrix ())
    {
      return QString ("%1x%2 matrix")
          .arg (octaveValue.matrix_value ().rows ())
          .arg (octaveValue.matrix_value ().cols ());

      // Convert complex matrix.
    }
  else if (octaveValue.is_complex_matrix ())
    {
      return QString ("%1x%2 complex matrix")
          .arg (octaveValue.matrix_value ().rows ())
          .arg (octaveValue.matrix_value ().cols ());

      // If everything else does not fit, we could not recognize the type.
    }
  else
    {
      return QString ("<Type not recognized>");
    }
}
