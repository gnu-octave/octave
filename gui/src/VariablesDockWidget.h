#ifndef VARIABLESDOCKWIDGET_H
#define VARIABLESDOCKWIDGET_H

#include <QDockWidget>
#include <QTreeWidget>
#include "OctaveLink.h"

class VariablesDockWidget : public QDockWidget
{
public:
    VariablesDockWidget(QWidget *parent = 0);
    void setVariablesList(QList<SymbolRecord> symbolTable);

private:
    void construct();
    void updateTreeEntry(QTreeWidgetItem *treeItem, SymbolRecord symbolRecord);
    void updateScope(int topLevelItemIndex, QList<SymbolRecord> symbolTable);
    QTreeWidget *m_variablesTreeWidget;
};

#endif // VARIABLESDOCKWIDGET_H
