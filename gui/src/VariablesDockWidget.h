#ifndef VARIABLESDOCKWIDGET_H
#define VARIABLESDOCKWIDGET_H

#include <QDockWidget>
#include <QTreeWidget>
#include "OctaveLink.h"

class VariablesDockWidget : public QDockWidget
{
public:
    VariablesDockWidget(QWidget *parent = 0);
    void setVariablesList(QList<SymbolRecord> variablesList);

private:
    void construct();
    void updateTreeEntry(QTreeWidgetItem *treeItem, SymbolRecord symbolRecord);

    QTreeWidget *m_variablesTreeWidget;
};

#endif // VARIABLESDOCKWIDGET_H
