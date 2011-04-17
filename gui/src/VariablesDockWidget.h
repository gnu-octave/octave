#ifndef VARIABLESDOCKWIDGET_H
#define VARIABLESDOCKWIDGET_H

#include <QDockWidget>
#include <QTreeWidget>
#include <QSemaphore>
#include "OctaveLink.h"

class VariablesDockWidget : public QDockWidget
{
    Q_OBJECT
public:
    VariablesDockWidget(QWidget *parent = 0);
    void setVariablesList(QList<SymbolRecord> symbolTable);

signals:
    void saveWorkspace();
    void loadWorkspace();
    void clearWorkspace();

private slots:
    void emitSaveWorkspace();
    void emitLoadWorkspace();
    void emitClearWorkspace();

private:
    void construct();
    void updateTreeEntry(QTreeWidgetItem *treeItem, SymbolRecord symbolRecord);
    void updateScope(int topLevelItemIndex, QList<SymbolRecord> symbolTable);
    QTreeWidget *m_variablesTreeWidget;
    QSemaphore *m_updateSemaphore;
};

#endif // VARIABLESDOCKWIDGET_H
