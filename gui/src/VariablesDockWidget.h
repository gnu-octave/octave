#ifndef VARIABLESDOCKWIDGET_H
#define VARIABLESDOCKWIDGET_H

#include <QDockWidget>
#include <QTreeWidget>
#include "OctaveLink.h"

class VariablesDockWidget : public QDockWidget
{
public:
    VariablesDockWidget(QWidget *parent = 0);
    void setVariablesList(QList<OctaveLink::VariableMetaData> variablesList);

private:
    void construct();

    QTreeWidget *m_variablesTreeWidget;
};

#endif // VARIABLESDOCKWIDGET_H
