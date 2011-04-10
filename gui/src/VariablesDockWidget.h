#ifndef VARIABLESDOCKWIDGET_H
#define VARIABLESDOCKWIDGET_H

#include <QDockWidget>
#include <QTreeView>

class VariablesDockWidget : public QDockWidget
{
public:
    VariablesDockWidget(QWidget *parent = 0);
private:
    void construct();

    QTreeView *m_variablesTreeView;
};

#endif // VARIABLESDOCKWIDGET_H
