#ifndef TERMINALMDISUBWINDOW_H
#define TERMINALMDISUBWINDOW_H

#include <QMdiSubWindow>
#include <QTreeView>
#include <QListView>
#include <QStatusBar>
#include "QTerminalWidget.h"

class TerminalMdiSubWindow : public QMdiSubWindow {
    Q_OBJECT
public:
    TerminalMdiSubWindow(QWidget *parent = 0);

private slots:

private:
    void constructWindow();
    QTerminalWidget *m_terminalWidget;
    QTreeView *m_variableView;
    QListView *m_commandHistoryView;
    QStatusBar *m_statusBar;
};

#endif // TERMINALMDISUBWINDOW_H
