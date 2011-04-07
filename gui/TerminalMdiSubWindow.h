#ifndef TERMINALMDISUBWINDOW_H
#define TERMINALMDISUBWINDOW_H

#include <QMdiSubWindow>
#include "QTerminalWidget.h"

class TerminalMdiSubWindow : public QMdiSubWindow {
    Q_OBJECT
public:
    TerminalMdiSubWindow();

private slots:
    void launchTerminal();

private:
    QTerminalWidget *m_terminalWidget;
};

#endif // TERMINALMDISUBWINDOW_H
