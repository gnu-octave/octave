#ifndef TERMINAL_H
#define TERMINAL_H

#include <QMdiSubWindow>
#include "qtermwidget.h"

class Terminal : public QMdiSubWindow {
    Q_OBJECT
public:
    Terminal();

private slots:
    void launchTerminal();

private:
    QTermWidget *m_terminalWidget;
};

#endif // TERMINAL_H
