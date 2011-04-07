#ifndef TERMINAL_H
#define TERMINAL_H

#include <QMdiSubWindow>
#include "QTerminalWidget.h"

class Terminal : public QMdiSubWindow {
    Q_OBJECT
public:
    Terminal();

private slots:
    void launchTerminal();

private:
    QTerminalWidget *m_terminalWidget;
};

#endif // TERMINAL_H
