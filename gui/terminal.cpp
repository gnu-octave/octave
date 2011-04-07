#include "terminal.h"

Terminal::Terminal()
    : QMdiSubWindow(),
      m_terminalWidget(0) {
    setWindowTitle("Terminal Session");
    resize(800, 400);
    launchTerminal();
}

void Terminal::launchTerminal() {
    delete m_terminalWidget;
    m_terminalWidget = new QTermWidget(0, this);
    m_terminalWidget->setScrollBarPosition(QTermWidget::ScrollBarRight);
    setWidget(m_terminalWidget);

    QString programName = "octave";
    m_terminalWidget->setShellProgram(programName);
    m_terminalWidget->startShellProgram();
    setFocus();
    connect(m_terminalWidget, SIGNAL(finished()), this, SLOT(launchTerminal()));
}
