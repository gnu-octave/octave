#include "terminal.h"

Terminal::Terminal()
    : QMdiSubWindow(),
      m_terminalWidget(0) {
    setWindowTitle("Terminal Session");
    launchTerminal();
}

void Terminal::launchTerminal() {
    delete m_terminalWidget;
    m_terminalWidget = new QTermWidget(1, this);
    m_terminalWidget->setScrollBarPosition(QTermWidget::ScrollBarRight);
    setWidget(m_terminalWidget);

    connect(m_terminalWidget, SIGNAL(finished()), this, SLOT(launchTerminal()));
}
