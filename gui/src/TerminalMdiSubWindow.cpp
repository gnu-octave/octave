#include "TerminalMdiSubWindow.h"
#include <QHBoxLayout>
#include <QVBoxLayout>

TerminalMdiSubWindow::TerminalMdiSubWindow(QWidget *parent)
    : QMdiSubWindow(parent),
      m_terminalWidget(0) {
    constructWindow();
}

void TerminalMdiSubWindow::constructWindow() {
    setWindowTitle("Octave Session");
    resize(800, 400);
    setWidget(new QWidget(this));

    QVBoxLayout *vBoxLayout = new QVBoxLayout();

        QWidget *hWidget = new QWidget();
        QHBoxLayout *hBoxLayout = new QHBoxLayout();

        m_terminalWidget = new QTerminalWidget(0, hWidget);
        m_terminalWidget->setScrollBarPosition(QTerminalWidget::ScrollBarRight);
        m_terminalWidget->setShellProgram("octave");
        m_terminalWidget->startShellProgram();
        m_terminalWidget->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
        m_variableView = new QTreeView(hWidget);
        m_variableView->setMaximumWidth(200);

        hBoxLayout->addWidget(m_terminalWidget);
        hBoxLayout->addWidget(m_variableView);
        hWidget->setLayout(hBoxLayout);

        m_statusBar = new QStatusBar();

    vBoxLayout->addWidget(hWidget);
    vBoxLayout->addWidget(m_statusBar);
    widget()->setLayout(vBoxLayout);

    m_statusBar->showMessage("Ready.");
}
