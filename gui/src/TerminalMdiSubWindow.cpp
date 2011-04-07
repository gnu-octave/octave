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
    resize(900, 600);
    setWidget(new QWidget(this));

    QVBoxLayout *vBoxLayout = new QVBoxLayout();

        QWidget *hWidget = new QWidget();
        QHBoxLayout *hBoxLayout = new QHBoxLayout();

        m_terminalWidget = new QTerminalWidget(0, hWidget);
        m_terminalWidget->setScrollBarPosition(QTerminalWidget::ScrollBarRight);
        m_terminalWidget->setShellProgram("octave");
        m_terminalWidget->startShellProgram();
        m_terminalWidget->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);

            QWidget *hvWidget = new QWidget();
            QVBoxLayout *hvBoxLayout = new QVBoxLayout();
            m_variableView = new QTreeView(hWidget);
            m_commandHistoryView = new QListView(hWidget);
            hvWidget->setMaximumWidth(250);
            hvBoxLayout->addWidget(new QLabel("Variables", hWidget));
            hvBoxLayout->addWidget(m_variableView);
            hvBoxLayout->addWidget(new QLabel("Command History", hWidget));
            hvBoxLayout->addWidget(m_commandHistoryView);
            hvBoxLayout->setMargin(1);
            hvWidget->setLayout(hvBoxLayout);

        hBoxLayout->addWidget(m_terminalWidget);
        hBoxLayout->addWidget(hvWidget);
        hBoxLayout->setMargin(2);
        hWidget->setLayout(hBoxLayout);

        m_statusBar = new QStatusBar();

    vBoxLayout->addWidget(hWidget);
    vBoxLayout->addWidget(m_statusBar);
    vBoxLayout->setMargin(2);
    widget()->setLayout(vBoxLayout);

    m_statusBar->showMessage("Ready.");
}
