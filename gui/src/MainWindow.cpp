/* Quint - A graphical user interface for Octave
 * Copyright (C) 2011 Jacob Dawid
 * jacob.dawid@googlemail.com
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#include <QMenuBar>
#include <QMenu>
#include <QAction>
#include <QSettings>
#include <QDesktopServices>
#include "MainWindow.h"
#include "FileEditorMdiSubWindow.h"

MainWindow::MainWindow(QWidget *parent)
    : QMainWindow(parent),
      m_isRunning(true) {
    QDesktopServices desktopServices;
    m_settingsFile = desktopServices.storageLocation(QDesktopServices::HomeLocation) + "/.quint/setting.ini";
    constructWindow();
    establishOctaveLink();
}

MainWindow::~MainWindow() {
}

void MainWindow::handleOpenFileRequest(QString fileName) {
    reportStatusMessage("Opening file.");
    FileEditorMdiSubWindow *subWindow = new FileEditorMdiSubWindow(m_openedFiles);
    m_openedFiles->addSubWindow(subWindow);
    subWindow->loadFile(fileName);
    subWindow->showMaximized();
}

void MainWindow::reportStatusMessage(QString statusMessage) {
    m_statusBar->showMessage(statusMessage, 1000);
}

void MainWindow::openWebPage(QString url) {
    m_webView->load(QUrl(url));
}

void MainWindow::closeEvent(QCloseEvent *closeEvent) {
    reportStatusMessage("Saving data and shutting down.");
    QSettings settings(m_settingsFile, QSettings::IniFormat);
    settings.setValue("MainWindow/geometry", saveGeometry());
    settings.setValue("MainWindow/windowState", saveState());
    QMainWindow::closeEvent(closeEvent);
}

void MainWindow::constructWindow() {
    QStyle *style = QApplication::style();
    m_octaveTerminal = new OctaveTerminal(this);
    m_generalPurposeToolbar = new QToolBar("Octave Toolbar", this);
    m_variablesDockWidget = new VariablesDockWidget(this);
    m_historyDockWidget = new HistoryDockWidget(this);
    m_filesDockWidget = new FilesDockWidget(this);
    m_openedFiles = new QMdiArea(this);
    m_statusBar = new QStatusBar(this);
    m_webView = new QWebView(this);
    m_centralTabWidget = new QTabWidget(this);
    m_centralTabWidget->addTab(m_octaveTerminal, "Terminal");
    m_centralTabWidget->addTab(m_openedFiles, "Editor");
    m_centralTabWidget->addTab(m_webView, "Documentation");

    // TODO: Add meaningfull toolbar items.
    QAction *commandAction = new QAction(style->standardIcon(QStyle::SP_CommandLink),
        "", m_generalPurposeToolbar);
    QAction *computerAction = new QAction(style->standardIcon(QStyle::SP_ComputerIcon),
        "", m_generalPurposeToolbar);
    m_generalPurposeToolbar->addAction(commandAction);
    m_generalPurposeToolbar->addAction(computerAction);

    setWindowTitle("Octave");
    setCentralWidget(m_centralTabWidget);
    addToolBar(m_generalPurposeToolbar);
    addDockWidget(Qt::LeftDockWidgetArea, m_variablesDockWidget);
    addDockWidget(Qt::LeftDockWidgetArea, m_historyDockWidget);
    addDockWidget(Qt::RightDockWidgetArea, m_filesDockWidget);
    setStatusBar(m_statusBar);

    QSettings settings(m_settingsFile, QSettings::IniFormat);
    restoreGeometry(settings.value("MainWindow/geometry").toByteArray());
    restoreState(settings.value("MainWindow/windowState").toByteArray());

    connect(m_filesDockWidget, SIGNAL(openFile(QString)), this, SLOT(handleOpenFileRequest(QString)));
    connect(m_historyDockWidget, SIGNAL(information(QString)), this, SLOT(reportStatusMessage(QString)));

    openWebPage("http://www.gnu.org/software/octave/doc/interpreter/");
}

void MainWindow::establishOctaveLink() {
    m_octaveMainThread = new OctaveMainThread(this);
    m_octaveMainThread->start();

    m_octaveCallbackThread = new OctaveCallbackThread(this, this);
    m_octaveCallbackThread->start();

    command_editor::add_event_hook(OctaveLink::readlineEventHook);

    int fdm, fds;
    if(openpty(&fdm, &fds, 0, 0, 0) < 0) {
        assert(0);
    }
    dup2 (fds, 0);
    dup2 (fds, 1);
    dup2 (fds, 2);
    m_octaveTerminal->openTeletype(fdm);
    reportStatusMessage("Established link to Octave.");
}
