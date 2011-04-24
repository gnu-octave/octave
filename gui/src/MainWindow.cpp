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
#include <QFileDialog>
#include "MainWindow.h"
#include "FileEditorMdiSubWindow.h"
#include "ImageViewerMdiSubWindow.h"

MainWindow::MainWindow(QWidget *parent)
    : QMainWindow(parent),
      m_isRunning(true) {
    QDesktopServices desktopServices;
    m_settingsFile = desktopServices.storageLocation(QDesktopServices::HomeLocation) + "/.quint/settings.ini";
    constructWindow();
    establishOctaveLink();
}

MainWindow::~MainWindow() {
}

void MainWindow::handleOpenFileRequest(QString fileName) {
    reportStatusMessage(tr("Opening file."));
    QPixmap pixmap;
    if(pixmap.load(fileName)) {
        ImageViewerMdiSubWindow *subWindow = new ImageViewerMdiSubWindow(pixmap, this);
        m_openedFiles->addSubWindow(subWindow);
        subWindow->setWindowTitle(fileName);
        subWindow->showMaximized();
    } else {
        FileEditorMdiSubWindow *subWindow = new FileEditorMdiSubWindow(m_openedFiles);
        m_openedFiles->addSubWindow(subWindow);
        subWindow->loadFile(fileName);
        subWindow->showMaximized();
    }
    m_centralTabWidget->setCurrentWidget(m_openedFiles);
}

void MainWindow::reportStatusMessage(QString statusMessage) {
    m_statusBar->showMessage(statusMessage, 1000);
}

void MainWindow::openWebPage(QString url) {
    m_browserWidget->load(QUrl(url));
}

void MainWindow::handleSaveWorkspaceRequest() {
    QDesktopServices desktopServices;
    QString selectedFile = QFileDialog::getSaveFileName(this, tr("Save Workspace"),
        desktopServices.storageLocation(QDesktopServices::HomeLocation) + "/.quint/workspace");
    m_octaveTerminal->sendText(QString("save \'%1\'\n").arg(selectedFile));
    m_octaveTerminal->setFocus();
}

void MainWindow::handleLoadWorkspaceRequest() {
    QDesktopServices desktopServices;
    QString selectedFile = QFileDialog::getOpenFileName(this, tr("Load Workspace"),
        desktopServices.storageLocation(QDesktopServices::HomeLocation) + "/.quint/workspace");
    m_octaveTerminal->sendText(QString("load \'%1\'\n").arg(selectedFile));
    m_octaveTerminal->setFocus();
}

void MainWindow::handleClearWorkspaceRequest() {
    m_octaveTerminal->sendText("clear\n");
    m_octaveTerminal->setFocus();
}

void MainWindow::handleCommandDoubleClicked(QString command) {
    m_octaveTerminal->sendText(command);
    m_centralTabWidget->setCurrentWidget(m_octaveTerminal);
    m_octaveTerminal->setFocus();
}

void MainWindow::closeEvent(QCloseEvent *closeEvent) {
    m_isRunning = false;
    reportStatusMessage(tr("Saving data and shutting down."));
    writeSettings();

    m_octaveCallbackThread->terminate();
    m_octaveCallbackThread->wait();

    m_octaveMainThread->terminate();
    QMainWindow::closeEvent(closeEvent);
}

void MainWindow::readSettings() {
    QSettings settings(m_settingsFile, QSettings::IniFormat);
    restoreGeometry(settings.value("MainWindow/geometry").toByteArray());
    restoreState(settings.value("MainWindow/windowState").toByteArray());
}

void MainWindow::writeSettings() {
    QSettings settings(m_settingsFile, QSettings::IniFormat);
    settings.setValue("MainWindow/geometry", saveGeometry());
    settings.setValue("MainWindow/windowState", saveState());
}

void MainWindow::constructWindow() {
    QStyle *style = QApplication::style();
    m_octaveTerminal = new OctaveTerminal(this);
    m_generalPurposeToolbar = new QToolBar(tr("Octave Toolbar"), this);
    m_variablesDockWidget = new VariablesDockWidget(this);
    m_historyDockWidget = new HistoryDockWidget(this);
    m_filesDockWidget = new FilesDockWidget(this);
    m_openedFiles = new QMdiArea(this);
    m_statusBar = new QStatusBar(this);
    m_browserWidget = new BrowserWidget(this);
    m_serviceWidget = new BrowserWidget(this);
    m_plotterWidget = new PlotterWidget(this);
    m_centralTabWidget = new QTabWidget(this);
    m_centralTabWidget->addTab(m_octaveTerminal, tr("Command Window"));
    m_centralTabWidget->addTab(m_openedFiles, tr("File Editor"));
    m_centralTabWidget->addTab(m_plotterWidget, tr("Plotter"));
    m_centralTabWidget->addTab(m_browserWidget, tr("Documentation"));
    m_centralTabWidget->addTab(m_serviceWidget, tr("Service"));

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

    readSettings();

    connect(m_filesDockWidget, SIGNAL(openFile(QString)), this, SLOT(handleOpenFileRequest(QString)));
    connect(m_historyDockWidget, SIGNAL(information(QString)), this, SLOT(reportStatusMessage(QString)));
    connect(m_historyDockWidget, SIGNAL(commandDoubleClicked(QString)), this, SLOT(handleCommandDoubleClicked(QString)));
    connect(m_variablesDockWidget, SIGNAL(saveWorkspace()), this, SLOT(handleSaveWorkspaceRequest()));
    connect(m_variablesDockWidget, SIGNAL(loadWorkspace()), this, SLOT(handleLoadWorkspaceRequest()));
    connect(m_variablesDockWidget, SIGNAL(clearWorkspace()), this, SLOT(handleClearWorkspaceRequest()));

    openWebPage("http://www.gnu.org/software/octave/doc/interpreter/");
    m_serviceWidget->load(QUrl("http://powerup.ath.cx/quint"));
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
    reportStatusMessage(tr("Established link to Octave."));
}
