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
#include "FileEditorDockWidget.h"
#include "ImageViewerDockWidget.h"

MainWindow::MainWindow(QWidget *parent)
    : QMainWindow(parent),
      m_isRunning(true) {
    setObjectName("MainWindow");

    QDesktopServices desktopServices;
    m_settingsFile = desktopServices.storageLocation(QDesktopServices::HomeLocation) + "/.quint/settings.ini";
    construct();
    establishOctaveLink();
}

MainWindow::~MainWindow() {
}

void MainWindow::handleOpenFileRequest(QString fileName) {
    reportStatusMessage(tr("Opening file."));
    QPixmap pixmap;
    if(pixmap.load(fileName)) {
        ImageViewerDockWidget *imageViewerDockWidget = new ImageViewerDockWidget(pixmap, this);
        imageViewerDockWidget->setWindowTitle(fileName);
        addDockWidget(Qt::RightDockWidgetArea, imageViewerDockWidget);
    } else {
        FileEditorDockWidget *fileEditorDockWidget = new FileEditorDockWidget(this);
        fileEditorDockWidget->loadFile(fileName);
        addDockWidget(Qt::RightDockWidgetArea, fileEditorDockWidget);
    }
}

void MainWindow::reportStatusMessage(QString statusMessage) {
    m_statusBar->showMessage(statusMessage, 1000);
}

void MainWindow::handleSaveWorkspaceRequest() {
    QDesktopServices desktopServices;
    QString selectedFile = QFileDialog::getSaveFileName(this, tr("Save Workspace"),
        desktopServices.storageLocation(QDesktopServices::HomeLocation) + "/.quint/workspace");
    m_octaveTerminalDockWidget->octaveTerminal()->sendText(QString("save \'%1\'\n").arg(selectedFile));
    m_octaveTerminalDockWidget->octaveTerminal()->setFocus();
}

void MainWindow::handleLoadWorkspaceRequest() {
    QDesktopServices desktopServices;
    QString selectedFile = QFileDialog::getOpenFileName(this, tr("Load Workspace"),
        desktopServices.storageLocation(QDesktopServices::HomeLocation) + "/.quint/workspace");
    m_octaveTerminalDockWidget->octaveTerminal()->sendText(QString("load \'%1\'\n").arg(selectedFile));
    m_octaveTerminalDockWidget->octaveTerminal()->setFocus();
}

void MainWindow::handleClearWorkspaceRequest() {
    m_octaveTerminalDockWidget->octaveTerminal()->sendText("clear\n");
    m_octaveTerminalDockWidget->octaveTerminal()->setFocus();
}

void MainWindow::handleCommandDoubleClicked(QString command) {
    m_octaveTerminalDockWidget->octaveTerminal()->sendText(command);
    m_octaveTerminalDockWidget->octaveTerminal()->setFocus();
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

void MainWindow::construct() {
    setWindowTitle("Octave");
    setWindowIcon(QIcon("../media/quint_icon_small.png"));
    QStyle *style = QApplication::style();
    resize(800, 600);

    m_octaveTerminalDockWidget = new OctaveTerminalDockWidget(this, new OctaveTerminal(this));
    m_variablesDockWidget = new VariablesDockWidget(this);
    m_historyDockWidget = new HistoryDockWidget(this);
    m_filesDockWidget = new FilesDockWidget(this);
    m_browserDockWidget = new BrowserDockWidget(this, new BrowserWidget(this));
    m_serviceDockWidget = new BrowserDockWidget(this, new BrowserWidget(this));

    m_browserDockWidget->setObjectName("BrowserWidget");
    m_browserDockWidget->setWindowTitle(tr("Documentation"));
    m_serviceDockWidget->setObjectName("ServiceWidget");
    m_serviceDockWidget->setWindowTitle(tr("Service"));

    // This is needed, since a QMainWindow without a central widget is not supported.
    setCentralWidget(new QWidget(this));
    centralWidget()->setObjectName("CentralWidget");
    centralWidget()->hide();

    setDockOptions(QMainWindow::AllowTabbedDocks | QMainWindow::AllowNestedDocks | QMainWindow::AnimatedDocks);

    addDockWidget(Qt::RightDockWidgetArea, m_octaveTerminalDockWidget);
    addDockWidget(Qt::LeftDockWidgetArea, m_variablesDockWidget);
    addDockWidget(Qt::LeftDockWidgetArea, m_historyDockWidget);
    addDockWidget(Qt::LeftDockWidgetArea, m_filesDockWidget);
    addDockWidget(Qt::LeftDockWidgetArea, m_browserDockWidget);
    addDockWidget(Qt::LeftDockWidgetArea, m_serviceDockWidget);

    // TODO: Add meaningfull toolbar items.
    m_generalPurposeToolbar = new QToolBar(tr("Octave Toolbar"), this);
    QAction *commandAction = new QAction(style->standardIcon(QStyle::SP_CommandLink),
        "", m_generalPurposeToolbar);
    QAction *computerAction = new QAction(style->standardIcon(QStyle::SP_ComputerIcon),
        "", m_generalPurposeToolbar);
    m_generalPurposeToolbar->addAction(commandAction);
    m_generalPurposeToolbar->addAction(computerAction);
    addToolBar(m_generalPurposeToolbar);

    // Create status bar.

    m_statusBar = new QStatusBar(this);
    setStatusBar(m_statusBar);

    readSettings();

    connect(m_filesDockWidget, SIGNAL(openFile(QString)), this, SLOT(handleOpenFileRequest(QString)));
    connect(m_historyDockWidget, SIGNAL(information(QString)), this, SLOT(reportStatusMessage(QString)));
    connect(m_historyDockWidget, SIGNAL(commandDoubleClicked(QString)), this, SLOT(handleCommandDoubleClicked(QString)));
    connect(m_variablesDockWidget, SIGNAL(saveWorkspace()), this, SLOT(handleSaveWorkspaceRequest()));
    connect(m_variablesDockWidget, SIGNAL(loadWorkspace()), this, SLOT(handleLoadWorkspaceRequest()));
    connect(m_variablesDockWidget, SIGNAL(clearWorkspace()), this, SLOT(handleClearWorkspaceRequest()));

    m_browserDockWidget->browserWidget()->load(QUrl("http://www.gnu.org/software/octave/doc/interpreter/"));
    m_serviceDockWidget->browserWidget()->load(QUrl("http://powerup.ath.cx/bugtracker"));

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
    m_octaveTerminalDockWidget->octaveTerminal()->openTeletype(fdm);
    reportStatusMessage(tr("Established link to Octave."));
}
