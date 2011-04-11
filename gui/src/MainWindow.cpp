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
#include <QtWebKit/QWebView>

#include "MainWindow.h"

MainWindow::MainWindow(QWidget *parent)
    : QMainWindow(parent),
      m_isRunning(true) {
    resize(1000, 600);
    constructWindow();
    establishOctaveLink();
}

MainWindow::~MainWindow() {
}

void MainWindow::constructWindow() {
    m_octaveTerminal = new OctaveTerminal(this);
    m_variablesDockWidget = new VariablesDockWidget(this);
    m_historyDockWidget = new HistoryDockWidget(this);
    setWindowTitle("Quint");
    setCentralWidget(m_octaveTerminal);

    addDockWidget(Qt::LeftDockWidgetArea, m_variablesDockWidget);
    addDockWidget(Qt::LeftDockWidgetArea, m_historyDockWidget);
}

void MainWindow::establishOctaveLink() {
    //QMetaObject::invokeMethod(this, "setStatus", Q_ARG(QString, QString("Establishing Octave link..")));
    m_octaveLink = new OctaveLink();

    m_octaveMainThread = new OctaveMainThread(this);
    m_octaveMainThread->start();

    m_octaveCallbackThread = new OctaveCallbackThread(this, this);
    m_octaveCallbackThread->start();

    command_editor::add_event_hook(server_rl_event_hook_function);

    int fdm, fds;
    if(openpty(&fdm, &fds, 0, 0, 0) < 0) {
        assert(0);
    }
    dup2 (fds, 0);
    dup2 (fds, 1);
    dup2 (fds, 2);
    m_octaveTerminal->openTeletype(fdm);
}
