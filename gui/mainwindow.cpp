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

#include "mainwindow.h"
#include "octaveterminal.h"
#include "terminal.h"

MainWindow::MainWindow(QWidget *parent)
    : QMainWindow(parent) {
    m_mdiArea = new QMdiArea();
    setCentralWidget(m_mdiArea);

    //addOctaveTerminal();
    loadWebPage("Online Manual", "http://www.gnu.org/software/octave/doc/interpreter/");
    addTerminalWindow();

    m_mdiArea->setViewMode(QMdiArea::SubWindowView);
    showMaximized();
}

MainWindow::~MainWindow() {

}

void MainWindow::addOctaveTerminal() {
    OctaveTerminal *octaveTerminal = new OctaveTerminal;
    m_mdiArea->addSubWindow(octaveTerminal);
    Client *octaveClient = ClientManager::clientManager().startProcess("octave --interactive --verbose");
    octaveTerminal->assignClient(octaveClient);
}

void MainWindow::addTerminalWindow() {
    Terminal *terminal = new Terminal;
    m_mdiArea->addSubWindow(terminal);
}

void MainWindow::loadWebPage(QString title, QString url) {

    QWebView *webView = new QWebView();
    webView->setWindowTitle(title);
    webView->load(QUrl(url));
    m_mdiArea->addSubWindow(webView);
}
