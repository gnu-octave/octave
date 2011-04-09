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
#include "TerminalMdiSubWindow.h"


MainWindow::MainWindow(QWidget *parent)
    : QMainWindow(parent) {
    constructWindow();

    loadWebPage("Online Manual", "http://www.gnu.org/software/octave/doc/interpreter/");
    showMaximized();
}

MainWindow::~MainWindow() {
}

void MainWindow::addTerminalWindow() {
    TerminalMdiSubWindow *terminal = new TerminalMdiSubWindow(this);
    m_mdiArea->addSubWindow(terminal);
    terminal->show();
}

void MainWindow::loadWebPage(QString title, QString url) {

    QWebView *webView = new QWebView();
    webView->setWindowTitle(title);
    webView->load(QUrl(url));
    m_mdiArea->addSubWindow(webView);
}

void MainWindow::constructWindow() {
    m_mdiArea = new QMdiArea();
    setWindowTitle("Quint");
    setCentralWidget(m_mdiArea);

    QMenu *viewsMenu = menuBar()->addMenu("Views");
    QAction *addOctaveTerminalAction = viewsMenu->addAction("Add Octave Terminal");

    m_mdiArea->setViewMode(QMdiArea::SubWindowView);

    connect(addOctaveTerminalAction, SIGNAL(triggered()), this, SLOT(addTerminalWindow()));
}
