#include "mainwindow.h"
#include "octaveterminal.h"

#include <QMenuBar>
#include <QMenu>
#include <QAction>
#include <QtWebKit/QWebView>

MainWindow::MainWindow(QWidget *parent)
    : QMainWindow(parent) {
    showMaximized();
    m_mdiArea = new QMdiArea();
    setCentralWidget(m_mdiArea);

    addOctaveTerminal();
    loadWebPage("Online Manual", "http://www.gnu.org/software/octave/doc/interpreter/");
    m_mdiArea->setViewMode(QMdiArea::TabbedView);
}

MainWindow::~MainWindow() {

}

void MainWindow::addOctaveTerminal() {
    OctaveTerminal *octaveTerminal = new OctaveTerminal;
    m_mdiArea->addSubWindow(octaveTerminal);
    Client *octaveClient = ClientManager::clientManager().startProcess("octave");
    octaveTerminal->assignClient(octaveClient);
}

void MainWindow::loadWebPage(QString title, QString url) {

    QWebView *webView = new QWebView();
    webView->setWindowTitle(title);
    webView->load(QUrl(url));
    m_mdiArea->addSubWindow(webView);
}
