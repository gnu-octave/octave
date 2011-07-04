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

#include "BrowserWidget.h"
#include <QVBoxLayout>
#include <QAction>
#include <QStyle>
#include <QApplication>

BrowserDockWidget::BrowserDockWidget(QWidget *parent, BrowserWidget *browserWidget)
    : QDockWidget(parent) {
    m_browserWidget = browserWidget;
    setWidget(m_browserWidget);
}

BrowserDockWidget::~BrowserDockWidget() {
}

BrowserWidget *BrowserDockWidget::browserWidget() {
    return m_browserWidget;
}

BrowserWidget::BrowserWidget(QWidget *parent)
    : QWidget(parent) {
    construct();
}

void BrowserWidget::construct() {
    QStyle *style = QApplication::style();
    m_navigationToolBar = new QToolBar(this);
    m_webView = new QWebView(this);
    m_urlLineEdit = new QLineEdit(this);
    m_statusBar = new QStatusBar(this);

    QAction *backAction = new QAction(style->standardIcon(QStyle::SP_ArrowLeft),
        "", m_navigationToolBar);
    QAction *forwardAction = new QAction(style->standardIcon(QStyle::SP_ArrowRight),
        "", m_navigationToolBar);

    m_navigationToolBar->addAction(backAction);
    m_navigationToolBar->addAction(forwardAction);
    m_navigationToolBar->addWidget(m_urlLineEdit);

    QVBoxLayout *layout = new QVBoxLayout();
    layout->addWidget(m_navigationToolBar);
    layout->addWidget(m_webView);
    layout->addWidget(m_statusBar);
    layout->setMargin(2);
    setLayout(layout);

    connect(backAction, SIGNAL(triggered()), m_webView, SLOT(back()));
    connect(forwardAction, SIGNAL(triggered()), m_webView, SLOT(forward()));
    connect(m_webView, SIGNAL(urlChanged(QUrl)), this, SLOT(setUrl(QUrl)));
    connect(m_urlLineEdit, SIGNAL(returnPressed()), this, SLOT(jumpToWebsite()));
    //connect(m_webView, SIGNAL(statusBarMessage(QString)), this, SLOT(showMessage(QString)));
}

void BrowserWidget::setUrl(QUrl url) {
    m_urlLineEdit->setText(url.toString());
}

void BrowserWidget::jumpToWebsite() {
    QString url = m_urlLineEdit->text();
    if(!url.startsWith("http://"))
        url = "http://" + url;
    load(url);
}

void BrowserWidget::showStatusMessage(QString message) {
    m_statusBar->showMessage(message, 1000);
}

void BrowserWidget::load(QUrl url) {
    m_webView->load(url);
}
