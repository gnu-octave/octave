#include "BrowserWidget.h"
#include <QVBoxLayout>
#include <QAction>
#include <QStyle>
#include <QApplication>

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
    connect(m_webView, SIGNAL(statusBarMessage(QString)), m_statusBar, SLOT(showMessage(QString)));
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

void BrowserWidget::load(QUrl url) {
    m_webView->load(url);
}
