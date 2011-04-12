#ifndef BROWSERMDISUBWINDOW_H
#define BROWSERMDISUBWINDOW_H

#include <QWidget>
#include <QToolBar>
#include <QLineEdit>
#include <QtWebKit/QWebView>
#include <QStatusBar>

class BrowserWidget : public QWidget {
    Q_OBJECT
public:
    BrowserWidget(QWidget *parent = 0);
    void load(QUrl url);

public slots:
    void setUrl(QUrl url);
    void jumpToWebsite();

private:
    void construct();

    QLineEdit *m_urlLineEdit;
    QToolBar *m_navigationToolBar;
    QWebView *m_webView;
    QStatusBar *m_statusBar;
};

#endif // BROWSERMDISUBWINDOW_H
